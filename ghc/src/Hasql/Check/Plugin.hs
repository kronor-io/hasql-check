-- |
-- Module      : Hasql.Check.Plugin
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- A GHC plugin that checks the statements of "Hasql.Check.Inferred.TH"
-- against a database while their module compiles, as "Hasql.Check.TH" does
-- for hasql-th's: once GHC has type-checked the module, the types each
-- statement is used at are known, and the plugin checks it as
-- @hasql-check --inferred@ would. A statement that does not fit the schema
-- is a compile error at its quote.
--
-- > {-# OPTIONS_GHC -fplugin=Hasql.Check.Plugin #-}
--
-- or @ghc-options: -fplugin=Hasql.Check.Plugin@ for a whole package, which
-- depends on @hasql-check-ghc@. It is configured as "Hasql.Check.TH" is, by
-- @HASQL_CHECK_DATABASE_URL@ and the rest (see "Hasql.Check.Compile"); with
-- no database set, it checks nothing.
module Hasql.Check.Plugin (plugin) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Data, cast, gmapQ)
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import GHC.Core.TyCo.Rep (Type)
import GHC.Core.TyCon (tyConName)
import GHC.Core.Type (filterOutInvisibleTypes, isCharLitTy, isNumLitTy, isStrLitTy, splitTyConApp_maybe)
import GHC.Core.Utils (exprType)
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Plugins (Plugin (..), PluginRecompile (..), defaultPlugin, keepRenamedSource)
import GHC.Hs
import GHC.Tc.Errors.Types (TcRnMessage (..))
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Tc.Types.Evidence (EvTerm (..), HsWrapper (..))
import GHC.Tc.Utils.Monad (addErrAt, updTcRef)
import GHC.Types.Error (mkPlainError, mkSimpleUnknownDiagnostic, noHints)
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Types.SrcLoc
import GHC.Types.Var (varName)
import GHC.Unit.Module (moduleName)
import GHC.Utils.Fingerprint (fingerprintString)
import GHC.Utils.Outputable (empty, nest, text, vcat)
import Hasql.Check.Compile (Config (..), Session (..), readConfig, reportUnavailable, warning, warnings, withSession)
import Hasql.Check.Finding (Rule (..))
import Hasql.Check.Ghc.Source (Column (..), Quote (..), Statement (..), Ty (..), statementOf)
import Hasql.Check.Inferred.Quote (Inferred (Inferred), checkInferred)
import Hasql.Check.Inferred.Quote qualified as Quote


plugin :: Plugin
plugin =
    defaultPlugin
        { -- The quotes as GHC read them, after CPP and from an editor's buffer.
          renamedResultAction = keepRenamedSource
        , typeCheckResultAction = \_ _ env -> env <$ check env
        , pluginRecompile = \_ -> recompile <$> readConfig
        }


-- | A module is checked again when what it was checked with changes, as a
-- different database; a change of schema rechecks only the modules with
-- statements, which depend on the schema file.
recompile :: Config -> PluginRecompile
recompile config =
    MaybeRecompile (fingerprintString (show (config.databaseUrl, config.schemaFile, config.nullability, config.types, config.baseline)))


check :: TcGblEnv -> TcM ()
check env = do
    config <- liftIO readConfig
    forM_ config.databaseUrl \url -> do
        -- A module with a statement depends on the schema, so that a change
        -- of schema rechecks it, as for "Hasql.Check.TH"'s quoters, and on
        -- the baseline its warnings are settled against.
        let found = statements (tcg_binds env)
            quotes = quasiQuotes (tcg_rn_decls env)
        unless (null found) (forM_ (catMaybes [config.schemaFile, config.baseline]) \file -> updTcRef (tcg_dependent_files env) (file :))
        forM_ found \(s, passed) ->
            forM_ (quoteAround quotes s) \(line, q) ->
                case statementOf line q (map fromType passed) of
                    Nothing -> pure ()
                    Just (Left err) -> failAt s err
                    Just (Right statement) -> checkStatement config url s statement


checkStatement :: Config -> String -> RealSrcSpan -> Statement -> TcM ()
checkStatement config url s statement = do
    let file = unpackFS (srcSpanFile s)
        inferred = toInferred file statement
        rules = [Nullable | config.nullability, Just _ <- [statement.row]] <> [Narrowing | config.types]
    outcome <- liftIO $ withSession config url (not (null rules)) \session ->
        checkInferred session.connection (snd <$> session.catalogue) (Nullable `elem` rules) (Narrowing `elem` rules) inferred
    case outcome of
        Left why -> liftIO (reportUnavailable why)
        Right (Left err) -> failAt s ("hasql-check: the statement does not fit HASQL_CHECK_DATABASE_URL's schema:\n" <> err)
        Right (Right (found, fingerprint)) -> liftIO do
            messages <- warnings config rules file statement.line fingerprint found
            forM_ messages (warning file (statement.line, statement.quote.column))


-- | The statement as @hasql-check --inferred@ reads it.
toInferred :: FilePath -> Statement -> Inferred
toInferred file statement =
    Inferred
        file
        statement.line
        statement.quote.column
        (Text.pack statement.quote.quoter)
        (Text.pack statement.quote.body)
        (map column statement.params)
        (map column <$> statement.row)
  where
    column c = Quote.Column (Text.pack c.haskellType) (Text.pack c.postgres) (map Text.pack c.labels) c.nullable


failAt :: RealSrcSpan -> String -> TcM ()
failAt s message =
    addErrAt (RealSrcSpan s mempty) (TcRnUnknownMessage (mkSimpleUnknownDiagnostic (mkPlainError noHints layout)))
  where
    -- GHC's layout: the first line, and the rest under it.
    layout = case filter (not . null) (lines message) of
        first : rest -> vcat [text first, nest 2 (vcat (map text rest))]
        [] -> empty


-- * The statements in a type-checked module


-- | The quasi-quotes of a renamed module, with their spans and the line each
-- starts on.
quasiQuotes :: Data a => a -> [(RealSrcSpan, Quote)]
quasiQuotes x = here <> concat (gmapQ quasiQuotes x)
  where
    here = case cast x of
        Just (L l (HsUntypedSplice _ (HsQuasiQuote _ quoter (L _ body))) :: LHsExpr GhcRn)
            | RealSrcSpan s _ <- locA l ->
                [(s, Quote (srcSpanStartCol s) (getOccString quoter) (unpackFS body))]
        _ -> []


-- | The innermost quote whose span holds the given span - that of the code it
-- was spliced into - with the line it starts on.
quoteAround :: [(RealSrcSpan, Quote)] -> RealSrcSpan -> Maybe (Int, Quote)
quoteAround quotes s =
    case [(srcSpanStartLine o, q) | (o, q) <- around, all (\(o', _) -> o' `containsSpan` o) around] of
        found : _ -> Just found
        [] -> Nothing
  where
    around = filter (\(o, _) -> o `containsSpan` s) quotes


-- | Each use of 'Hasql.Check.Inferred.statement' or
-- 'Hasql.Check.Inferred.resultless' - each quote - with its span and the
-- types of the dictionaries it is passed: GHC has worked out the types it is
-- used at.
statements :: Data a => a -> [(RealSrcSpan, [Type])]
statements x = here <> concat (gmapQ statements x)
  where
    here = case cast x of
        Just (L l (XExpr (WrapExpr w (HsVar _ (L _ v)))) :: LHsExpr GhcTc)
            | getOccString v `elem` ["statement", "resultless"]
            , Just m <- nameModule_maybe (varName v)
            , moduleNameString (moduleName m) == "Hasql.Check.Inferred"
            , RealSrcSpan s _ <- locA l ->
                [(s, dictionaries w)]
        _ -> []


dictionaries :: HsWrapper -> [Type]
dictionaries = \case
    WpCompose a b -> dictionaries a <> dictionaries b
    WpEvApp (EvExpr e) -> [exprType e]
    _ -> []


-- | A type, as "Hasql.Check.Ghc.Source" reads it.
fromType :: Type -> Ty
fromType t
    | Just s <- isStrLitTy t = Lit (unpackFS s)
    | Just n <- isNumLitTy t = OtherLit (show n)
    | Just c <- isCharLitTy t = OtherLit (show c)
    | Just (tc, args) <- splitTyConApp_maybe t =
        let name = tyConName tc
         in Con
                (maybe "" (moduleNameString . moduleName) (nameModule_maybe name))
                (getOccString name)
                (map fromType (filterOutInvisibleTypes tc args))
    | otherwise = Var
