-- |
-- Module      : Main
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Print, for every quote of "Hasql.Check.Inferred.TH" in the given .hie
-- files, the types GHC instantiated its statement at, one JSON object a line:
--
-- > {"file":"src/Shop.hs","line":14,"column":11,"quoter":"maybeStatement",
-- >  "sql":"...","params":[{"type":"GHC.Internal.Int.Int64","nullable":false}],
-- >  "row":[...]}
--
-- for @hasql-check --inferred@. The quote's own span holds the nodes of the
-- code it generated, typed where it is used, even when the statement is an
-- argument with no signature of its own. @row@ is null for a statement
-- returning no rows.
--
-- The .hie format changes with each GHC release: build this with the GHC the
-- project is built with.
module Main where

import Control.Monad (unless)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Data.FastString (getLexicalFastString, unpackFS)
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils (recoverFullType)
import GHC.Iface.Type (IfaceTyCon (..), IfaceTyLit (..))
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import GHC.Unit.Module (moduleName, moduleNameString)
import Source (Quote (..), Ty (..), Types (..), columnTypes, ground, quoteAt, typesOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)


main :: IO ()
main = do
    files <- getArgs
    nc <- initNameCache 'z' []
    errors <- concat <$> mapM (dump nc) files
    mapM_ (hPutStrLn stderr) errors
    unless (null errors) exitFailure
  where
    dump nc path = do
        result <- readHieFile nc path
        let hf = hie_file_result result
            toTy = convert . flip recoverFullType (hie_types hf)
            found = Map.fromListWith (flip (<>)) [(s, [t]) | (s, t) <- concatMap (collect toTy) (Map.elems (getAsts (hie_asts hf)))]
            source = lines (Text.unpack (Text.decodeUtf8Lenient (hie_hs_src hf)))
        concat
            <$> mapM
                ( \(s, here) ->
                    let params = [p | Con "Hasql.Check.Inferred" "Params" [p] <- here]
                        rows = [r | Con "Hasql.Check.Inferred" "Row" [r] <- here]
                        paramNames = [ns | Con "Hasql.Check.Inferred" "PostgresParams" [l] <- here, Just ns <- [symbols l]]
                        columnNames = [ns | Con "Hasql.Check.Inferred" "PostgresColumns" [l] <- here, Just ns <- [symbols l]]
                        paramLabels = [ls | Con "Hasql.Check.Inferred" "PostgresParamLabels" [l] <- here, Just ls <- [labelLists l]]
                        columnLabels = [ls | Con "Hasql.Check.Inferred" "PostgresColumnLabels" [l] <- here, Just ls <- [labelLists l]]
                        -- The enclosing declarations hold the same
                        -- dictionaries, but only the quote's own span starts
                        -- after "[quoter|".
                        at = quoteAt source (srcSpanStartLine s, srcSpanStartCol s) (srcSpanEndLine s, srcSpanEndCol s)
                        location q = hie_hs_file hf <> ":" <> show (srcSpanStartLine s) <> ":" <> show q.column
                     in case (at, typesOf ground params rows) of
                            (Just _, NotInferred) -> pure []
                            (Just q, Resolved p r)
                                | Just names <- listToMaybe paramNames
                                , Just labels <- listToMaybe paramLabels
                                , Just row <- traverse (\r' -> (r',,) <$> listToMaybe columnNames <*> listToMaybe columnLabels) r ->
                                    [] <$ putStrLn (output (hie_hs_file hf) (srcSpanStartLine s) q (p, names, labels) row)
                            (Just q, _) -> pure [unresolved (location q)]
                            (Nothing, _) -> pure []
                )
                (Map.toList found)

    unresolved location =
        location
            <> ": the statement's types are not known where it is written, so it cannot be checked;"
            <> " give its binding a concrete type, such as Statement Int64 Text"

    collect toTy node =
        -- The dictionaries the generated code passes are identifiers of the
        -- quote's node, typed by the instance GHC chose.
        let infos = Map.elems (getSourcedNodeInfo (sourcedNodeInfo node))
            here =
                [(nodeSpan node, toTy t) | info <- infos, t <- nodeType info]
                    <> [(nodeSpan node, toTy t) | info <- infos, (Right _, details) <- Map.toList (nodeIdentifiers info), Just t <- [identType details]]
         in here <> concatMap (collect toTy) (nodeChildren node)


-- | One quote's line of output.
output :: FilePath -> Int -> Quote -> (Ty, [String], [[String]]) -> Maybe (Ty, [String], [[String]]) -> String
output file line q params row =
    object
        [ ("file", string file)
        , ("line", show line)
        , ("column", show q.column)
        , ("quoter", string q.quoter)
        , ("sql", string q.body)
        , ("params", columns True params)
        , ("row", maybe "null" (columns False) row)
        ]


-- | A parameter or row type, as the list of its columns, with the Postgres
-- type of each, and the labels of each that is an enum.
columns :: Bool -> (Ty, [String], [[String]]) -> String
columns isParams (t, names, labels) = list (zipWith3 (\(c, nullable) postgres ls -> one c postgres ls nullable) (columnTypes isParams t) names labels)
  where
    one c postgres ls nullable =
        object
            [ ("type", string (qualified c))
            , ("postgres", string postgres)
            , ("labels", list (map string ls))
            , ("nullable", if nullable then "true" else "false")
            ]
    qualified = \case
        Con m n args -> unwords ((if null m then n else m <> "." <> n) : map (parenthesised . qualified) args)
        Lit s -> show s
        OtherLit s -> s
        Var -> "?"
    list xs = "[" <> intercalate "," xs <> "]"
    parenthesised name = if ' ' `elem` name then "(" <> name <> ")" else name


-- | A type-level list of lists of strings.
labelLists :: Ty -> Maybe [[String]]
labelLists = \case
    Con _ ":" [l, rest] -> (:) <$> symbols l <*> labelLists rest
    Con _ "[]" [] -> Just []
    _ -> Nothing


-- | A type-level list of strings.
symbols :: Ty -> Maybe [String]
symbols = \case
    Con _ ":" [Lit s, rest] -> (s :) <$> symbols rest
    Con _ "[]" [] -> Just []
    _ -> Nothing


convert :: HieTypeFix -> Ty
convert (Roll t) = case t of
    HTyConApp tc (HieArgs args) -> uncurry Con (names (ifaceTyConName tc)) [convert a | (True, a) <- args]
    HQualTy _ b -> convert b
    HForAllTy _ b -> convert b
    HCastTy a -> convert a
    HLitTy (IfaceStrTyLit s) -> Lit (unpackFS (getLexicalFastString s))
    HLitTy (IfaceNumTyLit n) -> OtherLit (show n)
    HLitTy (IfaceCharTyLit c) -> OtherLit (show c)
    _ -> Var
  where
    names :: Name -> (String, String)
    names n = (maybe "" (moduleNameString . moduleName) (nameModule_maybe n), occNameString (nameOccName n))


object :: [(String, String)] -> String
object fields = "{" <> intercalate "," [string k <> ":" <> v | (k, v) <- fields] <> "}"


string :: String -> String
string s = "\"" <> concatMap escape s <> "\""
  where
    escape = \case
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\t' -> "\\t"
        c | ord c < 0x20 -> printf "\\u%04x" (ord c)
        c -> [c]
