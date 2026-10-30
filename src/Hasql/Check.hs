{-# LANGUAGE CPP #-}

-- |
-- Module      : Hasql.Check
-- Copyright   : (c) 2022 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
module Hasql.Check where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import PostgresqlSyntax qualified as Ast
import Pqi qualified
import Prelude


-- | Turn the SQL text inside a quasiquote into the SQL the splice actually
-- sends to Postgres, mirroring @Hasql.TH.expPreparableStmtAstParser@ composed
-- with @Hasql.TH.Extraction.Exp.sql@ line for line. Rendering at 'mempty'
-- drops the nullability markers.
renderSql :: Text -> Either String ByteString
renderSql sql = renderStmt <$> parseStmt sql


-- | The statement in a quasiquote, as the splice would render it.
parseStmt :: Text -> Either String Ast.PreparableStmt
parseStmt sql = either (Left . Text.unpack) (Right . lowerAst) (Ast.parse quoterSettings sql)


-- | A statement in plain SQL, as Postgres reads it, whatever syntax this
-- build takes in hasql-th's quotes: for the quoters of
-- "Hasql.Check.Inferred.TH", and the annotated statements that stand for
-- theirs, which hasql-th's nullability markers may annotate.
--
-- Not lowered: Kronor's fork numbers the placeholders afresh as it lowers
-- its parameter fields, which would turn a lone @$2@ into @$1@.
parsePlainStmt :: Bool -> Text -> Either String Ast.PreparableStmt
parsePlainStmt markers sql = either (Left . Text.unpack) Right (Ast.parse (Ast.nullabilityMarkers markers) sql)


-- | A statement in whatever syntax this build's quotes take, left exactly as
-- parsed: the quoters of "Hasql.Check.Inferred.TH" read Kronor's Haskell
-- targets off it, and lower it themselves.
parseForkStmt :: Text -> Either String Ast.PreparableStmt
parseForkStmt sql = either (Left . Text.unpack) Right (Ast.parse quoterSettings sql)


renderStmt :: Ast.PreparableStmt -> ByteString
renderStmt = Text.encodeUtf8 . Ast.toText mempty

#ifdef KRONOR_SYNTAX

-- | The settings Kronor's fork of hasql-th parses with: upstream's nullability
-- markers, plus its Haskell targets and Haskell param fields.
quoterSettings :: Ast.Settings
quoterSettings =
    mconcat
        [ Ast.nullabilityMarkers True
        , Ast.haskellTargets True
        , Ast.haskellParamFields True
        ]


-- | 'Ast.lower' numbers the placeholders as they fall in the tree that will
-- actually be rendered - the one with the Haskell targets erased - but hands
-- back the tree that still carries them, because the quasiquoter builds its
-- row decoder by walking those targets. We have no decoder to build, so the
-- erasure the quasiquoter defers to 'Hasql.TH.Extraction.Exp' happens right
-- here.
lowerAst :: Ast.PreparableStmt -> Ast.PreparableStmt
lowerAst ast = Ast.eraseHaskellTargets (fst (Ast.lower ast))

#else

-- | The settings upstream hasql-th parses with.
quoterSettings :: Ast.Settings
quoterSettings = Ast.nullabilityMarkers True


-- | Upstream hasql-th renders the statement as parsed.
lowerAst :: Ast.PreparableStmt -> Ast.PreparableStmt
lowerAst = id

#endif


prepare :: Pqi.Connection -> Text -> Text -> IO (Either String ())
prepare conn stmtName unParsedStmt =
    either (pure . Left) (prepareStmt conn (Text.encodeUtf8 stmtName)) (parseStmt unParsedStmt)


-- | Prepare a statement under a name, returning the server's error if it fails.
prepareStmt :: Pqi.Connection -> ByteString -> Ast.PreparableStmt -> IO (Either String ())
prepareStmt conn stmtName stmt = do
    mr <- conn.prepare stmtName (renderStmt stmt) Nothing
    case mr of
        Nothing -> Left . maybe "Something went wrong" (Text.unpack . Text.decodeUtf8Lenient) <$> conn.errorMessage
        Just r -> do
            rs <- r.resultStatus
            if rs `elem` [Pqi.EmptyQuery, Pqi.BadResponse, Pqi.NonfatalError, Pqi.FatalError]
                then maybe (Left "Something went wrong") (Left . Text.unpack . Text.decodeUtf8Lenient) <$> r.resultErrorMessage
                else pure (Right ())
