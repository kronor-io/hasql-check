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
renderSql sql = do
    ast <- either (Left . Text.unpack) Right (Ast.parse quoterSettings sql :: Either Text Ast.PreparableStmt)
    pure (Text.encodeUtf8 (Ast.toText mempty (lowerAst ast)))

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
prepare conn stmtName unParsedStmt = do
    case renderSql unParsedStmt of
        Left err -> pure $ Left err
        Right stmt -> do
            mr <- conn.prepare (Text.encodeUtf8 stmtName) stmt Nothing
            case mr of
                Nothing -> pure (Left "Something went wrong")
                Just r -> do
                    rs <- r.resultStatus
                    if rs `elem` [Pqi.EmptyQuery, Pqi.BadResponse, Pqi.NonfatalError, Pqi.FatalError]
                        then maybe (Left "Something went wrong") (Left . Text.unpack . Text.decodeUtf8Lenient) <$> conn.errorMessage
                        else pure (Right ())
