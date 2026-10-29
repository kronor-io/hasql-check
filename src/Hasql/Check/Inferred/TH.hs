{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Hasql.Check.Inferred.TH
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Quoters for SQL without annotations: the Haskell types a statement is used
-- with decide its parameter and result types.
--
-- > profile :: Statement Int64 (Maybe (Text, Maybe Text))
-- > profile = [maybeStatement| select email, nickname from customer where id = $1 |]
--
-- sends
--
-- > WITH hasql_check_result (c1, c2) AS (SELECT email, nickname FROM customer WHERE id = $1 :: int8)
-- > SELECT c1 :: text, c2 :: text FROM hasql_check_result
--
-- GHC can only tell whether those types fit the schema with a database; see
-- the README for checking them with @hasql-check --inferred@.
module Hasql.Check.Inferred.TH (
    singletonStatement,
    maybeStatement,
    vectorStatement,
    resultlessStatement,
    template,
) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Check (parsePlainStmt, renderStmt)
import Hasql.Check.Annotate (annotate, placeholder)
import Hasql.Check.Inferred (Piece (..), Template (..), Wrapped (..), resultless, statement)
import Hasql.Decoders qualified as D
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import PostgresqlSyntax qualified as Ast
import Text.Read (readMaybe)
import Prelude


quoter :: (Q Exp -> Q Exp) -> QuasiQuoter
quoter make =
    QuasiQuoter
        { quoteExp = either fail (make . lift) . template . Text.pack
        , quotePat = const (fail "Not supported in patterns")
        , quoteType = const (fail "Not supported in types")
        , quoteDec = const (fail "Not supported in declarations")
        }


singletonStatement, maybeStatement, vectorStatement, resultlessStatement :: QuasiQuoter
singletonStatement = quoter \t -> [|statement $t D.singleRow|]
maybeStatement = quoter \t -> [|statement $t D.rowMaybe|]
vectorStatement = quoter \t -> [|statement $t D.rowVector|]
resultlessStatement = quoter \t -> [|resultless $t|]


-- | The statement, rendered as hasql-th would render it, cut where its
-- parameters' annotations go, and split where its result columns' cast
-- goes around it.
template :: Text -> Either String Template
template sql = do
    stmt <- parsePlainStmt False sql
    -- Names the statement does not already contain, in a string literal or
    -- as a table, say.
    let plain = Text.toLower (Text.decodeUtf8Lenient (renderStmt stmt))
        unused base = until (not . (`Text.isInfixOf` plain)) (<> "_") base
        prefix = unused "hasql_check_slot_"
        annotated = annotate (\n -> Just (placeholder (prefix <> Text.pack (show n)))) stmt
        cut = cutWith prefix
    pure
        Template
            { whole = cut (Text.decodeUtf8Lenient (renderStmt annotated))
            , wrapped =
                ( \(with, body) ->
                    Wrapped
                        { with = cut . Ast.toText mempty <$> with
                        , body = cut (Text.decodeUtf8Lenient (renderStmt body))
                        , name = unused "hasql_check_result"
                        }
                )
                    <$> splitWith annotated
            }


-- | The statement's own WITH list, and the rest of it.
splitWith :: Ast.PreparableStmt -> Maybe (Maybe Ast.WithClause, Ast.PreparableStmt)
splitWith = \case
    -- "(WITH ... SELECT ...)" is a parenthesised select clause.
    Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens Nothing (Ast.WithParensSelectClause p) s l f)) ->
        let (w, p') = inParens p
         in Just (w, Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens Nothing (Ast.WithParensSelectClause p') s l f)))
    Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens w c s l f)) ->
        Just (w, Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens Nothing c s l f)))
    Ast.SelectPreparableStmt (Ast.WithParensSelectStmt s) -> Just (Ast.SelectPreparableStmt . Ast.WithParensSelectStmt <$> inParens s)
    Ast.InsertPreparableStmt (Ast.InsertStmt w a b c d) -> Just (w, Ast.InsertPreparableStmt (Ast.InsertStmt Nothing a b c d))
    Ast.UpdatePreparableStmt (Ast.UpdateStmt w a b c d e) -> Just (w, Ast.UpdatePreparableStmt (Ast.UpdateStmt Nothing a b c d e))
    Ast.DeletePreparableStmt (Ast.DeleteStmt w a b c d) -> Just (w, Ast.DeletePreparableStmt (Ast.DeleteStmt Nothing a b c d))
    Ast.CallPreparableStmt _ -> Nothing


-- | A parenthesised SELECT's WITH list, and the rest of it: the list can go
-- outside the parentheses, and must, if it has a data-modifying statement.
inParens :: Ast.SelectWithParens -> (Maybe Ast.WithClause, Ast.SelectWithParens)
inParens = \case
    Ast.NoParensSelectWithParens (Ast.SelectNoParens w c s l f) -> (w, Ast.NoParensSelectWithParens (Ast.SelectNoParens Nothing c s l f))
    Ast.WithParensSelectWithParens s -> Ast.WithParensSelectWithParens <$> inParens s


cutWith :: Text -> Text -> [Piece]
cutWith prefix = cut
  where
    cut t = case Text.breakOn (" :: " <> prefix) t of
        (before, "") -> [Sql before | not (Text.null before)]
        (before, rest) ->
            let (slot, after) = Text.span isAlphaNum (Text.drop (Text.length (" :: " <> prefix)) rest)
             in [Sql before | not (Text.null before)] <> piece slot <> cut after

    piece slot = case readMaybe (Text.unpack slot) of
        Just i -> [ParamType i]
        Nothing -> [Sql (" :: " <> prefix <> slot)]
