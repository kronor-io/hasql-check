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
-- sends @select (email)::text, (nickname)::text from customer where id = $1::int8@.
-- GHC can only tell whether those types fit the schema with a database; see
-- the README for checking them with @hasql-check --inferred@.
module Hasql.Check.Inferred.TH (
    singletonStatement,
    maybeStatement,
    vectorStatement,
    resultlessStatement,
    pieces,
) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Check (parseStmt, renderStmt)
import Hasql.Check.Annotate (Slot (..), annotate, placeholder)
import Hasql.Check.Inferred (Piece (..), resultless, statement)
import Hasql.Decoders qualified as D
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import Text.Read (readMaybe)
import Prelude


quoter :: (Q Exp -> Q Exp) -> QuasiQuoter
quoter make =
    QuasiQuoter
        { quoteExp = either fail (make . lift) . pieces . Text.pack
        , quotePat = const (fail "Not supported in patterns")
        , quoteType = const (fail "Not supported in types")
        , quoteDec = const (fail "Not supported in declarations")
        }


singletonStatement, maybeStatement, vectorStatement, resultlessStatement :: QuasiQuoter
singletonStatement = quoter \p -> [|statement $p D.singleRow|]
maybeStatement = quoter \p -> [|statement $p D.rowMaybe|]
vectorStatement = quoter \p -> [|statement $p D.rowVector|]
resultlessStatement = quoter \p -> [|resultless $p|]


-- | The statement, rendered as hasql-th would render it, cut where the
-- annotations go.
pieces :: Text -> Either String [Piece]
pieces sql = do
    stmt <- parseStmt sql
    -- A marker the statement does not already contain, in a string literal
    -- say, so that only the annotations are cut out.
    let plain = Text.decodeUtf8Lenient (renderStmt stmt)
        prefix = until (not . (`Text.isInfixOf` plain)) (<> "x_") "hasql_check_slot_"
    pure (cutWith prefix (Text.decodeUtf8Lenient (renderStmt (annotate (Just . placeholder . marker prefix) stmt))))
  where
    marker prefix = \case
        Param n -> prefix <> "p" <> Text.pack (show n)
        Column n -> prefix <> "c" <> Text.pack (show n)


cutWith :: Text -> Text -> [Piece]
cutWith prefix = cut
  where
    cut t = case Text.breakOn (" :: " <> prefix) t of
        (before, "") -> [Sql before | not (Text.null before)]
        (before, rest) ->
            let (slot, after) = Text.span isAlphaNum (Text.drop (Text.length (" :: " <> prefix)) rest)
             in [Sql before | not (Text.null before)] <> piece slot <> cut after

    piece slot = case Text.uncons slot of
        Just ('p', n) | Just i <- readMaybe (Text.unpack n) -> [ParamType i]
        Just ('c', n) | Just i <- readMaybe (Text.unpack n) -> [ColumnType i]
        _ -> [Sql (" :: " <> prefix <> slot)]
