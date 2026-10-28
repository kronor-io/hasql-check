-- |
-- Module      : Hasql.Check.Quoter
-- Copyright   : (c) 2022 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
module Hasql.Check.Quoter where

import Data.Text (Text)
import Data.Text qualified as Text
import Prelude


-- | The hasql-th quoters whose SQL is a single preparable statement.
-- @uncheckedSql@ is deliberately absent: hasql-th neither parses nor prepares
-- it, because it is meant for scripts -- DDL, several statements -- that
-- PREPARE rejects.
quotersSupported :: [Text]
quotersSupported =
    [ "singletonStatement"
    , "maybeStatement"
    , "vectorStatement"
    , "foldStatement"
    , "resultlessStatement"
    , "rowsAffectedStatement"
    ]


-- | The quoter's name without its module qualifier: @Hasql.TH.vectorStatement@
-- becomes @vectorStatement@.
unqualified :: Text -> Text
unqualified = Text.takeWhileEnd (/= '.')


-- | Whether a quoter, qualified or not, is one of the given names.
isQuoterIn :: [Text] -> Text -> Bool
isQuoterIn names quoter = unqualified quoter `elem` names
