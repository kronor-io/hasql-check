-- |
-- Module      : Hasql.Check.Inferred.Quote
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Quotes of "Hasql.Check.Inferred.TH", with the Haskell types GHC used them
-- at, as @hasql-check-hie@ writes them: one JSON object a line.
--
-- Each is checked as the hasql-th quote it stands for - the same SQL, with
-- the annotations the Haskell types give it - so that every check of an
-- annotated quote applies to it too.
module Hasql.Check.Inferred.Quote (
    Inferred (..),
    Column (..),
    readInferred,
    annotated,
    columns,
    checkSent,
) where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32)
import Hasql.Check (parsePlainStmt, quoterSettings)
import Hasql.Check.Annotate (annotate, typename)
import Hasql.Check.Inferred (Piece (..), Template (..), known, render)
import Hasql.Check.Inferred.TH (template)
import Hasql.Check.Nullability (query)
import PostgresqlSyntax qualified as Ast
import Pqi qualified
import Text.Read (readMaybe)
import Prelude


data Inferred = Inferred
    { file :: FilePath
    , line :: Int
    , column :: Int
    , quoter :: Text
    , sql :: Text
    , params :: [Column]
    , row :: Maybe [Column]
    -- ^ Nothing for a statement that returns no rows.
    }


-- | A parameter or result column: the qualified name of its Haskell type, and
-- whether it is a 'Maybe'.
data Column = Column
    { haskellType :: Text
    , nullable :: Bool
    }


instance Aeson.FromJSON Inferred where
    parseJSON = Aeson.withObject "quote" \o ->
        Inferred
            <$> o .: "file"
            <*> o .: "line"
            <*> o .: "column"
            <*> o .: "quoter"
            <*> o .: "sql"
            <*> o .: "params"
            <*> o .:? "row"


instance Aeson.FromJSON Column where
    parseJSON = Aeson.withObject "column" \o -> Column <$> o .: "type" <*> o .: "nullable"


-- | Quotes, one JSON object a line.
readInferred :: ByteString -> Either String [Inferred]
readInferred contents =
    traverse
        (\(n, l) -> either (\e -> Left ("line " <> show n <> ": " <> e)) Right (Aeson.eitherDecodeStrict l))
        [(n, l) | (n :: Int, l) <- zip [1 ..] (ByteString.Char8.lines contents), not (ByteString.Char8.null l)]


-- | The statement with its parameters annotated, as a hasql-th quote: its
-- result columns are cast around it, which the warnings see through
-- 'columns'.
annotated :: Inferred -> Either String Text
annotated q = do
    stmt <- parsePlainStmt False q.sql
    params <- traverse postgresType q.params
    let param n = case drop (n - 1) params of
            (name, nullable) : _ | n >= 1 -> Just (typename name nullable)
            _ -> Nothing
    pure (Ast.toText quoterSettings (annotate param stmt))


-- | The Postgres type each result column is read as, and whether it may be
-- NULL.
columns :: Inferred -> Maybe [(Text, Bool)]
columns q = either (const Nothing) Just . traverse postgresType =<< q.row


postgresType :: Column -> Either String (Text, Bool)
postgresType c = case lookup c.haskellType known of
    Just name -> Right (name, c.nullable)
    Nothing ->
        Left
            ( "no Postgres type is known for "
                <> Text.unpack c.haskellType
                <> "; the checker knows "
                <> Text.unpack (Text.intercalate ", " (map fst known))
            )


-- | Whether the statement, as the quoter sends it, prepares; whether it
-- takes as many parameters as the Haskell type sends, and returns as many
-- columns as it reads. Prepared, as hasql prepares it, with the parameters'
-- types, which Postgres could not tell for a parameter the statement does
-- not use.
checkSent :: Pqi.Connection -> Inferred -> IO [String]
checkSent conn q = case (,,) <$> template q.sql <*> traverse postgresType q.params <*> traverse (traverse postgresType) q.row of
    Left err -> pure [err]
    Right (t, params, row) -> do
        oids <- traverse (oid . fst) params
        case sequence oids of
            Nothing -> pure ["a parameter's type does not exist: " <> unwords [Text.unpack name | ((name, _), Nothing) <- zip params oids]]
            Just paramOids -> do
                shape <- described (render (map fst params) [] t) paramOids
                case shape of
                    Left err -> pure [err]
                    Right found -> do
                        -- From the statement itself: prepared with the
                        -- types hasql sends, it takes them all, used or not.
                        let used = [n | ParamType n <- t.whole]
                            sends = length q.params
                            paramErrors =
                                [ concat ["the Haskell type sends parameter $", show n, ", which the statement does not use"]
                                | n <- [1 .. sends]
                                , n `notElem` used
                                ]
                                    <> [ "the statement uses $" <> show (maximum used) <> ", but the Haskell type sends " <> count "parameter" sends
                                       | not (null used)
                                       , maximum used > sends
                                       ]
                        columnErrors <- case (row, t.wrapped) of
                            (Nothing, _) -> pure []
                            (Just readAs, _)
                                | length found /= length readAs ->
                                    pure ["the statement returns " <> count "column" (length found) <> ", but the Haskell type reads " <> show (length readAs)]
                            -- A CALL cannot be wrapped, so its columns are not
                            -- cast: they must already be of the types read.
                            (Just readAs, Nothing) -> do
                                expected <- traverse (oid . fst) readAs
                                sequence
                                    [ (\pg -> "column " <> show i <> " (" <> name <> ") is " <> pg <> ", but is read as " <> Text.unpack c.haskellType) <$> nameOf found'
                                    | (i :: Int, c, (name, found'), Just e) <- zip4 [1 ..] (concat q.row) found expected
                                    , found' /= e
                                    ]
                            (Just _, Just _) -> pure []
                        -- What is sent, wrapped in its casts.
                        sent <-
                            if null (paramErrors <> columnErrors)
                                then either pure (const []) <$> described (render (map fst params) (maybe [] (map fst) row) t) paramOids
                                else pure []
                        pure (paramErrors <> columnErrors <> sent)
  where
    described sql paramOids = do
        prepared <- conn.prepare "" (Text.encodeUtf8 sql) (Just paramOids)
        case prepared of
            Nothing -> Left . maybe "Something went wrong" (Text.unpack . Text.decodeUtf8Lenient) <$> conn.errorMessage
            Just result -> do
                status <- result.resultStatus
                if status /= Pqi.CommandOk
                    then Left . maybe "Something went wrong" (Text.unpack . Text.decodeUtf8Lenient) <$> result.resultErrorMessage
                    else
                        conn.describePrepared "" >>= \case
                            Nothing -> Left . maybe "Something went wrong" (Text.unpack . Text.decodeUtf8Lenient) <$> conn.errorMessage
                            Just d -> do
                                n <- d.nfields
                                found <- traverse (\i -> (,) <$> (maybe "?column?" (Text.unpack . Text.decodeUtf8Lenient) <$> d.fname i) <*> d.ftype i) [0 .. n - 1]
                                pure (Right found)
    count :: String -> Int -> String
    count what 1 = "1 " <> what
    count what k = unwords [show k, what <> "s"]
    oid :: Text -> IO (Maybe Word32)
    oid name =
        query conn (mconcat ["SELECT to_regtype(", quoteLiteral name, ")::oid::text"]) >>= \case
            Right [[Just o]] -> pure (readMaybe (ByteString.Char8.unpack o))
            _ -> pure Nothing
    nameOf :: Word32 -> IO String
    nameOf o =
        query conn (mconcat ["SELECT ", ByteString.Char8.pack (show o), "::regtype::text"]) >>= \case
            Right [[Just name]] -> pure (Text.unpack (Text.decodeUtf8Lenient name))
            _ -> pure ("type " <> show o)
    quoteLiteral name = mconcat ["'", Text.encodeUtf8 (Text.replace "'" "''" name), "'"]


zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []
