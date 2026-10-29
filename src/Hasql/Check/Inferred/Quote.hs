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
    sent,
    columns,
    checkSent,
) where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32)
import Hasql.Check (parsePlainStmt, quoterSettings)
import Hasql.Check.Annotate (annotate, typename, untypedLiterals)
import Hasql.Check.Inferred (Piece (..), Template (..), render)
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


-- | A parameter or result column: the qualified name of its Haskell type,
-- its Postgres type, and whether it is a 'Maybe'.
data Column = Column
    { haskellType :: Text
    , postgres :: Text
    -- ^ As GHC worked it out from the Haskell type's 'Hasql.Check.Inferred.PgType'.
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
    parseJSON = Aeson.withObject "column" \o -> Column <$> o .: "type" <*> o .: "postgres" <*> o .: "nullable"


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
    params <- traverse (\c -> typename c.postgres c.nullable) q.params
    let param n = case drop (n - 1) params of
            t : _ | n >= 1 -> Just t
            _ -> Nothing
    pure (Ast.toText quoterSettings (annotate param stmt))


-- | The statement as the quoter sends it, casts and all.
sent :: Inferred -> Either String Text
sent q = do
    t <- template q.sql
    params <- traverse postgresType q.params
    row <- traverse (traverse postgresType) q.row
    pure (render (map fst params) (maybe [] (map fst) row) t)


-- | The Postgres type each result column is read as, and whether it may be
-- NULL.
columns :: Inferred -> Maybe [(Text, Bool)]
columns q = either (const Nothing) Just . traverse postgresType =<< q.row


postgresType :: Column -> Either String (Text, Bool)
postgresType c = Right (c.postgres, c.nullable)


-- | Whether the statement, as the quoter sends it, prepares; whether it
-- takes as many parameters as the Haskell type sends, and returns as many
-- columns as it reads, each of a type it can be read as. Prepared, as hasql
-- prepares it, with the parameters' types, which Postgres could not tell for
-- a parameter the statement does not use.
--
-- A result column is cast to the type it is read as, and Postgres casts
-- anything it can convert, even text to a number, which fails at runtime for
-- text that is not one. So the column must have that type already, or one
-- Postgres converts to it without being told to: implicitly, or on
-- assignment.
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
                            (Just readAs, wrapped) -> do
                                expected <- traverse (oid . fst) readAs
                                let literals = either (const []) untypedLiterals (parsePlainStmt False q.sql)
                                    missing = [Text.unpack name | ((name, _), Nothing) <- zip readAs expected]
                                if not (null missing)
                                    then pure ["no such Postgres type: " <> unwords missing]
                                    else
                                        fmap concat . sequence $
                                            [ case wrapped of
                                                -- A CALL cannot be wrapped, so its
                                                -- columns are not cast: they must
                                                -- already be of the types read, or
                                                -- domains over them.
                                                Nothing -> do
                                                    same <- sameBase found' e
                                                    if same then pure [] else (\pg -> ["column " <> show i <> " (" <> name <> ") is " <> pg <> ", but is read as " <> Text.unpack c.haskellType]) <$> nameOf found'
                                                Just _ -> case lookup i literals of
                                                    Just literal -> literalError i name c literal
                                                    Nothing -> conversionError i name c found' e
                                            | (i :: Int, c, (name, found'), Just e) <- zip4 [1 ..] (concat q.row) found expected
                                            , found' /= e
                                            ]
                        -- What is sent, wrapped in its casts.
                        sentErrors <-
                            if null (paramErrors <> columnErrors)
                                then either pure (const []) <$> described (render (map fst params) (maybe [] (map fst) row) t) paramOids
                                else pure []
                        pure (paramErrors <> columnErrors <> sentErrors)
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
    -- A literal is converted once and for all: try it.
    literalError i name c literal =
        query conn (Text.encodeUtf8 (mconcat ["SELECT (", literal, ") :: ", c.postgres])) <&> \case
            Right _ -> []
            Left err -> [concat ["column ", show i, " (", name, ") is ", Text.unpack literal, ", which cannot be read as ", Text.unpack c.haskellType, ": ", err]]
    conversionError i name c from to = do
        converts <- convertsWithoutBeingTold from to
        fromName <- nameOf from
        toName <- nameOf to
        pure
            [ concat
                [ "column "
                , show i
                , " ("
                , name
                , ") is "
                , fromName
                , ", but is read as "
                , Text.unpack c.haskellType
                , " ("
                , toName
                , "), which Postgres converts it to only when told to, and can fail to at runtime"
                ]
            | not converts
            ]
    -- Through their base types, so that a domain converts as the type it is
    -- over; and an array as its elements, as Postgres converts arrays.
    convertsWithoutBeingTold :: Word32 -> Word32 -> IO Bool
    convertsWithoutBeingTold from to =
        query
            conn
            ( mconcat
                [ "SELECT f.typelem::text, t.typelem::text FROM pg_catalog.pg_type f, pg_catalog.pg_type t"
                , " WHERE f.oid = "
                , base from
                , " AND t.oid = "
                , base to
                , " AND f.typcategory = 'A' AND t.typcategory = 'A'"
                ]
            )
            >>= \case
                Right [[Just f, Just t]]
                    | Just f' <- readMaybe (ByteString.Char8.unpack f)
                    , Just t' <- readMaybe (ByteString.Char8.unpack t) ->
                        convertsWithoutBeingTold f' t'
                _ -> scalarConverts from to
    scalarConverts from to =
        query
            conn
            ( mconcat
                [ "SELECT (SELECT castcontext::text FROM pg_catalog.pg_cast WHERE castsource = "
                , base from
                , " AND casttarget = "
                , base to
                , "), "
                , base from
                , " = "
                , base to
                , ", (SELECT typcategory = 'S' FROM pg_catalog.pg_type WHERE oid = "
                , base to
                , ")"
                ]
            )
            >>= \case
                Right [[context, same, toString]] -> pure (same == Just "t" || toString == Just "t" || context `elem` [Just "i", Just "a"])
                _ -> pure False
    -- Through every domain a domain is over.
    base o =
        mconcat
            [ "(WITH RECURSIVE d (oid, typtype, typbasetype) AS (SELECT oid, typtype, typbasetype FROM pg_catalog.pg_type WHERE oid = "
            , ByteString.Char8.pack (show o)
            , " UNION ALL SELECT t.oid, t.typtype, t.typbasetype FROM pg_catalog.pg_type t, d WHERE t.oid = d.typbasetype AND d.typtype = 'd')"
            , " SELECT oid FROM d WHERE typtype <> 'd')"
            ]
    sameBase from to =
        query conn (mconcat ["SELECT ", base from, " = ", base to]) <&> \case
            Right [[Just "t"]] -> True
            _ -> False
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
