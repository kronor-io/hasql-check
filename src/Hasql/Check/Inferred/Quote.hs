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
    checkShape,
) where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32)
import Hasql.Check (parseStmt, quoterSettings, renderStmt)
import Hasql.Check.Annotate (annotate, annotatesResults, typename)
import Hasql.Check.Annotate qualified as Slot
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
    , postgresType :: Text
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


-- | The hasql-th quote the inferred one stands for.
annotated :: Inferred -> Either String Text
annotated q = Ast.toText quoterSettings . fst <$> annotatedAst True q


-- | The statement, annotated - its result columns too, if asked - and
-- whether its result columns could be.
annotatedAst :: Bool -> Inferred -> Either String (Ast.PreparableStmt, Bool)
annotatedAst withResults q = do
    stmt <- parseStmt q.sql
    params <- traverse column q.params
    row <- traverse (traverse column) q.row
    let slot = \case
            Slot.Param n -> at n params
            Slot.Column n | withResults -> at n =<< row
            Slot.Column _ -> Nothing
        at n xs = case drop (n - 1) xs of
            t : _ | n >= 1 -> Just t
            _ -> Nothing
    pure (annotate slot stmt, annotatesResults stmt)
  where
    column c = typename c.postgresType c.nullable


-- | Whether the statement takes as many parameters as the Haskell type
-- sends, and returns as many columns as it reads, each of a type it can be
-- read as.
--
-- An annotated column is cast to the type it is read as, and Postgres casts
-- anything it can convert, even text to a number, which fails at runtime for
-- text that is not one. So the column must have that type already, or one
-- Postgres converts to it without being told to: implicitly, or on
-- assignment. A column behind a @*@ is not annotated, and must already have
-- the type.
checkShape :: Pqi.Connection -> Inferred -> IO [String]
checkShape conn q = case annotatedAst False q of
    Left _ -> pure []
    Right (paramsOnly, annotatable) -> do
        expected <- traverse (oid . (.postgresType)) (concat q.row)
        prepared <- conn.prepare "" (renderStmt paramsOnly) Nothing
        status <- maybe (pure Pqi.FatalError) (.resultStatus) prepared
        if status /= Pqi.CommandOk
            then pure []
            else
                conn.describePrepared "" >>= \case
                    Nothing -> pure []
                    Just d -> do
                        nparams <- d.nparams
                        n <- d.nfields
                        found <- traverse (\i -> (,) <$> (maybe "?column?" (Text.unpack . Text.decodeUtf8Lenient) <$> d.fname i) <*> d.ftype i) [0 .. n - 1]
                        let paramErrors =
                                [ "the statement takes " <> count "parameter" (fromIntegral nparams) <> ", but the Haskell type sends " <> show (length q.params)
                                | fromIntegral nparams /= length q.params
                                ]
                        columnErrors <- case q.row of
                            Nothing -> pure []
                            Just row
                                | length found /= length row ->
                                    pure ["the statement returns " <> count "column" (length found) <> ", but the Haskell type reads " <> show (length row)]
                                | otherwise ->
                                    fmap concat . sequence $
                                        [ columnError annotatable i name c t e
                                        | (i :: Int, c, (name, t), Just e) <- zip4 [1 ..] row found expected
                                        , t /= e
                                        ]
                        pure (paramErrors <> columnErrors)
  where
    columnError annotatable i name c t e = do
        fromName <- nameOf t
        toName <- nameOf e
        converts <- if annotatable then convertsWithoutBeingTold t e else pure False
        let mismatch = concat ["column ", show i, " (", name, ") is ", fromName, ", but is read as ", Text.unpack c.haskellType]
            conversion = concat [" (", toName, "), which Postgres converts it to only when told to, and can fail to at runtime"]
        pure [mismatch <> if annotatable then conversion else "" | not converts]
    -- Through their base types, so that a domain converts as the type it is
    -- over.
    convertsWithoutBeingTold from to =
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
                ]
            )
            >>= \case
                Right [[context, same]] -> pure (same == Just "t" || context `elem` [Just "i", Just "a"])
                _ -> pure False
    base o =
        let o' = ByteString.Char8.pack (show o)
         in mconcat ["(SELECT CASE WHEN typtype = 'd' THEN typbasetype ELSE oid END FROM pg_catalog.pg_type WHERE oid = ", o', ")"]
    count :: String -> Int -> String
    count what 1 = "1 " <> what
    count what k = unwords [show k, what <> "s"]
    oid :: Text -> IO (Maybe Word32)
    oid name =
        query conn (mconcat ["SELECT ", quoteLiteral name, "::regtype::oid::text"]) >>= \case
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
