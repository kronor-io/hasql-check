-- |
-- Module      : Hasql.Check.Compile
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Checking statements while GHC compiles them, from "Hasql.Check.TH"'s
-- quoters or from a compiler plugin: the configuration, read from the
-- environment, one connection for the whole compilation, and warnings that
-- GHC's output and an editor pick up.
--
-- * @HASQL_CHECK_DATABASE_URL@: a libpq connection string or URI. Unset,
--   nothing is checked.
-- * @HASQL_CHECK_SCHEMA_FILE@: a file that changes whenever the schema does,
--   such as a migration plan.
-- * @HASQL_CHECK_NULLABILITY@, @HASQL_CHECK_TYPES@: when set, the warnings of
--   @hasql-check --nullability@ and @--types@ too.
-- * @HASQL_CHECK_BASELINE@: a baseline file, as @--write-baseline@ writes it.
module Hasql.Check.Compile (
    -- * Configuration
    Config (..),
    readConfig,

    -- * The connection
    Session (..),
    withSession,
    reportUnavailable,

    -- * Warnings
    warnings,
    warning,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Either (fromRight)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Check.Finding (Baseline, Finding, Rule, directivesAbove, readBaseline, settle)
import Hasql.Check.Nullability qualified as Nullability
import Pqi qualified
import Pqi.Ffi qualified
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Prelude


-- * Configuration


data Config = Config
    { databaseUrl :: Maybe String
    , schemaFile :: Maybe FilePath
    , nullability :: Bool
    , types :: Bool
    , baseline :: Maybe FilePath
    }


readConfig :: IO Config
readConfig =
    Config
        <$> nonEmpty "HASQL_CHECK_DATABASE_URL"
        <*> nonEmpty "HASQL_CHECK_SCHEMA_FILE"
        <*> (isJust <$> nonEmpty "HASQL_CHECK_NULLABILITY")
        <*> (isJust <$> nonEmpty "HASQL_CHECK_TYPES")
        <*> nonEmpty "HASQL_CHECK_BASELINE"
  where
    nonEmpty name =
        lookupEnv name >>= \case
            Just "" -> pure Nothing
            other -> pure other


-- * The connection


data Session = Session
    { url :: String
    -- ^ What the connection was made to. A long GHCi session can outlive a
    -- change of HASQL_CHECK_DATABASE_URL, and must follow it.
    , connection :: Pqi.Connection
    , catalogue :: Maybe (ByteString.ByteString, Nullability.Catalogue)
    -- ^ Loaded for nullability and types, keyed by the schema file's contents.
    }


-- | One connection for every statement the compiler checks, created on first
-- use. The lock also keeps the statements of one check together, which
-- nullability needs: it prepares, explains and deallocates a named
-- statement.

{- HLINT ignore session "Avoid restricted function" -}
-- A process-wide variable is the point: it outlives every splice.
session :: MVar (Maybe Session)
session = unsafePerformIO (newMVar Nothing)
{-# NOINLINE session #-}


-- | Run a check on the connection to the given database, with the catalogue
-- if asked for; or why the database cannot be reached. A connection that
-- breaks during a check - a restarted database under a long GHCi session -
-- is replaced, and the check run once more on the new one.
withSession :: Config -> String -> Bool -> (Session -> IO a) -> IO (Either String a)
withSession config url withCatalogue action = modifyMVar session \current -> do
    fingerprint <-
        if withCatalogue
            then maybe (pure ByteString.empty) readFileOrEmpty config.schemaFile
            else pure ByteString.empty
    let attempt existing =
            open fingerprint existing >>= \case
                Left why -> pure (Nothing, Left why)
                Right s -> do
                    result <- action s
                    status <- s.connection.status
                    pure (if status == Pqi.ConnectionOk then Just s else Nothing, Right result)
    attempt current >>= \case
        (Nothing, Right _) | Just _ <- current -> attempt Nothing
        result -> pure result
  where
    open fingerprint = \case
        Just s
            | s.url /= url -> do
                s.connection.finish
                open fingerprint Nothing
            | not withCatalogue || fmap fst s.catalogue == Just fingerprint -> pure (Right s)
            | otherwise -> loadCatalogue fingerprint s.connection
        Nothing -> do
            conn <- Pqi.Ffi.adapter.connectdb (ByteString.Char8.pack url)
            status <- conn.status
            if status /= Pqi.ConnectionOk
                then Left . maybe "cannot connect" ByteString.Char8.unpack <$> conn.errorMessage
                else
                    if withCatalogue
                        then loadCatalogue fingerprint conn
                        else pure (Right (Session url conn Nothing))

    loadCatalogue fingerprint conn =
        Nullability.prepareSession conn >>= \case
            Left err -> pure (Left err)
            Right cat -> pure (Right (Session url conn (Just (fingerprint, cat))))

    readFileOrEmpty path = fromRight ByteString.empty <$> (try (ByteString.readFile path) :: IO (Either SomeException ByteString.ByteString))


-- | Whether the database being unavailable has already been reported.

{- HLINT ignore reportedUnavailable "Avoid restricted function" -}
reportedUnavailable :: IORef Bool
reportedUnavailable = unsafePerformIO (newIORef False)
{-# NOINLINE reportedUnavailable #-}


-- | Say, once a compilation, that the database cannot be reached: not about
-- any one statement, so with no location.
reportUnavailable :: String -> IO ()
reportUnavailable why = do
    reported <- atomicModifyIORef' reportedUnavailable (True,)
    unless reported (hPutStrLn stderr ("hasql-check: not checking statements: " <> why))


-- * Warnings


-- | The warnings to report for a statement at a line of a file, after its
-- directives and the baseline, if any.
warnings :: Config -> [Rule] -> FilePath -> Int -> Text -> [Finding] -> IO [String]
warnings config rules file line fingerprint found
    | null rules = pure []
    | otherwise = do
        fileLines <- readLines file
        known <- traverse readBaselineFile config.baseline
        let (_, messages) = settle rules (directivesAbove fileLines line) (either (const Nothing) Just =<< known) fingerprint found
        pure (maybe [] (\err -> ["cannot read HASQL_CHECK_BASELINE: " <> err]) (known >>= either Just (const Nothing)) <> messages)


-- | A warning in GHC's format, so that tools reading GHC's output pick it up,
-- but written to stderr rather than reported to GHC: under -Werror GHC would
-- fail the build on it, and these are advice.
warning :: FilePath -> (Int, Int) -> String -> IO ()
warning file (line, column) message =
    hPutStrLn stderr (file <> ":" <> show line <> ":" <> show column <> ": warning: hasql-check: " <> message)


-- | A file's lines, or none if it cannot be read.
readLines :: FilePath -> IO [Text]
readLines path = either (const []) (Text.lines . Text.decodeUtf8Lenient) <$> (try (ByteString.readFile path) :: IO (Either SomeException ByteString.ByteString))


readBaselineFile :: FilePath -> IO (Either String Baseline)
readBaselineFile path =
    (try (ByteString.readFile path) :: IO (Either SomeException ByteString.ByteString)) >>= \case
        Left e -> pure (Left (show e))
        Right bytes -> pure (readBaseline (Text.decodeUtf8Lenient bytes))
