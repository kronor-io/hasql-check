-- |
-- Module      : Hasql.Check.TH
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- hasql-th's quasiquoters, checking their statements against a live database
-- while they compile. Import this module instead of "Hasql.TH".
--
-- Nothing is checked unless @HASQL_CHECK_DATABASE_URL@ is set, so builds
-- without a database - CI, nix - behave exactly as with "Hasql.TH". When it
-- is set, a statement that does not @PREPARE@ against that database is a
-- compile error at its quote.
--
-- * @HASQL_CHECK_DATABASE_URL@: a libpq connection string or URI.
-- * @HASQL_CHECK_SCHEMA_FILE@: a file that changes whenever the schema does,
--   such as a migration plan. Every module with a checked quote depends on
--   it, so a schema change rechecks them. Cabal only notices it if it is also
--   listed in the package's @extra-source-files@; GHCi notices it regardless.
-- * @HASQL_CHECK_NULLABILITY@: when set, the warnings of @hasql-check
--   --nullability@ become compiler warnings.
--
-- GHC runs a quote only when it compiles the module, so a module compiled
-- before the database was set, or before the schema changed, is not checked
-- until it recompiles. Run the @hasql-check@ executable in CI for a check
-- of everything.
module Hasql.Check.TH (
    -- * Statements
    singletonStatement,
    maybeStatement,
    vectorStatement,
    foldStatement,
    resultlessStatement,
    rowsAffectedStatement,
    uncheckedSql,
    uncheckedSqlFile,

    -- * Checking any quoter
    checked,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Either (fromRight)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Hasql.Check (parseStmt, prepareStmt)
import Hasql.Check.Nullability qualified as Nullability
import Hasql.TH (uncheckedSql, uncheckedSqlFile)
import Hasql.TH qualified as TH
import Language.Haskell.TH (Loc (..), Q, location, runIO)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (addDependentFile)
import Pqi qualified
import Pqi.Ffi qualified
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Prelude


singletonStatement :: QuasiQuoter
singletonStatement = withRows TH.singletonStatement


maybeStatement :: QuasiQuoter
maybeStatement = withRows TH.maybeStatement


vectorStatement :: QuasiQuoter
vectorStatement = withRows TH.vectorStatement


foldStatement :: QuasiQuoter
foldStatement = withRows TH.foldStatement


resultlessStatement :: QuasiQuoter
resultlessStatement = checked TH.resultlessStatement


rowsAffectedStatement :: QuasiQuoter
rowsAffectedStatement = checked TH.rowsAffectedStatement


-- | Check a quoter's statements, then run it. For quoters built on hasql-th's
-- statement syntax.
checked :: QuasiQuoter -> QuasiQuoter
checked = wrap False


-- | 'checked', also checking nullability when asked to.
withRows :: QuasiQuoter -> QuasiQuoter
withRows = wrap True


wrap :: Bool -> QuasiQuoter -> QuasiQuoter
wrap rows quoter = quoter{quoteExp = \sql -> check rows sql *> quoteExp quoter sql}


-- * Configuration


data Config = Config
    { databaseUrl :: Maybe String
    , schemaFile :: Maybe FilePath
    , nullability :: Bool
    }


readConfig :: IO Config
readConfig =
    Config
        <$> nonEmpty "HASQL_CHECK_DATABASE_URL"
        <*> nonEmpty "HASQL_CHECK_SCHEMA_FILE"
        <*> (isJust <$> nonEmpty "HASQL_CHECK_NULLABILITY")
  where
    nonEmpty name =
        lookupEnv name >>= \case
            Just "" -> pure Nothing
            other -> pure other


-- * The session


data Session = Session
    { connection :: Pqi.Connection
    , catalogue :: Maybe (ByteString.ByteString, Nullability.Catalogue)
    -- ^ Loaded for nullability, keyed by the schema file's contents.
    }


-- | One connection for every quote the compiler runs, created on first use.
-- The lock also keeps the statements of one check together, which nullability
-- needs: it prepares, explains and deallocates a named statement.

{- HLINT ignore session "Avoid restricted function" -}
-- A process-wide variable is the point: it outlives every splice.
session :: MVar (Maybe Session)
session = unsafePerformIO (newMVar Nothing)
{-# NOINLINE session #-}


-- | Whether the database being unavailable has already been reported.

{- HLINT ignore reportedUnavailable "Avoid restricted function" -}
reportedUnavailable :: IORef Bool
reportedUnavailable = unsafePerformIO (newIORef False)
{-# NOINLINE reportedUnavailable #-}


data Outcome
    = -- | The database cannot be reached; nothing was checked.
      Unavailable String
    | Failed String
    | Checked [Nullability.Warning]


check :: Bool -> String -> Q ()
check rows sql = do
    config <- runIO readConfig
    forM_ config.schemaFile addDependentFile
    forM_ config.databaseUrl \url -> do
        outcome <- runIO (checkStatement config url (rows && config.nullability) sql)
        case outcome of
            Unavailable why -> do
                first <- runIO (atomicModifyIORef' reportedUnavailable (True,))
                -- Not about this quote, so no location.
                when (not first) (runIO (hPutStrLn stderr ("hasql-check: not checking statements: " <> why)))
            Failed err -> fail ("hasql-check: the statement does not prepare against HASQL_CHECK_DATABASE_URL:\n" <> err)
            Checked warnings -> forM_ warnings (warn . Nullability.describe)


-- | A warning in GHC's format, so that tools reading GHC's output pick it up,
-- but written to stderr rather than reported to GHC: under -Werror GHC would
-- fail the build on it, and these are advice.
warn :: String -> Q ()
warn message = do
    loc <- location
    let (line, column) = loc.loc_start
    runIO (hPutStrLn stderr (loc.loc_filename <> ":" <> show line <> ":" <> show column <> ": warning: hasql-check: " <> message))


checkStatement :: Config -> String -> Bool -> String -> IO Outcome
checkStatement config url withNullability sql = case parseStmt (Text.pack sql) of
    -- hasql-th reports the parse error itself.
    Left _ -> pure (Checked [])
    Right stmt -> modifyMVar session \current -> do
        fingerprint <-
            if withNullability
                then maybe (pure ByteString.empty) readFileOrEmpty config.schemaFile
                else pure ByteString.empty
        attempt fingerprint stmt current >>= \case
            -- A dropped connection - a restarted database under a long GHCi
            -- session - is retried once on a new one.
            (Nothing, Failed _) | Just _ <- current -> attempt fingerprint stmt Nothing
            result -> pure result
  where
    attempt fingerprint stmt current =
        open fingerprint current >>= \case
            Left why -> pure (Nothing, Unavailable why)
            Right s -> do
                result <- prepareStmt s.connection "" stmt
                status <- s.connection.status
                let kept = if status == Pqi.ConnectionOk then Just s else Nothing
                case result of
                    Left err -> pure (kept, Failed err)
                    Right () -> case s.catalogue of
                        Just (_, cat) | withNullability -> (kept,) . Checked <$> Nullability.check s.connection cat stmt
                        _ -> pure (kept, Checked [])

    open fingerprint = \case
        Just s
            | not withNullability || fmap fst s.catalogue == Just fingerprint -> pure (Right s)
            | otherwise -> loadCatalogue fingerprint s.connection
        Nothing -> do
            conn <- Pqi.Ffi.adapter.connectdb (ByteString.Char8.pack url)
            status <- conn.status
            if status /= Pqi.ConnectionOk
                then Left . maybe "cannot connect" (ByteString.Char8.unpack) <$> conn.errorMessage
                else
                    if withNullability
                        then loadCatalogue fingerprint conn
                        else pure (Right (Session conn Nothing))

    loadCatalogue fingerprint conn =
        Nullability.prepareSession conn >>= \case
            Left err -> pure (Left err)
            Right cat -> pure (Right (Session conn (Just (fingerprint, cat))))

    readFileOrEmpty path = fromRight ByteString.empty <$> (try (ByteString.readFile path) :: IO (Either SomeException ByteString.ByteString))
