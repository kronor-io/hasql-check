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
-- * @HASQL_CHECK_TYPES@: when set, so do the warnings of @hasql-check
--   --types@.
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

import Control.Monad (forM_)
import Data.Text qualified as Text
import Hasql.Check (parseStmt, prepareStmt, renderStmt)
import Hasql.Check.Compile (Config (..), Session (..), readConfig, reportUnavailable, warning, warnings, withSession)
import Hasql.Check.Finding (Rule (..), statementHash)
import Hasql.Check.Nullability qualified as Nullability
import Hasql.Check.Types qualified as Types
import Hasql.TH (uncheckedSql, uncheckedSqlFile)
import Hasql.TH qualified as TH
import Language.Haskell.TH (Loc (..), Q, location, runIO)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (addDependentFile)
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


-- * Checking


check :: Bool -> String -> Q ()
check rows sql = do
    config <- runIO readConfig
    forM_ config.schemaFile addDependentFile
    forM_ config.databaseUrl \url -> case parseStmt (Text.pack sql) of
        -- hasql-th reports the parse error itself.
        Left _ -> pure ()
        Right stmt -> do
            let rules = [Nullable | rows && config.nullability] <> [Narrowing | config.types]
            outcome <- runIO $ withSession config url (not (null rules)) \s ->
                prepareStmt s.connection "" stmt >>= \case
                    Left err -> pure (Left err)
                    Right () -> case s.catalogue of
                        Just (_, cat) | not (null rules) -> do
                            nullable <- if Nullable `elem` rules then Nullability.check s.connection cat stmt else pure []
                            narrowing <- if Narrowing `elem` rules then Types.check s.connection cat stmt else pure []
                            pure (Right (nullable <> narrowing))
                        _ -> pure (Right [])
            case outcome of
                Left why -> runIO (reportUnavailable why)
                Right (Left err) -> fail ("hasql-check: the statement does not prepare against HASQL_CHECK_DATABASE_URL:\n" <> err)
                -- Without warnings asked for, no location is needed: there is
                -- none outside GHC, under runQ.
                Right (Right _) | null rules -> pure ()
                Right (Right found) -> do
                    loc <- location
                    messages <- runIO (warnings config rules loc.loc_filename (fst loc.loc_start) (statementHash (renderStmt stmt)) found)
                    forM_ messages (runIO . warning loc.loc_filename loc.loc_start)
