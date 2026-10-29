-- | The plugin, as GHC runs it: each case compiles a fixture module with
-- @-fplugin=Hasql.Check.Plugin@, against a throwaway PostgreSQL server.
--
-- GHC finds the plugin and its dependencies through the package environment
-- cabal writes (@write-ghc-environment-files: always@ in cabal.project), and
-- must be the GHC that built them: @ghc-<version>@, or @$GHC@.
module Main where

import Control.Exception (bracket_)
import Control.Monad (unless, when)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isNothing)
import Data.Version (showVersion)
import System.Directory (findExecutable, getCurrentDirectory, listDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Info (fullCompilerVersion)
import System.Process (callProcess, proc, readCreateProcessWithExitCode, readProcess)
import System.Process qualified as Process


main :: IO ()
main = do
    env <- environmentFile
    compiler <- ghc
    withPostgres \conn -> do
        failures <- traverse (run compiler env conn) cases
        let failed = [name | (name, False) <- zip (map caseName cases) failures]
        mapM_ (putStrLn . ("failed: " <>)) failed
        if null failed then putStrLn (show (length cases) <> " passed") else exitFailure


data Case = Case
    { caseName :: String
    , database :: Database
    , extraEnv :: [(String, String)]
    , source :: String
    , expect :: (ExitCode, String) -> Bool
    }


data Database = TestDatabase | NoDatabase | Unreachable


cases :: [Case]
cases =
    [ Case
        "checks nothing when no database is set"
        NoDatabase
        []
        (fixture "Statement Int64 Text" "select nope from customer where id = $1")
        \(code, out) -> code == ExitSuccess && not ("hasql-check:" `isInfixOf` out)
    , Case
        "accepts a statement that fits the schema"
        TestDatabase
        []
        (fixture "Statement Int64 (Maybe Text)" "select nickname from customer where id = $1")
        \(code, _) -> code == ExitSuccess
    , Case
        "fails at the quote on a statement that does not prepare"
        TestDatabase
        []
        (fixture "Statement Int64 Text" "select nope from customer where id = $1")
        \(code, out) ->
            code /= ExitSuccess
                && "Fixture.hs:11:" `isInfixOf` out
                && "column \"nope\" does not exist" `isInfixOf` out
    , Case
        "fails on a column read through a conversion that can fail"
        TestDatabase
        []
        (fixture "Statement Int64 Int64" "select email from customer where id = $1")
        \(code, out) -> code /= ExitSuccess && "column 1 (email) is text, but is read as" `isInfixOf` out
    , Case
        "fails on an enum read without a label the database has"
        TestDatabase
        []
        ( unlines
            [ "{-# LANGUAGE DataKinds, DerivingVia, QuasiQuotes, UndecidableInstances #-}"
            , "module Fixture where"
            , "import Data.Int (Int64)"
            , "import GHC.Generics (Generic)"
            , "import Hasql.Check.Inferred (PgEnum (..), Value)"
            , "import Hasql.Check.Inferred.TH"
            , "import Hasql.Statement (Statement)"
            , "data State = PENDING | PAID"
            , "    deriving stock (Generic)"
            , "    deriving (Value) via PgEnum \"payment_state\" State"
            , "s :: Statement Int64 State"
            , "s = [singletonStatement| select state from payment where id = $1 |]"
            ]
        )
        \(code, out) -> code /= ExitSuccess && "which cannot read labels payment_state has: REFUNDED" `isInfixOf` out
    , Case
        "warns about a nullable column read without a Maybe, without failing"
        TestDatabase
        [("HASQL_CHECK_NULLABILITY", "1")]
        (fixture "Statement Int64 Text" "select nickname from customer where id = $1")
        \(code, out) ->
            code == ExitSuccess
                && "Fixture.hs:11:" `isInfixOf` out
                && "warning: hasql-check: column 1 (nickname) is not a Maybe in Haskell" `isInfixOf` out
    , Case
        "does not fail the build when the database cannot be reached"
        Unreachable
        []
        (fixture "Statement Int64 Text" "select nope from customer where id = $1")
        \(code, out) -> code == ExitSuccess && "hasql-check: not checking statements" `isInfixOf` out
    ]


-- | A module with one statement, of the given type, on line 11.
fixture :: String -> String -> String
fixture type' sql =
    unlines
        [ "{-# LANGUAGE QuasiQuotes #-}"
        , "module Fixture where"
        , ""
        , "import Data.Int (Int64)"
        , "import Data.Text (Text)"
        , "import Hasql.Check.Inferred.TH"
        , "import Hasql.Statement (Statement)"
        , ""
        , ""
        , "s :: " <> type'
        , "s = [singletonStatement| " <> sql <> " |]"
        ]


run :: FilePath -> FilePath -> String -> Case -> IO Bool
run compiler env conn c =
    withSystemTempDirectory "hasql-check-plugin" \dir -> do
        writeFile (dir </> "Fixture.hs") c.source
        inherited <- getEnvironment
        let database = case c.database of
                TestDatabase -> [("HASQL_CHECK_DATABASE_URL", conn)]
                NoDatabase -> []
                Unreachable -> [("HASQL_CHECK_DATABASE_URL", "host=/nonexistent")]
            ours = database <> c.extraEnv
            procEnv = ours <> filter (not . ("HASQL_CHECK_" `isPrefixOf`) . fst) inherited
        (code, out, err) <-
            readCreateProcessWithExitCode
                (proc compiler ["-package-env", env, "-fno-code", "-fplugin=Hasql.Check.Plugin", "Fixture.hs"]){Process.cwd = Just dir, Process.env = Just procEnv}
                ""
        let ok = c.expect (code, out <> err)
        unless ok (putStrLn (c.caseName <> ":\n" <> show code <> "\n" <> out <> err))
        pure ok


-- | The package environment cabal wrote, in this directory or one above.
-- Cabal writes it once a command has run, after the tests of a first
-- @cabal test@: then it is written here, by building the plugin.
environmentFile :: IO FilePath
environmentFile =
    find >>= \case
        Just file -> pure file
        Nothing -> do
            callProcess "cabal" ["build", "-v0", "--write-ghc-environment-files=always", "hasql-check-ghc:lib:hasql-check-ghc"]
            find >>= \case
                Just file -> pure file
                Nothing -> do
                    putStrLn "no .ghc.environment file: set write-ghc-environment-files: always in cabal.project"
                    exitFailure
  where
    find = getCurrentDirectory >>= go
    go dir = do
        found <- filter (".ghc.environment." `isPrefixOf`) <$> listDirectory dir
        case found of
            file : _ -> pure (Just (dir </> file))
            []
                | takeDirectory dir == dir -> pure Nothing
                | otherwise -> go (takeDirectory dir)


-- | The GHC that built the plugin.
ghc :: IO FilePath
ghc =
    lookupEnv "GHC" >>= \case
        Just path -> pure path
        Nothing -> do
            let versioned = "ghc-" <> showVersion fullCompilerVersion
            found <- findExecutable versioned
            pure (maybe "ghc" (const versioned) found)


-- | A connection string for a throwaway server, reachable only over a socket
-- in a temporary directory.
withPostgres :: (String -> IO ()) -> IO ()
withPostgres action =
    withSystemTempDirectory "hasql-check-plugin-db" \dir -> do
        initdb <- findExecutable "initdb"
        when (isNothing initdb) do
            putStrLn "initdb is not on PATH; the plugin tests need a PostgreSQL installation"
            exitFailure
        let dataDir = dir </> "data"
            conn = "host=" <> dir <> " port=5432 user=postgres dbname=postgres"
        _ <- readProcess "initdb" ["--pgdata", dataDir, "--username", "postgres", "--auth", "trust", "--no-sync"] ""
        bracket_
            (callProcess "pg_ctl" ["--pgdata", dataDir, "--log", dir </> "postgres.log", "--options", "-k " <> dir <> " -c listen_addresses=''", "--wait", "--silent", "start"])
            (callProcess "pg_ctl" ["--pgdata", dataDir, "--mode", "immediate", "--silent", "stop"])
            do
                callProcess
                    "psql"
                    [ conn
                    , "--quiet"
                    , "--command"
                    , "create table customer (id int8 primary key, email text not null, nickname text); create type payment_state as enum ('PENDING', 'PAID', 'REFUNDED'); create table payment (id int8 primary key, state payment_state not null)"
                    ]
                action conn
