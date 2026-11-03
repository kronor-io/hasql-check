{-# LANGUAGE CPP #-}

-- | The plugin, as GHC runs it: each case compiles a fixture module with
-- @-fplugin=Hasql.Check.Plugin@, against a throwaway PostgreSQL server. Or
-- hasql-check-hie, as a sandboxed build runs it: the case compiles the
-- module to a .hie file, which hasql-check-hie reads and hasql-check
-- --inferred checks against the same server.
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
    , via :: Via
    , before :: [Database]
    -- ^ Compilations of the same module, in the same directory, first.
    , database :: Database
    , extraEnv :: [(String, String)]
    , source :: String
    , expect :: (ExitCode, String) -> Bool
    }


data Database = TestDatabase | NoDatabase | Unreachable


-- | Which of the two checks the statements: the compiler plugin, or
-- hasql-check --inferred on what hasql-check-hie read from the .hie file.
data Via = Plugin | Hie


cases :: [Case]
cases =
    [ Case
        "checks nothing when no database is set"
        Plugin
        []
        NoDatabase
        []
        (fixture "Statement Int64 Text" "select nope from customer where id = $1")
        \(code, out) -> code == ExitSuccess && not ("hasql-check:" `isInfixOf` out)
    , Case
        "accepts a statement that fits the schema"
        Plugin
        []
        TestDatabase
        []
        (fixture "Statement Int64 (Maybe Text)" "select nickname from customer where id = $1")
        \(code, _) -> code == ExitSuccess
    , Case
        "fails at the quote on a statement that does not prepare"
        Plugin
        []
        TestDatabase
        []
        (fixture "Statement Int64 Text" "select nope from customer where id = $1")
        \(code, out) ->
            code /= ExitSuccess
                && "Fixture.hs:11:" `isInfixOf` out
                && "column \"nope\" does not exist" `isInfixOf` out
    , Case
        "fails on a column read through a conversion that can fail"
        Plugin
        []
        TestDatabase
        []
        (fixture "Statement Int64 Int64" "select email from customer where id = $1")
        \(code, out) -> code /= ExitSuccess && "column 1 (email) is text, but is read as" `isInfixOf` out
    , Case
        "fails on an enum read without a label the database has"
        Plugin
        []
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
        Plugin
        []
        TestDatabase
        [("HASQL_CHECK_NULLABILITY", "1")]
        (fixture "Statement Int64 Text" "select nickname from customer where id = $1")
        \(code, out) ->
            code == ExitSuccess
                && "Fixture.hs:11:" `isInfixOf` out
                && "warning: hasql-check: column 1 (nickname) is not a Maybe in Haskell" `isInfixOf` out
    , Case
        "checks the statement GHC compiled, after CPP, not the file"
        Plugin
        []
        TestDatabase
        []
        ( unlines
            [ "{-# LANGUAGE CPP, QuasiQuotes #-}"
            , "module Fixture where"
            , "import Data.Int (Int64)"
            , "import Data.Text (Text)"
            , "import Hasql.Check.Inferred.TH"
            , "import Hasql.Statement (Statement)"
            , "#define FIELD nope"
            , "s :: Statement Int64 Text"
            , "s = [singletonStatement| select FIELD from customer where id = $1 |]"
            ]
        )
        \(code, out) -> code /= ExitSuccess && "column \"nope\" does not exist" `isInfixOf` out
    , Case
        "checks a module compiled before without a database once one is set"
        Plugin
        [NoDatabase]
        TestDatabase
        []
        (fixture "Statement Int64 Text" "select nope from customer where id = $1")
        \(code, out) -> code /= ExitSuccess && "column \"nope\" does not exist" `isInfixOf` out
    , Case
        "does not fail the build when the database cannot be reached"
        Plugin
        []
        Unreachable
        []
        (fixture "Statement Int64 Text" "select nope from customer where id = $1")
        \(code, out) -> code == ExitSuccess && "hasql-check: not checking statements" `isInfixOf` out
    , Case
        "checks a statement from its .hie file"
        Hie
        []
        TestDatabase
        []
        (fixture "Statement Int64 Int64" "select email from customer where id = $1")
        \(code, out) ->
            code /= ExitSuccess
                && "Error in Fixture.hs:11:5" `isInfixOf` out
                && "column 1 (email) is text, but is read as GHC.Internal.Int.Int64" `isInfixOf` out
    ]
        <> fieldCases


-- | Statements with $1.$field param fields, which only hasql-check's
-- kronor-syntax flag accepts. Each slot's type is the type of the field it
-- sends, which GHC only knows once it has solved the field's HasField.
fieldCases :: [Case]
#ifdef KRONOR_SYNTAX
fieldCases =
    [ Case
        "recovers the types of a record parameter's fields from a .hie file"
        Hie
        []
        TestDatabase
        []
        (discrepancy "Maybe Int64" "Bool")
        \(code, out) ->
            code == ExitSuccess
                && ( concat
                        [ "\"params\":["
                        , param "Data.Text.Internal.Text" "text" False
                        , ","
                        , param "Data.UUID.Types.Internal.UUID" "uuid" True
                        , ","
                        , param "GHC.Internal.Int.Int64" "int8" True
                        , ","
                        , param "Data.Text.Internal.Text" "text" True
                        , "],\"row\":["
                        , param "GHC.Internal.Types.Bool" "bool" False
                        , "]"
                        ]
                   )
                `isInfixOf` out
    , Case
        "reports a record parameter's field sent as the wrong type, from a .hie file"
        Hie
        []
        TestDatabase
        []
        (discrepancy "Maybe Text" "Bool")
        \(code, out) -> code /= ExitSuccess && "function record_discrepancy(text, uuid, text, text) does not exist" `isInfixOf` out
    , Case
        "reports a column read as the wrong type, with a record parameter, from a .hie file"
        Hie
        []
        TestDatabase
        []
        (discrepancy "Maybe Int64" "Int64")
        \(code, out) -> code /= ExitSuccess && "column 1 (record_discrepancy) is boolean, but is read as GHC.Internal.Int.Int64" `isInfixOf` out
    , Case
        "accepts a statement with a record parameter that fits the schema"
        Plugin
        []
        TestDatabase
        []
        (discrepancy "Maybe Int64" "Bool")
        \(code, _) -> code == ExitSuccess
    , Case
        "fails on a column read as the wrong type, with a record parameter"
        Plugin
        []
        TestDatabase
        []
        (discrepancy "Maybe Int64" "Int64")
        \(code, out) -> code /= ExitSuccess && "column 1 (record_discrepancy) is boolean, but is read as GHC.Internal.Int.Int64" `isInfixOf` out
    ]
  where
    param type' postgres nullable =
        concat ["{\"type\":\"", type', "\",\"postgres\":\"", postgres, "\",\"labels\":[],\"nullable\":", if nullable then "true" else "false", "}"]


{- | A module with one statement that sends the fields of a record, given
the type of its amount field and the type of the row: record_discrepancy
takes an int8 amount and returns a boolean.

The fork's parser wants white space after a field, before a comma or a
closing parenthesis, so the fields are written one a line.
-}
discrepancy :: String -> String -> String
discrepancy amount row =
    unlines
        [ "{-# LANGUAGE DataKinds, QuasiQuotes #-}"
        , "module Fixture where"
        , ""
        , "import Data.Int (Int64)"
        , "import Data.Text (Text)"
        , "import Data.UUID (UUID)"
        , "import Hasql.Check.Inferred.TH"
        , "import Hasql.Statement (Statement)"
        , ""
        , "data Discrepancy = Discrepancy {kind :: Text, orderId :: Maybe UUID, amount :: " <> amount <> ", note :: Maybe Text}"
        , ""
        , "s :: Statement Discrepancy " <> row
        , "s ="
        , "    [singletonStatement|"
        , "        select record_discrepancy("
        , "              $1.$kind"
        , "            , $1.$orderId"
        , "            , $1.$amount"
        , "            , $1.$note"
        , "        )"
        , "    |]"
        ]
#else
fieldCases = []
#endif


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
        let compile db = do
                let database = case db of
                        TestDatabase -> [("HASQL_CHECK_DATABASE_URL", conn)]
                        NoDatabase -> []
                        Unreachable -> [("HASQL_CHECK_DATABASE_URL", "host=/nonexistent")]
                    ours = database <> c.extraEnv
                    procEnv = ours <> filter (not . ("HASQL_CHECK_" `isPrefixOf`) . fst) inherited
                -- Interfaces written, so that a later compilation can reuse them.
                readCreateProcessWithExitCode
                    (proc compiler ["-package-env", env, "-fno-code", "-fwrite-interface", "-fplugin=Hasql.Check.Plugin", "Fixture.hs"]){Process.cwd = Just dir, Process.env = Just procEnv}
                    ""
        (code, out, err) <- case c.via of
            Plugin -> do
                mapM_ compile c.before
                compile c.database
            Hie -> do
                (built, buildOut, buildErr) <-
                    readCreateProcessWithExitCode
                        (proc compiler ["-package-env", env, "-fno-code", "-fwrite-ide-info", "-hiedir", ".", "Fixture.hs"]){Process.cwd = Just dir}
                        ""
                if built /= ExitSuccess
                    then pure (built, buildOut, buildErr)
                    else do
                        (read', quotes, readErr) <- readCreateProcessWithExitCode (proc "hasql-check-hie" ["Fixture.hie"]){Process.cwd = Just dir} ""
                        writeFile (dir </> "statements.jsonl") quotes
                        if read' /= ExitSuccess
                            then pure (read', quotes, readErr)
                            else do
                                (checked, checkOut, checkErr) <-
                                    readCreateProcessWithExitCode
                                        (proc "hasql-check" ["--database", conn, "--inferred", "statements.jsonl"]){Process.cwd = Just dir}
                                        ""
                                pure (checked, quotes <> checkOut, readErr <> checkErr)
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
                    , "create table customer (id int8 primary key, email text not null, nickname text); create type payment_state as enum ('PENDING', 'PAID', 'REFUNDED'); create table payment (id int8 primary key, state payment_state not null); create function record_discrepancy(text, uuid, int8, text) returns boolean language sql as 'select true'"
                    ]
                action conn
