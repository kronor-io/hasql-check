-- |
-- Module      : Main
-- Copyright   : (c) 2022 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import Data.ByteString qualified
import Data.Either (isLeft, rights)
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Encoding qualified
import Hasql.Check
import Hasql.Check.Finding
import Hasql.Check.Lexer
import Hasql.Check.Nullability qualified as Nullability
import Hasql.Check.Parser
import Hasql.Check.Quoter
import Hasql.Check.Types qualified as Types
import Hasql.Connection qualified
import Hasql.Connection.Settings qualified
import Hasql.Session qualified
import Pqi qualified
import Pqi.Ffi qualified
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Environment qualified
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStr, hPutStrLn, stderr)
import Prelude hiding (lex)


data Options = Options
    { database :: Maybe Text
    , extraQuoters :: [Text]
    , listOnly :: Bool
    , nullability :: Bool
    , types :: Bool
    , baselineFile :: Maybe FilePath
    , writeBaseline :: Maybe FilePath
    , failOnWarnings :: Bool
    , filesFrom :: Maybe FilePath
    , showHelp :: Bool
    }


defaultOptions :: Options
defaultOptions =
    Options
        { database = Nothing
        , extraQuoters = []
        , listOnly = False
        , nullability = False
        , types = False
        , baselineFile = Nothing
        , writeBaseline = Nothing
        , failOnWarnings = False
        , filesFrom = Nothing
        , showHelp = False
        }


options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['d']
        ["database"]
        (ReqArg (\s o -> o{database = Just (Data.Text.pack s)}) "CONNINFO")
        "libpq connection string or URI to prepare against"
    , Option
        ['q']
        ["quoter"]
        (ReqArg (\s o -> o{extraQuoters = Data.Text.pack s : o.extraQuoters}) "NAME")
        "also check quasiquotes with this quoter name (repeatable)"
    , Option
        ['l']
        ["list"]
        (NoArg (\o -> o{listOnly = True}))
        "list the quasiquotes that would be checked, without connecting"
    , Option
        ['n']
        ["nullability"]
        (NoArg (\o -> o{nullability = True}))
        "warn about result columns annotated non-null that can be null"
    , Option
        ['t']
        ["types"]
        (NoArg (\o -> o{types = True}))
        "warn about annotations that convert a value to a narrower type"
    , Option
        ['b']
        ["baseline"]
        (ReqArg (\s o -> o{baselineFile = Just s}) "FILE")
        "ignore the findings listed in FILE"
    , Option
        []
        ["write-baseline"]
        (ReqArg (\s o -> o{writeBaseline = Just s}) "FILE")
        "write the findings, after directives, to FILE as a baseline"
    , Option
        []
        ["fail-on-warnings"]
        (NoArg (\o -> o{failOnWarnings = True}))
        "exit 1 if anything is warned about"
    , Option
        []
        ["files0-from"]
        (ReqArg (\s o -> o{filesFrom = Just s}) "FILE")
        "also check the NUL-separated file names in FILE, or stdin if FILE is -"
    , Option
        ['h']
        ["help"]
        (NoArg (\o -> o{showHelp = True}))
        "show this help"
    ]


usage :: IO String
usage = do
    progName <- getProgName
    let header =
            unlines
                [ "Usage: " ++ progName ++ " [options...] FILES..."
                , ""
                , "Prepares the SQL in every hasql-th quasiquote in FILES against a live database,"
                , "so statements that do not match the schema fail here instead of at runtime."
                , ""
                , "The database is the first of: --database, $DATABASE_URL, $psql_conn_string, and"
                , "otherwise libpq's own defaults (PGHOST, PGPORT, PGDATABASE, PGUSER, ...)."
                , ""
                , "Quoters checked by default: " ++ Data.Text.unpack (Data.Text.intercalate ", " quotersSupported)
                ]
    pure (usageInfo header options)


execOpts :: IO (Options, [FilePath])
execOpts = do
    argv <- getArgs
    case getOpt Permute options argv of
        (fs, files, []) -> do
            let opts = foldl (flip ($)) defaultOptions fs
            when opts.showHelp do
                usage >>= putStr
                exitSuccess
            -- One invocation for every file, rather than xargs splitting them
            -- between several, so that --write-baseline sees all the findings.
            listed <- case opts.filesFrom of
                Nothing -> pure []
                Just path -> do
                    contents <- if path == "-" then Data.ByteString.getContents else Data.ByteString.readFile path
                    pure
                        [ Data.Text.unpack (Data.Text.Encoding.decodeUtf8Lenient name)
                        | name <- Data.ByteString.split 0 contents
                        , not (Data.ByteString.null name)
                        ]
            let allFiles = files <> listed
            when (null allFiles && null opts.filesFrom) do
                usage >>= hPutStr stderr
                exitWith (ExitFailure 2)
            pure (opts, allFiles)
        (_, _, errs) -> do
            usage >>= hPutStr stderr . (concat errs ++)
            exitWith (ExitFailure 2)


-- | The connection string. An empty one leaves every setting to libpq's
-- environment variables and defaults.
connectionString :: Options -> IO Text
connectionString opts = case opts.database of
    Just conn -> pure conn
    Nothing -> do
        databaseUrl <- nonEmpty <$> System.Environment.lookupEnv "DATABASE_URL"
        legacy <- nonEmpty <$> System.Environment.lookupEnv "psql_conn_string"
        pure (maybe "" Data.Text.pack (databaseUrl <|> legacy))
  where
    nonEmpty = \case
        Just "" -> Nothing
        other -> other


getConnection :: Options -> IO Hasql.Connection.Connection
getConnection opts = do
    sqlConnString <- connectionString opts
    eConn <- Hasql.Connection.acquire Pqi.Ffi.adapter (Hasql.Connection.Settings.connectionString sqlConnString)
    case eConn of
        Left err -> do
            error (show err)
        Right conn -> pure conn


-- | hasql 2 has no @withLibPQConnection@; the supported escape hatch is a
-- session built with 'Hasql.Session.onLibpqConnection', which hands over the
-- raw pqi connection and takes it back unchanged.
withLibPqConnection :: Hasql.Connection.Connection -> (Pqi.Connection -> IO a) -> IO a
withLibPqConnection conn action = do
    result <-
        Hasql.Connection.use
            conn
            (Hasql.Session.onLibpqConnection (\pq -> (\r -> (Right r, pq)) <$> action pq))
    case result of
        Left err -> error (show err)
        Right a -> pure a


data PrepareError
    = ParseError FilePath String
    | DbError QuasiQuote String
    deriving stock (Eq, Show)


-- | The checked quasiquotes in each file, or why the file could not be read.
collect :: [Text] -> [FilePath] -> IO [(FilePath, Either String [QuasiQuote])]
collect quoters files =
    forM files \file -> do
        errParse <- parseFile file
        pure (file, filter (\(QuasiQuote (_, quoter) _) -> isQuoterIn quoters quoter) <$> errParse)


location :: FilePath -> QuasiQuote -> String
location file (QuasiQuote (AlexPn _ quoterRow quoterCol, _) _) =
    file <> ":" <> show quoterRow <> ":" <> show quoterCol


main :: IO ()
main = do
    (opts, fileList) <- execOpts
    parsed <- collect (opts.extraQuoters <> quotersSupported) fileList
    if opts.listOnly
        then do
            forM_ parsed \case
                (file, Left err) -> putStrLn ("Error in " <> file <> "\n" <> err)
                (file, Right qqs) -> forM_ qqs \qq@(QuasiQuote (_, quoter) _) ->
                    putStrLn (location file qq <> ": " <> Data.Text.unpack quoter)
            when (any (isLeft . snd) parsed) (exitWith (ExitFailure 1))
        else do
            baseline <- forM opts.baselineFile \path -> do
                contents <- Data.Text.Encoding.decodeUtf8Lenient <$> Data.ByteString.readFile path
                either (\err -> hPutStrLn stderr err >> exitWith (ExitFailure 2)) pure (readBaseline contents)
            conn <- getConnection opts
            results <- withLibPqConnection conn \libPqConn -> do
                catalogue <-
                    if opts.nullability || opts.types
                        then either (error . ("Cannot read the catalogue: " <>)) Just <$> Nullability.prepareSession libPqConn
                        else pure Nothing
                fmap concat . forM parsed $ \case
                    (file, Left err) -> do
                        putStrLn ("Error in " <> file)
                        putStrLn err
                        pure [Left (ParseError file err)]
                    (file, Right qqs) -> do
                        fileLines <- Data.Text.lines . Data.Text.Encoding.decodeUtf8Lenient <$> Data.ByteString.readFile file
                        forM qqs \qq@(QuasiQuote (AlexPn _ line _, quoter) (_, stmt)) -> do
                            errPrepare <- Hasql.Check.prepare libPqConn "" stmt
                            case errPrepare of
                                Left err -> do
                                    putStrLn ("Error in " <> location file qq)
                                    putStrLn err
                                    pure (Left (DbError qq err))
                                Right _ -> case (catalogue, parseStmt stmt) of
                                    (Just cat, Right ast) -> do
                                        let checkedRules =
                                                [Nullable | opts.nullability && isQuoterIn Nullability.resultQuoters quoter]
                                                    <> [Narrowing | opts.types]
                                        nullable <- if Nullable `elem` checkedRules then Nullability.check libPqConn cat ast else pure []
                                        narrowing <- if opts.types then Types.check libPqConn cat ast else pure []
                                        let h = statementHash (renderStmt ast)
                                            (kept, messages) = settle checkedRules (directivesAbove fileLines line) baseline h (nullable <> narrowing)
                                        when (not (null messages)) do
                                            putStrLn ("Warning in " <> location file qq)
                                            forM_ messages \m -> putStrLn ("  " <> m)
                                        pure (Right (length messages, [BaselineEntry h f.rule f.subject (Data.Text.pack (location file qq)) | f <- kept]))
                                    _ -> pure (Right (0, []))
            let (warningCounts, entries) = unzip (rights results)
            forM_ opts.writeBaseline \path ->
                Data.ByteString.writeFile path (Data.Text.Encoding.encodeUtf8 (renderBaseline (concat entries)))
            when (any isLeft results) (exitWith (ExitFailure 1))
            when (opts.failOnWarnings && sum warningCounts > 0) (exitWith (ExitFailure 1))
