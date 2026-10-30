module Main where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Hasql.ThFormat (ProcessResult (..), processBytes, processFile)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import Prelude


data Mode
    = -- | checkOnly, files
      ModeFiles Bool [FilePath]
    | ModeStdin


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case parseArgs args of
        Left err -> do
            putStrLn err
            putStrLn (usage progName)
            exitWith (ExitFailure 2)
        Right ModeStdin -> do
            input <- BS.getContents
            case processBytes input of
                Left err -> do
                    putStrLn ("Error: " <> err)
                    exitWith (ExitFailure 1)
                Right output ->
                    BS.putStr output
        Right (ModeFiles checkOnly files)
            | null files -> do
                putStrLn (usage progName)
                exitWith (ExitFailure 2)
            | otherwise -> do
                results <- mapM (processOneFile checkOnly) files
                let hasErrors = any isError results
                    hasChanges = any isChanged results
                when hasErrors (exitWith (ExitFailure 1))
                when (checkOnly && hasChanges) (exitWith (ExitFailure 1))


processOneFile :: Bool -> FilePath -> IO ProcessResult
processOneFile checkOnly filePath = do
    result <- processFile checkOnly filePath
    case result of
        Error err -> do
            putStrLn ("Error processing " <> filePath <> ": " <> err)
            pure result
        Changed
            | checkOnly -> do
                putStrLn ("Would reformat: " <> filePath)
                pure result
            | otherwise -> do
                putStrLn ("Reformatted: " <> filePath)
                pure result
        Unchanged -> pure result


isError :: ProcessResult -> Bool
isError (Error _) = True
isError _ = False


isChanged :: ProcessResult -> Bool
isChanged Changed = True
isChanged _ = False


parseArgs :: [String] -> Either String Mode
parseArgs = go Nothing False []
  where
    go mStdin checkOnly files [] =
        case mStdin of
            Just True
                | not (null files) -> Left "Cannot combine --stdin with file arguments."
                | checkOnly -> Left "Cannot combine --stdin with --check."
                | otherwise -> Right ModeStdin
            _ -> Right (ModeFiles checkOnly (reverse files))
    go mStdin _ files ("--check" : rest) =
        go mStdin True files rest
    go _ checkOnly files ("--stdin" : rest) =
        go (Just True) checkOnly files rest
    go _ _ _ (('-' : '-' : opt) : _) =
        Left ("Unknown option: --" <> opt)
    go mStdin checkOnly files (f : rest) =
        go mStdin checkOnly (f : files) rest


usage :: String -> String
usage progName =
    "Usage: "
        <> progName
        <> " [--check] files...\n"
        <> "       "
        <> progName
        <> " --stdin\n\n"
        <> "Options:\n"
        <> "  --check       Don't modify files, exit with code 1 if any file would change\n"
        <> "  --stdin       Read from stdin, write formatted output to stdout\n"
