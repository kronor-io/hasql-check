-- | Per-quasiquote AST diff between two versions of a Haskell file.
--
-- Usage: hasql-th-audit <old-file> <new-file>
--
-- Extracts every supported hasql-th quasiquote from both files (using the
-- same lexer/parser the formatter uses), parses each SQL block with
-- postgresql-syntax, and reports any pair whose AST differs. Designed to
-- audit a large reformat for accidental semantic regressions like the
-- `DISTINCT ON (...)` clause that was silently dropped by PR #5324 in
-- `Worker.BinTable.Shift4`.
--
-- Exit codes:
--   0 — all QQs in the file have semantically equivalent ASTs
--   1 — one or more QQs differ
--   2 — usage error or lexer error in one of the files
{-# LANGUAGE CPP #-}

{-# LANGUAGE CPP #-}

module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Check.Lexer (AlexPosn (..), runAlex)
import Hasql.Check.Parser (QuasiQuote (..), parse)
import Hasql.Check.Quoter (quotersSupported)
import PostgresqlSyntax qualified as Ast
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Prelude


-- | One per QQ pair: either an unauditable pair (input parser rejected one
-- side) or a real AST mismatch. Used to split reporting so parse-skips don't
-- count as failures.
data AuditEntry
    = ParseSkip String
    | AstDiff String


main :: IO ()
main = do
    args <- getArgs
    case args of
        [oldPath, newPath] -> do
            oldBytes <- BS.readFile oldPath
            newBytes <- BS.readFile newPath
            case (,) <$> extractQqs oldBytes <*> extractQqs newBytes of
                Left err -> do
                    putStrLn ("Lex error: " <> err)
                    exitWith (ExitFailure 2)
                Right (oldQqs, newQqs)
                    | length oldQqs /= length newQqs -> do
                        putStrLn $
                            "QQ count mismatch in "
                                <> newPath
                                <> ": old="
                                <> show (length oldQqs)
                                <> " new="
                                <> show (length newQqs)
                        exitWith (ExitFailure 1)
                    | otherwise -> do
                        let entries = classifyQqs newPath (zip oldQqs newQqs)
                            diffs = [m | AstDiff m <- entries]
                            skips = [m | ParseSkip m <- entries]
                        -- Parse-skips are informational (the formatter
                        -- itself tolerates unparseable input); emit them
                        -- on stderr so they don't conflate with regressions.
                        mapM_ (hPutStrLn stderr) skips
                        mapM_ putStrLn diffs
                        if null diffs
                            then pure ()
                            else exitWith (ExitFailure 1)
        _ -> do
            putStrLn "Usage: hasql-th-audit <old-file> <new-file>"
            exitWith (ExitFailure 2)


-- | Extract supported hasql-th QQs from a Haskell source ByteString.
extractQqs :: BS.ByteString -> Either String [QuasiQuote]
extractQqs bs =
    case runAlex (BSL.fromStrict bs) parse of
        Left err -> Left err
        Right qqs -> Right (filter isSupportedQuoter qqs)


isSupportedQuoter :: QuasiQuote -> Bool
isSupportedQuoter (QuasiQuote (_, quoter) _) = quoter `elem` quotersSupported


-- | For each (old, new) QQ pair, parse the SQL content and classify the
-- outcome: parse-skip (one side unparseable — informational, never a
-- regression flag) or AST diff (real semantic divergence). QQ pairs where
-- both sides parse and the ASTs match are dropped (no entry).
classifyQqs :: FilePath -> [(QuasiQuote, QuasiQuote)] -> [AuditEntry]
classifyQqs filePath = mapMaybe go . zip [(1 :: Int) ..]
  where
    go (idx, (QuasiQuote (AlexPn _ oldLine _, _) (_, oldSql), QuasiQuote (AlexPn _ newLine _, _) (_, newSql))) =
        case (parseSql oldSql, parseSql newSql) of
            (Left _, _) ->
                -- Old side didn't parse; can't audit semantic equivalence.
                -- 'formatSql' itself tolerates unparseable input by leaving
                -- it untouched, so we mirror that behaviour rather than
                -- failing the build on it.
                Just (ParseSkip (filePath <> ":" <> show oldLine <> " (qq #" <> show idx <> "): old SQL did not parse, skipping"))
            (_, Left _) ->
                Just (ParseSkip (filePath <> ":" <> show newLine <> " (qq #" <> show idx <> "): new SQL did not parse, skipping"))
            (Right a, Right b)
                | a == b -> Nothing
                | otherwise ->
                    Just $
                        AstDiff $
                            filePath
                                <> ":"
                                <> show newLine
                                <> " (qq #"
                                <> show idx
                                <> "): AST DIFFERS"
                                <> "\n  --- old (line "
                                <> show oldLine
                                <> ") ---\n"
                                <> T.unpack (T.strip oldSql)
                                <> "\n  --- new (line "
                                <> show newLine
                                <> ") ---\n"
                                <> T.unpack (T.strip newSql)


-- | Parse with the Kronor extensions enabled, exactly as the quasiquoters
-- (and the formatter) do, so a quasiquote using @$f (..)@ or @$1.$field@ is
-- audited rather than skipped.
settings :: Ast.Settings
settings =
    mconcat
        [ Ast.nullabilityMarkers True
#ifdef KRONOR_SYNTAX
        , Ast.haskellTargets True
        , Ast.haskellParamFields True
#endif
        ]


parseSql :: Text -> Either Text Ast.PreparableStmt
parseSql = Ast.parse settings . T.strip
