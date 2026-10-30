module Hasql.ThFormat (
    processFile,
    processBytes,
    ProcessResult (..),
)
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (lefts, rights)
import Data.List (intercalate, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hasql.Check.Lexer (AlexPosn (..), runAlex)
import Hasql.Check.Parser (QuasiQuote (..), parse)
import Hasql.Check.Quoter (quotersSupported)
import Hasql.ThFormat.SqlFormat (formatSql)
import Prelude


data ProcessResult
    = Unchanged
    | Changed
    | Error String
    deriving (Show, Eq)


-- | Format a ByteString in memory, returning the result. The Alex lexer
-- requires a lazy ByteString so we convert at this single boundary; the rest
-- of the pipeline works on strict bytes. Returns 'Left' with a
-- newline-separated list of per-QQ errors if the formatter ever emitted
-- AST-divergent SQL; the bytes are left untouched in that case.
processBytes :: ByteString -> Either String ByteString
processBytes original =
    case runAlex (BSL.fromStrict original) parse of
        Left err -> Left err
        Right qqs ->
            let supported = reverse (filter isSupportedQuoter qqs)
                results = map makeReplacement supported
                errors = lefts results
                replacements = rights results
             in if null errors
                    then Right (applyReplacements original (sortReplacements replacements))
                    else Left (intercalate "\n" errors)


processFile :: Bool -> FilePath -> IO ProcessResult
processFile checkOnly filePath = do
    original <- BS.readFile filePath
    case processBytes original of
        Left err -> pure (Error err)
        Right modified
            | original == modified -> pure Unchanged
            | checkOnly -> pure Changed
            | otherwise -> do
                BS.writeFile filePath modified
                pure Changed


isSupportedQuoter :: QuasiQuote -> Bool
isSupportedQuoter (QuasiQuote (_, quoter) _) = quoter `elem` quotersSupported


data Replacement = Replacement
    { rStartOffset :: Int
    , rEndOffset :: Int
    , rFormatted :: Text
    }
    deriving (Show)


makeReplacement :: QuasiQuote -> Either String Replacement
makeReplacement (QuasiQuote (AlexPn quoterOffset quoterLine quoterCol, quoterName) (AlexPn endOffset _ _, sqlContent)) =
    let startOffset = quoterOffset + 1 + T.length quoterName + 1
        -- quoterCol is 1-based column of '[', so the base indent for SQL is (quoterCol - 1) spaces
        qqIndent = quoterCol - 1
     in case formatSql qqIndent sqlContent of
            Right formatted ->
                Right
                    Replacement
                        { rStartOffset = startOffset
                        , rEndOffset = endOffset
                        , rFormatted = formatted
                        }
            Left reason -> Left ("line " <> show quoterLine <> ": " <> T.unpack reason)


sortReplacements :: [Replacement] -> [Replacement]
sortReplacements = sortBy (comparing (Down . rStartOffset))


applyReplacements :: ByteString -> [Replacement] -> ByteString
applyReplacements = foldl applyOne
  where
    applyOne :: ByteString -> Replacement -> ByteString
    applyOne bs Replacement{rStartOffset, rEndOffset, rFormatted} =
        let before = BS.take rStartOffset bs
            after = BS.drop rEndOffset bs
            formattedBS = TE.encodeUtf8 rFormatted
         in before <> formattedBS <> after
