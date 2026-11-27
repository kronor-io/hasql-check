-- | The quasiquotes of a Haskell source, found by "Hasql.Check.Lexer".
module Hasql.Check.Parser (
    QuasiQuote (..),
    QuasiQuotes,
    parseQuasiQuotes,
    parseFile,
)
where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Hasql.Check.Lexer (AlexPosn, Token (..), TokenKind (..), tokenize)
import System.Directory (doesFileExist)
import Prelude


type QuasiQuotes = [QuasiQuote]


{- | A quasiquote: where its @[@ is and its quoter, then where its closing
@|]@ is and its body.
-}
data QuasiQuote
    = QuasiQuote (AlexPosn, Text) (AlexPosn, Text)
    deriving (Eq, Show)


-- | The quasiquotes in a Haskell source, in the order they appear.
parseQuasiQuotes :: ByteString -> Either String QuasiQuotes
parseQuasiQuotes s = do
    text <- either (\e -> Left ("Malformed utf8 source: " <> show e)) Right (Text.Encoding.decodeUtf8' (ByteString.Lazy.toStrict s))
    tokens <- tokenize text
    pure [QuasiQuote (tokenPosn t, quoter) (close, body) | t@Token{tokenKind = QuasiQuoted quoter body close} <- tokens]


parseFile :: FilePath -> IO (Either String QuasiQuotes)
parseFile fn = do
    flag <- doesFileExist fn
    if not flag
        then pure (Left ("The following file does not exist : " ++ fn))
        else parseQuasiQuotes <$> ByteString.Lazy.readFile fn
