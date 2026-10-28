{
{-# LANGUAGE NoStrictData #-}

module Hasql.Check.Parser where

import Prelude
import Data.Text (Text)
import Hasql.Check.Lexer
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import System.Directory
}

%expect 0
%name parse
%monad { Alex } { >>= } { return }
%lexer { lexer } { Lexeme _ Eof }
%tokentype { Lexeme }
%error { parseError }

%token

  quoter { Lexeme _ (Quoter _) }
  quote  { Lexeme _ (Quote _)  }

%%

quasiquotes : quasiquotes quasiquote    { $2 : $1 }
            | {- empty -}               { [] }

quasiquote  : quoter quote              { mkQuasiQuote $1 $2 }

{

type QuasiQuotes = [QuasiQuote]

data QuasiQuote =
  QuasiQuote (AlexPosn, Text) (AlexPosn, Text)
  deriving (Eq, Show)

mkQuasiQuote :: Lexeme -> Lexeme -> QuasiQuote
mkQuasiQuote (Lexeme posna (Quoter q))
  (Lexeme posnb (Quote qq))
  = QuasiQuote (posna, q) (posnb, qq)
mkQuasiQuote _ _ = error "mkQuasiQuote: Impossible"

parseError :: Lexeme -> Alex a
parseError (Lexeme (AlexPn _ line col) lc) = alexError ("Parsing failed at line: " <> show line <> ", col: " <> show col <> " " <> show lc)

-- | The quasiquotes in a Haskell source, in the order they appear.
parseQuasiQuotes :: ByteString -> Either String QuasiQuotes
parseQuasiQuotes s = reverse <$> runAlex s parse

parseFile :: FilePath -> IO (Either String QuasiQuotes)
parseFile fn = do
  flag <- doesFileExist fn
  if not flag
    then pure $ Left ("The following file does not exist : " ++ fn)
    else parseQuasiQuotes <$> ByteString.Lazy.readFile fn
}
