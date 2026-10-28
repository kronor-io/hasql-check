
{

{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Hasql.Check.Lexer where

import           Prelude
import           Control.Monad (when)
import           Data.Maybe (isJust, fromJust)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as ByteString.Builder
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import           System.Directory

}

%wrapper "monadUserState-bytestring"

$digit      = 0-9                                            -- digits
$small      = [a-z_]
$large      = [A-Z]
$alpha      = [A-Za-z]
$idchar     = [$alpha $digit _ \']                           -- identifier character
$symbol     = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]     -- ascii operator symbols
$pipe       = \|
$rbrack     = \]
$lbrack     = \[

@varid      = $small $idchar*
@modid      = $large $idchar*
-- GHC accepts a qualified quoter, [Hasql.TH.vectorStatement| ... |], as well
-- as a bare one.
@quoter     = (@modid \.)* @varid
@beginquote = $lbrack @quoter $pipe
@endquote   = $pipe $rbrack
-- A string gap: a backslash, whitespace including newlines, a backslash.
@gap        = \\ $white+ \\
@charlit    = \' ( [^\\ \' \n] | \\ [^\n] [^\' \n]* ) \'

tokens :-
<0>                        @beginquote   { enterQuote `andBegin` state_quote }
<0>                        \n            { skip }
-- Identifiers are consumed whole so the prime in x' is never read as the start
-- of a character literal.
<0>                        [$alpha \_] $idchar*  { skip }
-- Literals are skipped so that "{-", "--" or "[x|" inside one cannot open a
-- comment or a quote and hide the quotes that follow it.
<0>                        @charlit      { skip }
<0>                        \"\"\"        { skip `andBegin` state_multiline_string }
<0>                        \"            { skip `andBegin` state_string }
<state_string>             @gap          { skip }
<state_string>             \\ .          { skip }
<state_string>             \"            { skip `andBegin` state_initial }
<state_string>             \n            { skip }
<state_string>             .             { skip }
<state_multiline_string>   \\ .          { skip }
<state_multiline_string>   \"\"\"        { skip `andBegin` state_initial }
<state_multiline_string>   \n            { skip }
<state_multiline_string>   .             { skip }
-- A run of dashes followed by a symbol is an operator such as --> rather than
-- a comment.
<0>                        "--" \-* / ~$symbol { skip `andBegin` state_line_comment }
<state_line_comment>       \n            { skip `andBegin` state_initial }
<state_line_comment>       .             { skip }
<0>                        "{-"          { enterNewComment `andBegin` state_multi_comment }
<0>                        .             { skip }
<state_multi_comment>      "{-"          { embedComment }
<state_multi_comment>      "-}"          { unembedComment }
<state_multi_comment>      \n            { skip }
<state_multi_comment>      .             { skip }
<state_quote>              \n            { addCurrentToQuote }
<state_quote>              .             { addCurrentToQuote }
<state_quote>              @endquote     { leaveQuote `andBegin` state_initial }

{
data Lexeme = Lexeme AlexPosn LexemeClass
    deriving (Show, Eq)

data LexemeClass =
    Eof
    | Quoter Text
    | Quote Text
    deriving (Show, Eq)

mkLexemeClass :: LexemeClass -> AlexInput -> Int64 -> Alex Lexeme
mkLexemeClass c (p, _, _, _) _ = return (Lexeme p c)

-- states
state_initial :: Int
state_initial = 0

-- user state type

data AlexUserState = AlexUserState
    { lexerCommentDepth :: Int
    , lexerQuoterState :: Bool
    , lexerQuoterValue :: Builder
    , lexerQuoteState :: Bool
    , lexerQuoteValue :: Builder
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { lexerCommentDepth = 0
    , lexerQuoterState = False
    , lexerQuoterValue = mempty
    , lexerQuoteState = False
    , lexerQuoteValue = mempty
    }

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth depth = Alex $ \s -> Right (s{alex_ust = (alex_ust s){lexerCommentDepth = depth}}, ())

enterNewComment :: AlexInput -> Int64 -> Alex Lexeme
enterNewComment input len = do
    setLexerCommentDepth 1
    skip input len

embedComment :: AlexInput -> Int64 -> Alex Lexeme
embedComment input len = do
    d <- getLexerCommentDepth
    setLexerCommentDepth (d + 1)
    skip input len

unembedComment :: AlexInput -> Int64 -> Alex Lexeme
unembedComment input len = do
    d <- getLexerCommentDepth
    setLexerCommentDepth (d - 1)
    if d == 1
    then do
        (skip `andBegin` state_initial) input len
    else skip input len

getLexerQuoteState :: Alex Bool
getLexerQuoteState = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, lexerQuoteState ust)

getLexerQuoterState :: Alex Bool
getLexerQuoterState = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, lexerQuoterState ust)

setLexerQuoteState :: Bool -> Alex ()
setLexerQuoteState quoteState =
    Alex $ \s -> Right (s{alex_ust = (alex_ust s){lexerQuoteState = quoteState}}, ())

setLexerQuoterState :: Bool -> Alex ()
setLexerQuoterState quoterState =
    Alex $ \s -> Right (s{alex_ust = (alex_ust s){lexerQuoterState = quoterState}}, ())

getLexerQuoteValue :: Alex Builder
getLexerQuoteValue = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, lexerQuoteValue ust)

setLexerQuoteValue :: Builder -> Alex ()
setLexerQuoteValue builder = Alex $ \s -> Right (s{alex_ust = (alex_ust s){lexerQuoteValue = builder}}, ())

getLexerQuoterValue :: Alex Builder
getLexerQuoterValue = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, lexerQuoterValue ust)

setLexerQuoterValue :: Builder -> Alex ()
setLexerQuoterValue builder = Alex $ \s -> Right (s{alex_ust = (alex_ust s){lexerQuoterValue = builder}}, ())

addCurrentToQuote :: AlexInput -> Int64 -> Alex Lexeme
addCurrentToQuote (_, _, bytestring, _) len = do
    builder <- getLexerQuoteValue
    setLexerQuoteValue (builder <> ByteString.Builder.lazyByteString (ByteString.Lazy.take len bytestring))
    alexMonadScan

enterQuote :: AlexInput -> Int64 -> Alex Lexeme
enterQuote input len = do
    setLexerQuoteState True
    setLexerQuoteValue mempty
    addQuoter input len

leaveQuote :: AlexInput -> Int64 -> Alex Lexeme
leaveQuote (posn, _, _, _) _ = do
    builder <- getLexerQuoteValue
    setLexerQuoteState False
    setLexerQuoterState False
    case Text.Encoding.decodeUtf8' (ByteString.Lazy.toStrict (ByteString.Builder.toLazyByteString builder)) of
        Left e -> lexerError $ "Malformed utf8 string: " <> show e
        Right string -> pure $ Lexeme posn (Quote string)

addQuoter :: AlexInput -> Int64 -> Alex Lexeme
addQuoter (posn, _, bytestring, _) len = do
    let bsVariable = ByteString.Lazy.dropEnd 1 (ByteString.Lazy.drop 1 (ByteString.Lazy.take len bytestring))
    case Text.Encoding.decodeUtf8' (ByteString.Lazy.toStrict bsVariable) of
        Right var -> do
            pure $ Lexeme posn (Quoter var)
        Left e -> lexerError $ "Malformed utf8 representation of quoter, expecting unicode: " <> show e

getPosn :: Alex AlexPosn
getPosn = Alex $ \s@AlexState{alex_pos = posn} -> Right (s, posn)

alexEOF :: Alex Lexeme
alexEOF = flip Lexeme Eof <$> getPosn

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

-- the error message doesn't do a utf8 decoding of the input.
lexerError :: String -> Alex a
lexerError msg =
    do (p, c, inp, _) <- alexGetInput
       let inp1 = ByteString.Lazy.Char8.filter (/= '\r') (ByteString.Lazy.Char8.takeWhile (/='\n') inp)
       let inp2 = if (ByteString.Lazy.length inp1 > 30)
                     then trimBSL (ByteString.Lazy.take 30 inp1)
                     else trimBSL inp1
       let disp = if (ByteString.Lazy.null inp)
                     then " at end of file"
                     else if (ByteString.Lazy.null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ ByteString.Lazy.Char8.unpack inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    trimBSL = ByteString.Lazy.Char8.takeWhile (/= ' ') . ByteString.Lazy.Char8.dropWhile (== ' ')

-- Execution

scanner :: ByteString -> Either String [Lexeme]
scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
                            when (isJust m) (lexerError (fromJust m))
                            let tok@(Lexeme _ cl) = t
                            if (cl == Eof)
                               then do f1 <- getLexerQuoterState
                                       d2 <- getLexerQuoteState
                                       if ((not f1) && (not d2))
                                          then return [tok]
                                          else alexError "Quote not closed at end of file"
                               else do toks <- loop
                                       return (tok : toks)
              in  runAlex str loop

-- we capture the error message in order to complement it with the file position
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex $ \s ->
    case al s of
        Right (s', x) -> Right (s', (x, Nothing))
        Left  message -> Right (s, (undefined, Just message))


lexer :: (Lexeme -> Alex a) -> Alex a
lexer f = do
  (t, m) <- alexComplementError alexMonadScan
  when (isJust m) (lexerError (fromJust m))
  let tok@(Lexeme _ cl) = t
  if (cl == Eof)
     then do
       f1 <- getLexerQuoterState
       d2 <- getLexerQuoteState
       if ((not f1) && (not d2))
          then f tok
          else alexError "Quote not closed at end of file"
     else f tok

lexFile :: FilePath -> IO (Either String [Lexeme])
lexFile fn = do
  flag <- doesFileExist fn
  if not flag
    then pure $ Left ("The following file does not exist : " ++ fn)
    else do
      putStrLn ("Beginning analysis of the hasql-th quotes in file " ++ fn)
      s <- ByteString.Lazy.readFile fn
      pure (reverse <$> scanner s)

}
