{- | Splits a Haskell source into the tokens that matter for finding its
quasiquotes: code, comments, pragmas, string and character literals, and
the quasiquotes themselves, with their positions.

It reads only as much Haskell as that takes, but reads it as GHC does where
a mistake would hide a quote or invent one:

* comments nest, and a run of dashes is a comment only when it is not part
  of a longer operator (@-->@, @+--@, @⊕--@ are operators);
* pragmas are read to their end with their string literals, so a pragma's
  closing bracket or a quote inside a pragma's string is text;
* string literals (with escapes and gaps), MultilineStrings literals and
  character literals are text, and an identifier with a prime (@x'@, @é'@)
  does not start a character literal;
* identifiers and operators may be Unicode, so a quoter may be named
  @rø@ or with a combining mark;
* @[e|@, @[d|@, @[t|@ and @[p|@ open Template Haskell brackets, whose
  contents are Haskell, not quasiquotes.

Columns count characters, with a tab to the next multiple of eight as GHC
counts it; offsets count bytes of the UTF-8 source.
-}
module Hasql.Check.Lexer (
    AlexPosn (..),
    Token (..),
    TokenKind (..),
    tokenize,
)
where

import Data.Char (GeneralCategory (..), generalCategory, isAlpha, isAlphaNum, isAscii, isLower, isPunctuation, isSpace, isSymbol, isUpper, ord)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude


{- | A position in the source: the byte offset from its start, and the
1-based line and column. (The name is the one the Alex-generated lexer this
replaces gave it.)
-}
data AlexPosn = AlexPn !Int !Int !Int
    deriving (Eq, Ord, Show)


data Token = Token
    { tokenPosn :: AlexPosn
    , tokenText :: Text
    -- ^ The token's source text, exactly.
    , tokenKind :: TokenKind
    }
    deriving (Eq, Show)


data TokenKind
    = -- | Haskell that is none of the below.
      Code
    | -- | A line or (nested) block comment.
      Comment
    | -- | A @{-# ... #-}@ pragma.
      Pragma
    | -- | A string or MultilineStrings literal.
      StringLiteral
    | CharLiteral
    | -- | A quasiquote: its quoter, its body, and where its closing @|]@ is.
      QuasiQuoted Text Text AlexPosn
    deriving (Eq, Show)


-- | The source's tokens, in order. Their texts put together are the source.
tokenize :: Text -> Either String [Token]
tokenize src = place (AlexPn 0 1 1) <$> pieces (Text.unpack src)


data Piece = Piece String PieceKind


data PieceKind
    = PCode
    | PComment
    | PPragma
    | PString
    | PChar
    | PQuote String String


-- Positions, by walking the pieces' texts.
place :: AlexPosn -> [Piece] -> [Token]
place _ [] = []
place pos (Piece s k : rest) =
    Token pos (Text.pack s) kind : place (advance pos s) rest
  where
    kind = case k of
        PCode -> Code
        PComment -> Comment
        PPragma -> Pragma
        PString -> StringLiteral
        PChar -> CharLiteral
        PQuote name body -> QuasiQuoted (Text.pack name) (Text.pack body) (advance pos ("[" <> name <> "|" <> body))


advance :: AlexPosn -> String -> AlexPosn
advance = foldl step
  where
    step (AlexPn off line col) c = case c of
        '\n' -> AlexPn (off + 1) (line + 1) 1
        '\t' -> AlexPn (off + 1) line (col + 8 - ((col - 1) `mod` 8))
        _ -> AlexPn (off + utf8Length c) line (col + 1)
    utf8Length c
        | n < 0x80 = 1
        | n < 0x800 = 2
        | n < 0x10000 = 3
        | otherwise = 4
      where
        n = ord c


-- The lexer proper. Code characters are gathered into one piece until
-- something else starts.
pieces :: String -> Either String [Piece]
pieces = go ""
  where
    go acc s = case token s of
        Nothing -> case s of
            [] -> Right (flush acc [])
            c : r -> go (c : acc) r
        Just result -> do
            (piece, rest) <- result
            (flush acc . (piece :)) <$> go "" rest
    flush acc ps
        | null acc = ps
        | otherwise = Piece (reverse acc) PCode : ps


-- What starts here, if not a plain code character: 'Nothing' for one,
-- @Just (Left err)@ for a quasiquote left open.
token :: String -> Maybe (Either String (Piece, String))
token = \case
    s@('{' : '-' : '#' : _) -> Just (Right (first (`Piece` PPragma) (pragma (drop 3 s) "#-{")))
    '{' : '-' : r -> Just (Right (first (`Piece` PComment) (block (1 :: Int) r "-{")))
    '"' : '"' : '"' : r -> Just (Right (first (`Piece` PString) (multiline r "\"\"\"")))
    '"' : r -> Just (Right (first (`Piece` PString) (string r "\"")))
    '\'' : r | Just (lit, rest) <- charLiteral r -> Just (Right (Piece ('\'' : lit) PChar, rest))
    '[' : r
        | Just (name, afterBar) <- opener r -> Just case breakOn "|]" afterBar of
            Just (body, rest) -> Right (Piece ("[" <> name <> "|" <> body <> "|]") (PQuote name body), rest)
            Nothing -> Left "Quote not closed at end of file"
    s@(c : _)
        | isOperator c ->
            let (run, rest) = span isOperator s
             in if length run >= 2 && all (== '-') run
                    then let (comment, rest') = break (== '\n') rest in Just (Right (Piece (run <> comment) PComment, rest'))
                    else Just (Right (Piece run PCode, rest))
        | isAlpha c || c == '_' ->
            let (ident, rest) = span identChar s
             in Just (Right (Piece ident PCode, rest))
    _ -> Nothing
  where
    -- Each reader takes the text after the opener and the opener reversed,
    -- and gives the whole token and what follows. An unterminated one runs
    -- to the end of the source.
    pragma s acc = case s of
        [] -> (reverse acc, [])
        '#' : '-' : '}' : r -> (reverse ("}-#" <> acc), r)
        '"' : r -> let (str, r') = string r "\"" in pragma r' (reverse str <> acc)
        c : r -> pragma r (c : acc)
    block n s acc = case s of
        [] -> (reverse acc, [])
        '-' : '}' : r
            | n == 1 -> (reverse ("}-" <> acc), r)
            | otherwise -> block (n - 1) r ("}-" <> acc)
        '{' : '-' : r -> block (n + 1) r ("-{" <> acc)
        c : r -> block n r (c : acc)
    first f (a, b) = (f a, b)


-- A string literal's rest after its opening quote, with what was read so
-- far reversed: escapes and gaps are skipped, and a line break or the end
-- of the source ends an unterminated one.
string :: String -> String -> (String, String)
string s acc = case s of
    [] -> (reverse acc, [])
    '"' : r -> (reverse ('"' : acc), r)
    '\\' : c : r
        | isSpace c ->
            let (ws, rest) = span isSpace r
             in case rest of
                    '\\' : rest' -> string rest' ('\\' : reverse ws <> (c : '\\' : acc))
                    _ -> string rest (reverse ws <> (c : '\\' : acc))
        | otherwise -> string r (c : '\\' : acc)
    '\n' : _ -> (reverse acc, s)
    c : r -> string r (c : acc)


-- A MultilineStrings literal's rest after its opening @"""@.
multiline :: String -> String -> (String, String)
multiline s acc = case s of
    [] -> (reverse acc, [])
    '"' : '"' : '"' : r -> (reverse ("\"\"\"" <> acc), r)
    '\\' : c : r -> multiline r (c : '\\' : acc)
    c : r -> multiline r (c : acc)


-- A character literal's rest after its opening quote; 'Nothing' when the
-- quote is a Template Haskell name quote (@'f@, @''T@) instead.
charLiteral :: String -> Maybe (String, String)
charLiteral = \case
    '\\' : c : r -> case break (== '\'') (take 10 r) of
        (more, '\'' : _) | '\n' `notElem` more -> Just ('\\' : c : more <> "'", drop (length more + 1) r)
        _ -> Nothing
    c : '\'' : r | c /= '\n' -> Just ([c, '\''], r)
    _ -> Nothing


-- A quasiquote's quoter after its @[@, through the @|@: an optionally
-- qualified variable name, not one of Template Haskell's bracket letters.
opener :: String -> Maybe (String, String)
opener = go ""
  where
    go acc r = case r of
        c : _
            | isUpper c ->
                let (m, rest) = span identChar r
                 in case rest of
                        '.' : rest' -> go (acc <> m <> ".") rest'
                        _ -> Nothing
            | isLower c || c == '_' ->
                let (v, rest) = span identChar r
                    name = acc <> v
                 in case rest of
                        '|' : rest' | name `notElem` ["e", "d", "t", "p"] -> Just (name, rest')
                        _ -> Nothing
        _ -> Nothing


breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go ""
  where
    go acc r
        | needle `isPrefixOf` r = Just (reverse acc, drop (length needle) r)
        | otherwise = case r of
            [] -> Nothing
            c : r' -> go (c : acc) r'


identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_' || c == '\'' || generalCategory c == NonSpacingMark


-- | An operator character, as GHC reads one: an ASCII symbol, or any other
-- Unicode symbol or punctuation that is not one of Haskell's special
-- characters.
isOperator :: Char -> Bool
isOperator c
    | isAscii c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
    | otherwise = isSymbol c || (isPunctuation c && c `notElem` ("(),;[]`{}_\"'" :: String))
