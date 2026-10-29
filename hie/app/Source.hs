-- |
-- Module      : Source
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Find a quote in its source, from the span GHC gives its generated code.
--
-- GHC counts columns from 1, and a tab as reaching the next tab stop, every 8
-- columns; the source text counts characters.
module Source (
    Quote (..),
    quoteAt,
    Types (..),
    typesOf,
    Ty (..),
    ground,
    columnTypes,
    charIndex,
    ghcColumn,
) where

import Data.Char (isAlphaNum, isDigit)
import Data.List (intercalate)


data Quote = Quote
    { column :: Int
    -- ^ GHC's column of the quote's opening bracket.
    , quoter :: String
    , body :: String
    }
    deriving stock (Eq, Show)


-- | The quote a span covers, given the source's lines and the span's start
-- and end, as GHC's line and column. The span starts right after
-- @[quoter|@ and ends right after @|]@; any other span is not a quote's.
quoteAt :: [String] -> (Int, Int) -> (Int, Int) -> Maybe Quote
quoteAt source (startLine, startColumn) (endLine, endColumn) = do
    first <- line startLine
    final <- line endLine
    let start = charIndex first startColumn
        end = charIndex final endColumn - 2
        opening = take (start - 1) first
        bracket = lastIndex '[' opening
        name = drop (bracket + 1) opening
    if start >= 1
        && take 1 (drop (start - 1) first) == "|"
        && bracket >= 0
        && not (null name)
        && all (\c -> isAlphaNum c || c `elem` ("._'" :: String)) name
        then
            Just
                Quote
                    { column = ghcColumn first bracket
                    , quoter = name
                    , body =
                        if startLine == endLine
                            then take (end - start) (drop start first)
                            else
                                intercalate "\n" $
                                    [drop start first]
                                        <> [l | n <- [startLine + 1 .. endLine - 1], Just l <- [line n]]
                                        <> [take end final]
                    }
        else Nothing
  where
    line n
        | n >= 1, l : _ <- drop (n - 1) source = Just l
        | otherwise = Nothing
    lastIndex c s = case [i | (i, x) <- zip [0 ..] s, x == c] of
        [] -> -1
        is -> last is


-- | What the dictionaries in a quote's span say about its statement's types.
data Types t
    = -- | Not a statement of Hasql.Check.Inferred.TH.
      NotInferred
    | -- | Its types are only known where it is used, and may differ from one
      -- use to the next.
      Unresolved
    | -- | Its parameter type, and its row type unless it returns no rows.
      Resolved t (Maybe t)
    deriving stock (Eq, Show)


-- | The statement's types, given which types are concrete, and the types of
-- the quote's Params and Row dictionaries.
typesOf :: (t -> Bool) -> [t] -> [t] -> Types t
typesOf concrete params rows = case (filter concrete params, filter concrete rows) of
    _ | null params -> NotInferred
    ([], _) -> Unresolved
    (_, []) | not (null rows) -> Unresolved
    (p : _, r) -> Resolved p (case r of x : _ -> Just x; [] -> Nothing)


-- | The index in a line of the character at a GHC column.
charIndex :: String -> Int -> Int
charIndex = go 0 1
  where
    go i c rest target
        | c >= target = i
        | x : xs <- rest = go (i + 1) (advance c x) xs target
        | otherwise = i


-- | GHC's column of the character at an index in a line.
ghcColumn :: String -> Int -> Int
ghcColumn s i = foldl advance 1 (take i s)


advance :: Int -> Char -> Int
advance c = \case
    '\t' -> ((c - 1) `div` 8 + 1) * 8 + 1
    _ -> c + 1


-- | A type: a constructor applied to its visible arguments, by module and
-- name; a string literal, which names a Postgres type or label; another
-- literal; or something not concrete.
data Ty = Con String String [Ty] | Lit String | OtherLit String | Var
    deriving stock (Eq, Show)


-- | Whether a type is concrete.
ground :: Ty -> Bool
ground = \case
    Var -> False
    Lit _ -> True
    OtherLit _ -> True
    Con _ _ args -> all ground args


-- | A parameter or row type's columns, each with whether it is a 'Maybe':
-- a tuple's elements, none for unit as parameters, or the type itself.
columnTypes :: Bool -> Ty -> [(Ty, Bool)]
columnTypes isParams t = map column (split t)
  where
    split = \case
        Con m "Unit" [] | isParams, m `elem` tupleModules -> []
        Con m name args | m `elem` tupleModules, isTuple name -> args
        other -> [other]
    column = \case
        Con m "Maybe" [inner] | m `elem` maybeModules -> (inner, True)
        other -> (other, False)


-- | Where GHC defines tuples and unit, and Maybe: a type of the same name
-- elsewhere, a newtype TupleId say, is a type of its own.
tupleModules, maybeModules :: [String]
tupleModules = ["GHC.Tuple", "GHC.Tuple.Prim"]
maybeModules = ["GHC.Internal.Maybe", "GHC.Maybe"]


isTuple :: String -> Bool
isTuple name = case splitAt 5 name of
    ("Tuple", n) -> not (null n) && all isDigit n
    _ -> False
