{- |
Module      : Hasql.Check.Ghc.Source
Copyright   : (c) 2026 Kronor.io

License     : BSD-3-Clause
Maintainer  : lorenzo@kronor.io
Stability   : experimental
Portability : GHC

A statement of "Hasql.Check.Inferred.TH", from what GHC knows of it: its
quote, found in its source from the span GHC gives the code it generated,
and its types, from the dictionaries that code is passed. Whether GHC's
types come from a @.hie@ file or a compiler plugin, they are read here.

GHC counts columns from 1, and a tab as reaching the next tab stop, every 8
columns; the source text counts characters.
-}
module Hasql.Check.Ghc.Source (
    -- * Statements
    Statement (..),
    Column (..),
    statementAt,
    statementOf,
    typeName,

    -- * Quotes
    Quote (..),
    quoteAt,
    Types (..),
    typesOf,
    Ty (..),
    ground,
    columnTypes,
    symbols,
    labelLists,
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

{- | The quote a span covers, given the source's lines and the span's start
and end, as GHC's line and column. The span starts right after
@[quoter|@ and ends right after @|]@; any other span is not a quote's.
-}
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
    | {- | Its types are only known where it is used, and may differ from one
      use to the next.
      -}
      Unresolved
    | -- | Its parameter type, and its row type unless it returns no rows.
      Resolved t (Maybe t)
    deriving stock (Eq, Show)

{- | The statement's types, given which types are concrete, and the types of
the quote's Params and Row dictionaries.
-}
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

{- | A type: a constructor applied to its visible arguments, by module and
name; a string literal, which names a Postgres type or label; another
literal; or something not concrete.
-}
data Ty = Con String String [Ty] | Lit String | OtherLit String | Var
    deriving stock (Eq, Show)

-- | Whether a type is concrete.
ground :: Ty -> Bool
ground = \case
    Var -> False
    Lit _ -> True
    OtherLit _ -> True
    Con _ _ args -> all ground args

{- | A parameter or row type's columns, each with whether it is a 'Maybe':
a tuple's elements, none for unit as parameters, or the type itself.
-}
columnTypes :: Bool -> Ty -> [(Ty, Bool)]
columnTypes isParams t = map column (split t)
  where
    split = \case
        Con m "Unit" [] | isParams, m `elem` tupleModules -> []
        Con m name args | m `elem` tupleModules, isTuple name -> args
        other -> [other]
    column = columnOf

-- | A column's type and whether it is a 'Maybe', so may be NULL.
columnOf :: Ty -> (Ty, Bool)
columnOf = \case
    Con m "Maybe" [inner] | m `elem` maybeModules -> (inner, True)
    other -> (other, False)

{- | Where GHC defines tuples and unit, and Maybe: a type of the same name
elsewhere, a newtype TupleId say, is a type of its own.
-}
tupleModules, maybeModules :: [String]
tupleModules = ["GHC.Tuple", "GHC.Tuple.Prim"]
maybeModules = ["GHC.Internal.Maybe", "GHC.Maybe"]

isTuple :: String -> Bool
isTuple name = case splitAt 5 name of
    ("Tuple", n) -> not (null n) && all isDigit n
    _ -> False

-- * Statements

-- | A statement, with what the checker needs of it.
data Statement = Statement
    { line :: Int
    , quote :: Quote
    , params :: [Column]
    , row :: Maybe [Column]
    -- ^ Nothing for a statement that returns no rows.
    }

-- | A parameter or result column.
data Column = Column
    { haskellType :: String
    , postgres :: String
    , labels :: [String]
    -- ^ The enum's labels, if it is one.
    , nullable :: Bool
    }

{- | The statement whose generated code has the given span, given the source
lines and the types of the dictionaries the code is passed there: nothing
if the span is not a quote of these quoters, an error if its types are not
concrete there.
-}
statementAt :: [String] -> (Int, Int) -> (Int, Int) -> [Ty] -> Maybe (Either String Statement)
statementAt source start@(line', _) end here = do
    q <- quoteAt source start end
    statementOf line' q here

{- | The statement of a quote on a line, given the types of the dictionaries
its generated code is passed: nothing if it is not a quote of these
quoters, an error if its types are not concrete there.
-}
statementOf :: Int -> Quote -> [Ty] -> Maybe (Either String Statement)
statementOf line' q here = case first paramHs of
    -- The quoters build the encoder and the decoder themselves, so the types
    -- come one per placeholder and per column rather than as a tuple to
    -- split: a record target and a param field are neither.
    Just hs
        | Just names <- first paramNames
        , Just labels' <- first paramLabels ->
            Just (Right (Statement line' q (columnsOf (map columnOf hs) names labels') resultRow))
        | otherwise -> Just (Left unresolved)
    Nothing -> case typesOf ground params' rows of
        NotInferred -> Nothing
        Resolved p r
            | Just names <- first paramNames
            , Just labels' <- first paramLabels
            , Just row' <- traverse (\r' -> (r',,) <$> first columnNames <*> first columnLabels) r ->
                Just (Right (Statement line' q (columns True (p, names, labels')) (columns False <$> row')))
        _ -> Just (Left unresolved)
  where
    unresolved =
        "the statement's types are not known where it is written, so it cannot be checked; give its binding a concrete type, such as Statement Int64 Text"
    -- A statement that returns rows carries either the column types the
    -- quoter collected, or, where it could not count them, the Haskell row
    -- type's. One that returns none carries neither.
    resultRow = case (first columnNames, first columnLabels) of
        (Just names, Just labels') -> case (first columnHs, filter ground rows) of
            (Just hs, _) -> Just (columnsOf (map columnOf hs) names labels')
            (Nothing, r : _) -> Just (columnsOf (columnTypes False r) names labels')
            _ -> Nothing
        _ -> Nothing
    params' = [p | Con "Hasql.Check.Inferred" "Params" [p] <- here]
    rows = [r | Con "Hasql.Check.Inferred" "Row" [r] <- here]
    paramHs = [hs | Con "Hasql.Check.Inferred" "HaskellParams" [l] <- here, Just hs <- [types l], all ground hs]
    columnHs = [hs | Con "Hasql.Check.Inferred" "HaskellColumns" [l] <- here, Just hs <- [types l], all ground hs]
    paramNames = [ns | Con "Hasql.Check.Inferred" "PostgresParams" [l] <- here, Just ns <- [symbols l]]
    columnNames = [ns | Con "Hasql.Check.Inferred" "PostgresColumns" [l] <- here, Just ns <- [symbols l]]
    paramLabels = [ls | Con "Hasql.Check.Inferred" "PostgresParamLabels" [l] <- here, Just ls <- [labelLists l]]
    columnLabels = [ls | Con "Hasql.Check.Inferred" "PostgresColumnLabels" [l] <- here, Just ls <- [labelLists l]]
    first = \case
        x : _ -> Just x
        [] -> Nothing
    columns isParams (t, names, labels') = columnsOf (columnTypes isParams t) names labels'
    columnsOf cs names labels' =
        zipWith3 (\(c, nullable') postgres' ls -> Column (typeName c) postgres' ls nullable') cs names labels'

-- | A type as Haskell would write it, qualified.
typeName :: Ty -> String
typeName = \case
    Con m n args -> unwords ((if null m then n else m <> "." <> n) : map (parenthesised . typeName) args)
    Lit s -> show s
    OtherLit s -> s
    Var -> "?"
  where
    parenthesised name = if ' ' `elem` name then concat ["(", name, ")"] else name

{- | A type-level list of strings. The quoters build theirs by appending,
and GHC records the Append unreduced, so it is evaluated here.
-}
symbols :: Ty -> Maybe [String]
symbols = \case
    Con _ ":" [Lit s, rest] -> (s :) <$> symbols rest
    Con _ "[]" [] -> Just []
    Con _ "Append" [a, b] -> (<>) <$> symbols a <*> symbols b
    _ -> Nothing

{- | A type-level list of types, as 'Slots' and 'Target' carry: one per
placeholder or result column, which a record target or a param field makes
something other than a tuple's elements.
-}
types :: Ty -> Maybe [Ty]
types = \case
    Con _ ":" [t, rest] -> (t :) <$> types rest
    Con _ "[]" [] -> Just []
    Con _ "Append" [a, b] -> (<>) <$> types a <*> types b
    _ -> Nothing

-- | A type-level list of lists of strings.
labelLists :: Ty -> Maybe [[String]]
labelLists = \case
    Con _ ":" [l, rest] -> (:) <$> symbols l <*> labelLists rest
    Con _ "[]" [] -> Just []
    Con _ "Append" [a, b] -> (<>) <$> labelLists a <*> labelLists b
    _ -> Nothing
