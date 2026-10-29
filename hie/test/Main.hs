module Main where

import Control.Monad (unless)
import Source
import System.Exit (exitFailure)


-- | Each case: the source, the span GHC gives the quote's generated code, and
-- the quote. The columns are worked out by hand from GHC's rule that a tab
-- reaches the next of columns 9, 17, 25, ...
--
-- In "t =\t[singletonStatement| select 1 |]" the tab reaches column 9: '['
-- is at 9, the quoter at 10-27, '|' at 28, the body at 29-38, "|]" at 39-40.
-- In "\tb|]", 'b' is at 9 and "|]" at 10-11.
cases :: [(String, [String], (Int, Int), (Int, Int), Maybe Quote)]
cases =
    [ ("no tabs", ["f = [q|x|]"], (1, 8), (1, 11), Just (Quote 5 "q" "x"))
    , ("a tab before the quote", ["t =\t[singletonStatement| select 1 |]"], (1, 29), (1, 41), Just (Quote 9 "singletonStatement" " select 1 "))
    , ("a tab inside a multiline quote", ["f = [q|a", "\tb|]"], (1, 8), (2, 12), Just (Quote 5 "q" "a\n\tb"))
    , ("a span that is not a quote's", ["f = g x"], (1, 5), (1, 8), Nothing)
    ]


-- | The types of a quote, from its dictionaries' types; here a type is
-- concrete unless it is "?".
typeCases :: [(String, [String], [String], Types String)]
typeCases =
    [ ("concrete", ["Int64"], ["Text"], Resolved "Int64" (Just "Text"))
    , ("no rows", ["Int64"], [], Resolved "Int64" Nothing)
    , ("a polymorphic statement", ["?"], ["?"], Unresolved)
    , ("a polymorphic row", ["Int64"], ["?"], Unresolved)
    , ("not a statement of these quoters", [], [], NotInferred)
    ]


-- | A row type's columns: the oracle is the type itself.
columnCases :: [(String, Ty, [(Ty, Bool)])]
columnCases =
    [ ("a tuple", Con "GHC.Tuple" "Tuple2" [int64, maybe' text], [(int64, False), (text, True)])
    , ("a newtype named like a tuple", tupleId, [(tupleId, False)])
    , ("a type with a number in it", amount, [(amount, False)])
    , ("a Maybe of another module", otherMaybe, [(otherMaybe, False)])
    ]
  where
    int64 = Con "GHC.Internal.Int" "Int64" []
    text = Con "Data.Text.Internal" "Text" []
    maybe' a = Con "GHC.Internal.Maybe" "Maybe" [a]
    tupleId = Con "Shop" "TupleId" []
    amount = Con "Shop" "Amount" [OtherLit "2"]
    otherMaybe = Con "Shop" "Maybe" [int64]


main :: IO ()
main = do
    columnFailures <-
        traverse
            ( \(name, t, expected) -> do
                let found = columnTypes False t
                unless (found == expected) $
                    putStrLn (name <> ": expected " <> show expected <> ", found " <> show found)
                pure (found /= expected)
            )
            columnCases
    unless (ground (Con "Shop" "Amount" [OtherLit "2"])) $ putStrLn "a type with a number in it is not concrete"
    typeFailures <-
        traverse
            ( \(name, params, rows, expected) -> do
                let found = typesOf (/= "?") params rows
                unless (found == expected) $
                    putStrLn (name <> ": expected " <> show expected <> ", found " <> show found)
                pure (found /= expected)
            )
            typeCases
    failures <-
        traverse
            ( \(name, source, start, end, expected) -> do
                let found = quoteAt source start end
                unless (found == expected) $
                    putStrLn (name <> ": expected " <> show expected <> ", found " <> show found)
                pure (found /= expected)
            )
            cases
    if or (failures <> typeFailures <> columnFailures) || not (ground (Con "Shop" "Amount" [OtherLit "2"]))
        then exitFailure
        else putStrLn (show (length cases + length typeCases + length columnCases + 1) <> " passed")
