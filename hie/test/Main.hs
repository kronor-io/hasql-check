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


main :: IO ()
main = do
    failures <-
        traverse
            ( \(name, source, start, end, expected) -> do
                let found = quoteAt source start end
                unless (found == expected) $
                    putStrLn (name <> ": expected " <> show expected <> ", found " <> show found)
                pure (found /= expected)
            )
            cases
    if or failures then exitFailure else putStrLn (show (length cases) <> " passed")
