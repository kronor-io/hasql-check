-- |
-- Module      : Hasql.Check.Finding
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Warnings about a statement, and the two ways of suppressing them.
--
-- A directive, in the comment lines directly above the line a quote starts on,
-- suppresses findings about that quote's statement:
--
-- > -- hasql-check: allow nullable(street, city), narrowing($4)
-- > [singletonStatement| ... |]
--
-- A baseline file lists findings to ignore wherever their statement is, so
-- that existing findings can be left while new ones fail CI.
module Hasql.Check.Finding (
    -- * Findings
    Rule (..),
    ruleName,
    Finding (..),

    -- * Directives
    Allowance,
    directivesAbove,
    applyDirectives,

    -- * Baselines
    Baseline,
    BaselineEntry (..),
    statementHash,
    readBaseline,
    renderBaseline,
    inBaseline,

    -- * Both
    settle,
) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isSpace)
import Data.List (partition, sortOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Numeric (showHex)
import Prelude


-- * Findings


data Rule
    = -- | A result column annotated non-null that can be null.
      Nullable
    | -- | An annotation that converts a value to a narrower type.
      Narrowing
    deriving stock (Eq, Ord, Show, Enum, Bounded)


ruleName :: Rule -> Text
ruleName = \case
    Nullable -> "nullable"
    Narrowing -> "narrowing"


data Finding = Finding
    { rule :: Rule
    , subject :: Text
    -- ^ The result column's name, or the parameter as @$n@.
    , message :: String
    }


-- * Directives


-- | A rule allowed for a subject.
type Allowance = (Rule, Text)


-- | The allowances in the block of comment lines directly above a line, given
-- the file's lines and the line, from 1. Malformed directives are reported.
directivesAbove :: [Text] -> Int -> ([Allowance], [String])
directivesAbove fileLines line =
    foldMap parseLine (takeWhile isComment (reverse (take (line - 1) fileLines)))
  where
    isComment = Text.isPrefixOf "--" . Text.stripStart
    parseLine l = case Text.breakOn "hasql-check:" l of
        (_, rest) | not (Text.null rest) -> parseDirective (Text.strip (Text.drop (Text.length "hasql-check:") rest))
        _ -> mempty


parseDirective :: Text -> ([Allowance], [String])
parseDirective t = case Text.stripPrefix "allow" t of
    Just rest -> foldMap entry (splitTopLevel (Text.strip rest))
    Nothing -> ([], ["directive not understood: " <> Text.unpack t])
  where
    entry e = case Text.breakOn "(" (Text.strip e) of
        (name, args)
            | Just inner <- Text.stripPrefix "(" args >>= Text.stripSuffix ")"
            , Just r <- lookup (Text.strip name) [(ruleName r', r') | r' <- [minBound .. maxBound]] ->
                ([(r, Text.strip s) | s <- Text.splitOn "," inner, not (Text.all isSpace s)], [])
        _ -> ([], ["directive not understood: " <> Text.unpack e])
    -- Split on the commas between entries, not those inside them.
    splitTopLevel = go 0 ""
      where
        go :: Int -> Text -> Text -> [Text]
        go depth acc s = case Text.uncons s of
            Nothing -> [acc | not (Text.all isSpace acc)]
            Just (c, cs)
                | c == ',' && depth == 0 -> acc : go depth "" cs
                | c == '(' -> go (depth + 1) (Text.snoc acc c) cs
                | c == ')' -> go (depth - 1) (Text.snoc acc c) cs
                | otherwise -> go depth (Text.snoc acc c) cs


-- | The findings the allowances leave, and the allowances that suppressed
-- nothing for the rules that were checked.
applyDirectives :: [Rule] -> [Allowance] -> [Finding] -> ([Finding], [Allowance])
applyDirectives checked allowances findings =
    (kept, [a | a@(r, _) <- allowances, r `elem` checked, a `Set.notMember` used])
  where
    allowed f = (f.rule, f.subject) `elem` allowances
    (suppressed, kept) = partition allowed findings
    used = Set.fromList [(f.rule, f.subject) | f <- suppressed]


-- * Baselines


data BaselineEntry = BaselineEntry
    { hash :: Text
    , entryRule :: Rule
    , entrySubject :: Text
    , origin :: Text
    -- ^ Where the statement was when the entry was written, for people.
    }


-- | Findings to ignore, by statement, rule and subject.
newtype Baseline = Baseline (Set (Text, Rule, Text))


-- | A stable hash of a statement as rendered, so that layout changes to the
-- quote leave it alone. 64-bit FNV-1a.
statementHash :: ByteString -> Text
statementHash = Text.pack . pad . flip showHex "" . ByteString.foldl' step offset
  where
    offset = 0xcbf29ce484222325 :: Word64
    step h b = (h `xor` fromIntegral b) * 0x100000001b3
    pad s = replicate (16 - length s) '0' <> s


-- | One entry a line: hash, rule, subject, then optionally @# origin@.
readBaseline :: Text -> Either String Baseline
readBaseline = fmap (Baseline . Set.fromList) . traverse entry . filter (not . blank) . Text.lines
  where
    blank l = Text.null (Text.strip l) || "#" `Text.isPrefixOf` Text.stripStart l
    entry l = case Text.words (fst (Text.breakOn "#" l)) of
        [h, r, s] | Just r' <- lookup r [(ruleName x, x) | x <- [minBound .. maxBound]] -> Right (h, r', s)
        _ -> Left ("Malformed baseline line: " <> Text.unpack l)


renderBaseline :: [BaselineEntry] -> Text
renderBaseline entries =
    Text.unlines
        ( "# hasql-check baseline: findings to ignore. Written by --write-baseline."
            : [ Text.unwords [e.hash, ruleName e.entryRule, e.entrySubject, "#", e.origin]
              | e <- sortOn (\e -> (e.origin, e.hash, e.entryRule, e.entrySubject)) entries
              ]
        )


inBaseline :: Baseline -> Text -> Finding -> Bool
inBaseline (Baseline entries) h f = (h, f.rule, f.subject) `Set.member` entries


-- * Both


-- | Apply a quote's directives and the baseline to its statement's findings:
-- the findings left after the directives, which are what a baseline written
-- now would hold, and the messages to report - the findings the baseline does
-- not cover either, malformed directives, and allowances that suppress
-- nothing.
settle :: [Rule] -> ([Allowance], [String]) -> Maybe Baseline -> Text -> [Finding] -> ([Finding], [String])
settle checked (allowances, malformed) baseline h findings =
    ( kept
    , [f.message | f <- kept, maybe True (\b -> not (inBaseline b h f)) baseline]
        <> malformed
        <> [ concat ["allow ", Text.unpack (ruleName r), "(", Text.unpack subj, ") suppresses nothing"]
           | (r, subj) <- unused
           ]
    )
  where
    (kept, unused) = applyDirectives checked allowances findings
