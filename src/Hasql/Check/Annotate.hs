{-# LANGUAGE CPP #-}

-- |
-- Module      : Hasql.Check.Annotate
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Add the annotations hasql-th asks for to a statement that has none: a type
-- for every parameter, and one for every result column.
--
-- 'Hasql.Check.Inferred.TH' annotates with the types of the Haskell values a
-- statement is used with, and the checker annotates the same way, so that it
-- checks the statement the quoter sends.
module Hasql.Check.Annotate (
    Slot (..),
    annotate,
    typename,
    placeholder,
) where

import Data.Data (Data, cast, gmapT)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import PostgresqlSyntax qualified as Ast
import Prelude


-- | Where an annotation goes: on the @n@th parameter, or the @n@th result
-- column, both counting from 1.
data Slot = Param Int | Column Int
    deriving stock (Eq, Show)


-- | A type name, nullable or not.
typename :: Text -> Bool -> Ast.Typename
typename name nullable =
    Ast.Typename False (Ast.GenericTypeSimpleTypename (Ast.GenericType (Ast.UnquotedIdent name) Nothing Nothing)) nullable Nothing


-- | A made-up type with a plain name, to find in the rendered statement.
placeholder :: Text -> Ast.Typename
placeholder name = typename name False


-- | The statement with each occurrence of a parameter, and each result
-- column, cast to the type given for its slot, if one is.
--
-- Result columns are annotated only where each one can be told apart: not
-- behind a @*@ or a @t.*@, nor in a @VALUES@ list. A statement with such a column keeps
-- its result columns as they are, and its results must then have exactly the
-- types the Haskell values are read as.
annotate :: (Slot -> Maybe Ast.Typename) -> Ast.PreparableStmt -> Ast.PreparableStmt
annotate slot stmt =
    let withParams = params slot stmt
     in fromMaybe withParams (results slot withParams)


-- * Parameters


params :: (Slot -> Maybe Ast.Typename) -> Ast.PreparableStmt -> Ast.PreparableStmt
params slot = go
  where
    go :: Data b => b -> b
    go x = case cast x of
        Just (Ast.CExprAExpr p@(Ast.ParamCExpr n Nothing)) | Just t <- slot (Param n), Just r <- cast (Ast.TypecastAExpr (Ast.CExprAExpr p) t) -> r
        _ -> case cast x of
            Just (Ast.CExprBExpr p@(Ast.ParamCExpr n Nothing)) | Just t <- slot (Param n), Just r <- cast (Ast.TypecastBExpr (Ast.CExprBExpr p) t) -> r
            _ -> gmapT go x


-- * Result columns


results :: (Slot -> Maybe Ast.Typename) -> Ast.PreparableStmt -> Maybe Ast.PreparableStmt
results slot = \case
    Ast.SelectPreparableStmt s -> Ast.SelectPreparableStmt <$> selectStmt s
    Ast.InsertPreparableStmt (Ast.InsertStmt a b c d r) -> Ast.InsertPreparableStmt . Ast.InsertStmt a b c d <$> returning r
    Ast.UpdatePreparableStmt (Ast.UpdateStmt a b c d e r) -> Ast.UpdatePreparableStmt . Ast.UpdateStmt a b c d e <$> returning r
    Ast.DeletePreparableStmt (Ast.DeleteStmt a b c d r) -> Ast.DeletePreparableStmt . Ast.DeleteStmt a b c d <$> returning r
    Ast.CallPreparableStmt _ -> Nothing
  where
    selectStmt = \case
        Ast.NoParensSelectStmt s -> Ast.NoParensSelectStmt <$> noParens s
        Ast.WithParensSelectStmt s -> Ast.WithParensSelectStmt <$> withParens s

    withParens = \case
        Ast.NoParensSelectWithParens s -> Ast.NoParensSelectWithParens <$> noParens s
        Ast.WithParensSelectWithParens s -> Ast.WithParensSelectWithParens <$> withParens s

    -- SELECT DISTINCT may only be ordered by what it selects, so the ORDER
    -- BY follows the annotations.
    noParens (Ast.SelectNoParens w c s l f) = do
        c' <- clause c
        let s' = case (c, c') of
                (Ast.SimpleSelectSelectClause (Ast.NormalSimpleSelect (Just (Ast.DistinctTargeting Nothing (Ast.TargetList old))) _ _ _ _ _ _), Ast.SimpleSelectSelectClause (Ast.NormalSimpleSelect (Just (Ast.DistinctTargeting Nothing (Ast.TargetList new))) _ _ _ _ _ _)) ->
                    distinctOrder (toList old) (toList new) <$> s
                _ -> s
        pure (Ast.SelectNoParens w c' s' l f)

    clause = \case
        Ast.SimpleSelectSelectClause s -> Ast.SimpleSelectSelectClause <$> simple s
        Ast.WithParensSelectClause s -> Ast.WithParensSelectClause <$> withParens s

    -- Both sides of a UNION, INTERSECT or EXCEPT, so that they agree.
    simple = \case
        Ast.NormalSimpleSelect (Just t) b c d e f g -> (\t' -> Ast.NormalSimpleSelect (Just t') b c d e f g) <$> targeting t
        Ast.BinSimpleSelect op l all' r -> Ast.BinSimpleSelect op <$> clause l <*> pure all' <*> clause r
        _ -> Nothing

    returning = \case
        Just (Ast.ReturningClause l) -> Just . Ast.ReturningClause <$> targetList slot l
        Nothing -> Just Nothing

    targeting = \case
        Ast.NormalTargeting l -> Ast.NormalTargeting <$> normalTargets slot l
        Ast.AllTargeting (Just l) -> Ast.AllTargeting . Just <$> targetList slot l
        Ast.DistinctTargeting e l -> Ast.DistinctTargeting e <$> targetList slot l
        Ast.AllTargeting Nothing -> Nothing


targetList :: (Slot -> Maybe Ast.Typename) -> Ast.TargetList -> Maybe Ast.TargetList
targetList slot (Ast.TargetList els) = Ast.TargetList <$> numbered (targetEl slot) els

#ifdef KRONOR_SYNTAX

-- | Kronor's fork keeps the select list as Haskell targets; a statement for
-- these quoters has only plain SQL ones.
normalTargets :: (Slot -> Maybe Ast.Typename) -> Ast.HsTargetList -> Maybe Ast.HsTargetList
normalTargets slot (Ast.HsTargetList els) = Ast.HsTargetList <$> numbered one els
  where
    one n = \case
        Ast.SqlTargetEl te -> Ast.SqlTargetEl <$> targetEl slot n te
        _ -> Nothing

#else

normalTargets :: (Slot -> Maybe Ast.Typename) -> Ast.TargetList -> Maybe Ast.TargetList
normalTargets = targetList

#endif


targetEl :: (Slot -> Maybe Ast.Typename) -> Int -> Ast.TargetEl -> Maybe Ast.TargetEl
targetEl slot n = \case
    Ast.AliasedExprTargetEl a i -> (`Ast.AliasedExprTargetEl` i) <$> column a
    Ast.ImplicitlyAliasedExprTargetEl a i -> (`Ast.ImplicitlyAliasedExprTargetEl` i) <$> column a
    Ast.ExprTargetEl a -> Ast.ExprTargetEl <$> column a
    Ast.AsteriskTargetEl -> Nothing
  where
    column e
        | expands e = Nothing
        | otherwise = Just (maybe e (Ast.TypecastAExpr (Ast.CExprAExpr (Ast.InParensCExpr e Nothing))) (slot (Column n)))


-- | Whether an expression expands to several columns, as @t.*@ and
-- @(row).*@ do.
expands :: Ast.AExpr -> Bool
expands = \case
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref _ (Just i))) -> star i
    Ast.CExprAExpr (Ast.InParensCExpr _ (Just i)) -> star i
    Ast.CExprAExpr (Ast.ParamCExpr _ (Just i)) -> star i
    Ast.CExprAExpr (Ast.SelectWithParensCExpr _ (Just i)) -> star i
    _ -> False
  where
    star (Ast.Indirection els) = NonEmpty.last els == Ast.AllIndirectionEl


-- | The ORDER BY of a SELECT DISTINCT whose columns were annotated, given
-- them before and after.
--
-- A bare name in ORDER BY means the output column of that name if there is
-- one, and annotating a column keeps its name, so such a name is left alone:
-- it may even name another column than the expression it spells. Any other
-- ORDER BY expression that is one of the columns selected becomes that
-- column, annotated.
distinctOrder :: [Ast.TargetEl] -> [Ast.TargetEl] -> Ast.SortClause -> Ast.SortClause
distinctOrder old new (Ast.SortClause bys) = Ast.SortClause (fmap by bys)
  where
    replacements = zip (concatMap targetExpr old) (concatMap targetExpr new)
    outputNames = concatMap outputName old
    by = \case
        Ast.UsingSortBy e op n -> Ast.UsingSortBy (sortExpr e) op n
        Ast.AscDescSortBy e d n -> Ast.AscDescSortBy (sortExpr e) d n
    sortExpr e
        | Just name <- bareName e, name `elem` outputNames = e
        | otherwise = fromMaybe e (lookup e replacements)


-- | The name Postgres gives a result column, where it is a column's own.
outputName :: Ast.TargetEl -> [Text]
outputName = \case
    Ast.AliasedExprTargetEl _ i -> [identText i]
    Ast.ImplicitlyAliasedExprTargetEl _ i -> [identText i]
    Ast.ExprTargetEl (Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref i Nothing))) -> [identText i]
    Ast.ExprTargetEl (Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref _ (Just (Ast.Indirection els)))))
        | Ast.AttrNameIndirectionEl i <- NonEmpty.last els -> [identText i]
    _ -> []


bareName :: Ast.AExpr -> Maybe Text
bareName = \case
    Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref i Nothing)) -> Just (identText i)
    _ -> Nothing


identText :: Ast.Ident -> Text
identText = \case
    Ast.QuotedIdent t -> t
    Ast.UnquotedIdent t -> t


targetExpr :: Ast.TargetEl -> [Ast.AExpr]
targetExpr = \case
    Ast.AliasedExprTargetEl a _ -> [a]
    Ast.ImplicitlyAliasedExprTargetEl a _ -> [a]
    Ast.ExprTargetEl a -> [a]
    Ast.AsteriskTargetEl -> []


numbered :: (Int -> a -> Maybe b) -> NonEmpty a -> Maybe (NonEmpty b)
numbered f xs = traverse (uncurry f) (NonEmpty.zip (1 :| [2 ..]) xs)
