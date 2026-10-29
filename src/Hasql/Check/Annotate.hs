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
) where

import Data.Data (Data, cast, gmapT)
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


-- | The statement with each occurrence of a parameter, and each result
-- column, cast to the type given for its slot, if one is.
--
-- Result columns are annotated only where each one can be told apart: not
-- behind a @*@, nor in a @VALUES@ list. A statement with such a column keeps
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

    noParens (Ast.SelectNoParens w c s l f) = (\c' -> Ast.SelectNoParens w c' s l f) <$> clause c

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
    Ast.AliasedExprTargetEl a i -> Just (Ast.AliasedExprTargetEl (column a) i)
    Ast.ImplicitlyAliasedExprTargetEl a i -> Just (Ast.ImplicitlyAliasedExprTargetEl (column a) i)
    Ast.ExprTargetEl a -> Just (Ast.ExprTargetEl (column a))
    Ast.AsteriskTargetEl -> Nothing
  where
    column e = maybe e (Ast.TypecastAExpr (Ast.CExprAExpr (Ast.InParensCExpr e Nothing))) (slot (Column n))


numbered :: (Int -> a -> Maybe b) -> NonEmpty a -> Maybe (NonEmpty b)
numbered f xs = traverse (uncurry f) (NonEmpty.zip (1 :| [2 ..]) xs)
