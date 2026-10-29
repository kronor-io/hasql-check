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
-- Annotate the parameters of a statement that has no annotations, as
-- hasql-th asks for.
--
-- 'Hasql.Check.Inferred.TH' annotates with the types of the Haskell values a
-- statement is used with, and the checker annotates the same way, so that it
-- checks the statement the quoter sends. Result columns are not annotated
-- here: the quoter casts them around the statement, where they cannot change
-- what it means.
module Hasql.Check.Annotate (
    annotate,
    typename,
    placeholder,
    untypedLiterals,
) where

import Data.Data (Data, cast, gmapT)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text
import PostgresqlSyntax qualified as Ast
import Prelude


-- | A type, as an annotation would name it - @int8@, @public.payment_state@,
-- @text[]@ - nullable or not. An array's nullability is its elements'.
typename :: Text -> Bool -> Either String Ast.Typename
typename name nullable = case Ast.parse mempty name of
    Right (Ast.Typename setof t _ Nothing) -> Right (Ast.Typename setof t nullable Nothing)
    Right (Ast.Typename setof t _ (Just (dimensions, _))) -> Right (Ast.Typename setof t False (Just (dimensions, nullable)))
    Left _ -> Left ("not a Postgres type name: " <> Text.unpack name)


-- | A made-up type with a plain name, to find in the rendered statement.
placeholder :: Text -> Ast.Typename
placeholder name =
    Ast.Typename False (Ast.GenericTypeSimpleTypename (Ast.GenericType (Ast.UnquotedIdent name) Nothing Nothing)) False Nothing


-- | The statement with each occurrence of the @n@th parameter, counting from
-- 1, cast to the type given for it, if one is. A parameter that is
-- subscripted or has a field taken, @$1['key']@, is cast before that:
-- @($1 :: jsonb)['key']@.
annotate :: (Int -> Maybe Ast.Typename) -> Ast.PreparableStmt -> Ast.PreparableStmt
annotate param = go
  where
    go :: Data b => b -> b
    go x = case cast x of
        Just (Ast.CExprAExpr p@(Ast.ParamCExpr n Nothing)) | Just t <- param n, Just r <- cast (Ast.TypecastAExpr (Ast.CExprAExpr p) t) -> r
        _ -> case cast x of
            Just (Ast.CExprBExpr p@(Ast.ParamCExpr n Nothing)) | Just t <- param n, Just r <- cast (Ast.TypecastBExpr (Ast.CExprBExpr p) t) -> r
            _ -> case cast x of
                Just (Ast.ParamCExpr n (Just indirection))
                    | Just t <- param n
                    , Just r <- cast (Ast.InParensCExpr (Ast.TypecastAExpr (Ast.CExprAExpr (Ast.ParamCExpr n Nothing)) t) (Just indirection)) ->
                        r
                _ -> gmapT go x


-- | The result columns, counting from 1, that are a string literal or NULL
-- with no type of its own, and the literal: Postgres reads one as the type
-- it is cast to, so whether the quoter's cast works depends only on the
-- literal. Only a plain SELECT's columns are found.
untypedLiterals :: Ast.PreparableStmt -> [(Int, Text)]
untypedLiterals = \case
    Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens _ (Ast.SimpleSelectSelectClause (Ast.NormalSimpleSelect (Just t) _ _ _ _ _ _)) _ _ _)) ->
        [(n, Ast.toText mempty e) | (n, Just e) <- zip [1 ..] (targeting t), untyped e]
    _ -> []
  where
    targeting = \case
        Ast.NormalTargeting l -> normalTargets l
        Ast.AllTargeting (Just l) -> targets l
        Ast.DistinctTargeting _ l -> targets l
        Ast.AllTargeting Nothing -> []
    untyped = \case
        Ast.CExprAExpr (Ast.AexprConstCExpr (Ast.SAexprConst _)) -> True
        Ast.CExprAExpr (Ast.AexprConstCExpr Ast.NullAexprConst) -> True
        _ -> False


-- | Each target's expression; nothing for a @*@, which stops the counting.
targets :: Ast.TargetList -> [Maybe Ast.AExpr]
targets (Ast.TargetList els) = takeWhile (/= Nothing) (map expr (toList els))
  where
    expr = \case
        Ast.AliasedExprTargetEl a _ -> Just a
        Ast.ImplicitlyAliasedExprTargetEl a _ -> Just a
        Ast.ExprTargetEl a -> Just a
        Ast.AsteriskTargetEl -> Nothing

#ifdef KRONOR_SYNTAX

normalTargets :: Ast.HsTargetList -> [Maybe Ast.AExpr]
normalTargets (Ast.HsTargetList els) = takeWhile (/= Nothing) (map one (toList els))
  where
    one = \case
        Ast.SqlTargetEl te -> case te of
            Ast.AliasedExprTargetEl a _ -> Just a
            Ast.ImplicitlyAliasedExprTargetEl a _ -> Just a
            Ast.ExprTargetEl a -> Just a
            Ast.AsteriskTargetEl -> Nothing
        _ -> Nothing

#else

normalTargets :: Ast.TargetList -> [Maybe Ast.AExpr]
normalTargets = targets

#endif
