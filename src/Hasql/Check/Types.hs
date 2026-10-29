-- |
-- Module      : Hasql.Check.Types
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Warn about annotations that convert a value to a narrower type.
--
-- An annotation is a cast, and a cast between incompatible types is an error
-- @PREPARE@ already reports. A narrowing one is not: @bigint_column :: int4@
-- prepares, and fails or loses data at runtime only for values that do not
-- fit. To see what each annotation converts, the statement is prepared again
-- without the annotations on its result columns and on the parameters it
-- assigns to columns - in @INSERT ... VALUES@ and @UPDATE ... SET@ - and
-- Postgres reports the types it finds there.
--
-- Only result columns that read a table column are checked: an expression
-- converted to a narrower type - @count(*) :: int4@, @now() :: date@ - is
-- almost always converted on purpose.
--
-- Parameters anywhere else are not checked: comparing an @int4@ column with an
-- @int8@ parameter widens the column, and narrows nothing.
module Hasql.Check.Types (
    check,
) where

import Control.Exception (finally)
import Control.Monad (forM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32)
import Hasql.Check (renderStmt)
import Hasql.Check.Finding (Finding (..), Rule (..))
import Hasql.Check.Nullability (Catalogue, baseType, query, strip)
import PostgresqlSyntax qualified as Ast
import Pqi qualified
import Prelude


-- | The findings for a statement that prepares.
check :: Pqi.Connection -> Catalogue -> Ast.PreparableStmt -> IO [Finding]
check conn catalogue stmt
    | null assigned && null annotations = pure []
    | otherwise =
        ( describe "hasql_check_types_annotated" stmt >>= \case
            Nothing -> pure []
            Just annotated ->
                describe "hasql_check_types_stripped" stripped >>= \case
                    Nothing -> pure []
                    Just found -> pure (results annotated found <> params annotated found)
        )
            `finally` mapM_ (\name -> query conn ("DEALLOCATE " <> name)) ["hasql_check_types_annotated", "hasql_check_types_stripped"]
  where
    (assigned, withoutParams) = stripAssigned stmt
    (annotations, stripped) = fromMaybe ([], withoutParams) (strip withoutParams)

    results annotated found
        | length annotations /= length annotated.columns || length found.columns /= length annotated.columns = []
        | otherwise =
            catMaybes
                [ narrowing from to <&&> \(fromName, toName, consequence) ->
                    Finding Narrowing (Text.pack name) ("column " <> show i <> " (" <> name <> ") is annotated " <> toName <> ", but is " <> article fromName <> ": " <> consequence)
                | (i, Just _, (name, to, _), (_, from, fromTable)) <- zip4 [1 :: Int ..] annotations annotated.columns found.columns
                , fromTable
                ]

    params annotated found =
        catMaybes
            [ narrowing from to <&&> \(fromName, toName, consequence) ->
                Finding Narrowing (Text.pack ("$" <> show n)) ("parameter $" <> show n <> " is annotated " <> fromName <> ", but is assigned to " <> article toName <> " column: " <> consequence)
            | n <- assigned
            , Just from <- [lookupParam n annotated]
            , Just to <- [lookupParam n found]
            ]

    lookupParam n d = lookup n (zip [1 ..] d.params)

    narrowing from to = lossy (baseType catalogue from) (baseType catalogue to)

    describe name s = do
        prepared <- conn.prepare name (renderStmt s) Nothing
        status <- maybe (pure Pqi.FatalError) (.resultStatus) prepared
        if status /= Pqi.CommandOk
            then pure Nothing
            else
                conn.describePrepared name >>= \case
                    Nothing -> pure Nothing
                    Just d -> do
                        nparams <- d.nparams
                        nfields <- d.nfields
                        ps <- forM [0 .. nparams - 1] d.paramtype
                        cs <- forM [0 .. nfields - 1] \i ->
                            (,,)
                                <$> (maybe "?column?" (Text.unpack . Text.decodeUtf8Lenient) <$> d.fname i)
                                <*> d.ftype i
                                <*> ((/= 0) <$> d.ftable i)
                        pure (Just (Described ps cs))


data Described = Described
    { params :: [Word32]
    , columns :: [(String, Word32, Bool)]
    -- ^ Name, type, and whether it is a table column rather than an expression.
    }


(<&&>) :: Maybe a -> (a -> b) -> Maybe b
(<&&>) = flip fmap


zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []


article :: String -> String
article name@(c : _) | c `elem` ("aeiou" :: String) = "an " <> name
article name = "a " <> name


-- * Stripping assigned parameters


-- | The statement with the annotations removed from the parameters it assigns
-- to columns, and their numbers.
stripAssigned :: Ast.PreparableStmt -> ([Int], Ast.PreparableStmt)
stripAssigned = \case
    Ast.InsertPreparableStmt
        ( Ast.InsertStmt
                w
                t
                (Ast.SelectInsertRest cols o (Ast.NoParensSelectStmt (Ast.SelectNoParens w2 (Ast.SimpleSelectSelectClause (Ast.ValuesSimpleSelect (Ast.ValuesClause rows))) s l f)))
                c
                r
            ) ->
            let (ns, rows') = traverse (\(Ast.ExprList es) -> Ast.ExprList <$> traverse param es) rows
             in (ns, Ast.InsertPreparableStmt (Ast.InsertStmt w t (Ast.SelectInsertRest cols o (Ast.NoParensSelectStmt (Ast.SelectNoParens w2 (Ast.SimpleSelectSelectClause (Ast.ValuesSimpleSelect (Ast.ValuesClause rows'))) s l f))) c r))
    Ast.UpdatePreparableStmt (Ast.UpdateStmt w rel (Ast.SetClauseList clauses) f wh r) ->
        let (ns, clauses') = traverse setClause clauses
         in (ns, Ast.UpdatePreparableStmt (Ast.UpdateStmt w rel (Ast.SetClauseList clauses') f wh r))
    other -> ([], other)
  where
    setClause = \case
        Ast.TargetSetClause target e -> Ast.TargetSetClause target <$> param e
        other -> ([], other)
    param = \case
        Ast.TypecastAExpr p@(Ast.CExprAExpr (Ast.ParamCExpr n Nothing)) _ -> ([n], p)
        e -> ([], e)


-- * Narrowing conversions


-- | What converting a value of the first type to the second can do to it, with
-- the two types' names, if it is a narrowing conversion.
lossy :: Word32 -> Word32 -> Maybe (String, String, String)
lossy from to = do
    fromName <- lookup from names
    toName <- lookup to names
    consequence <- lookup (from, to) conversions
    pure (fromName, toName, consequence)
  where
    names =
        [ (int2, "smallint")
        , (int4, "integer")
        , (int8, "bigint")
        , (numeric, "numeric")
        , (float4, "real")
        , (float8, "double precision")
        , (timestamp, "timestamp")
        , (timestamptz, "timestamptz")
        , (date, "date")
        ]
    overflow = "it may overflow"
    rounding = "it may be rounded, or overflow"
    precision = "it may lose precision"
    conversions =
        [ ((int8, int4), overflow)
        , ((int8, int2), overflow)
        , ((int4, int2), overflow)
        , ((numeric, int8), rounding)
        , ((numeric, int4), rounding)
        , ((numeric, int2), rounding)
        , ((float8, int8), rounding)
        , ((float8, int4), rounding)
        , ((float8, int2), rounding)
        , ((float4, int8), rounding)
        , ((float4, int4), rounding)
        , ((float4, int2), rounding)
        , ((numeric, float8), precision)
        , ((numeric, float4), precision)
        , ((float8, float4), precision)
        , ((int8, float8), precision)
        , ((int8, float4), precision)
        , ((int4, float4), precision)
        , ((timestamptz, timestamp), "it becomes a local time in the session's time zone")
        , ((timestamptz, date), "it drops the time of day, in the session's time zone")
        , ((timestamp, date), "it drops the time of day")
        ]
    int2 = 21
    int4 = 23
    int8 = 20
    numeric = 1700
    float4 = 700
    float8 = 701
    timestamp = 1114
    timestamptz = 1184
    date = 1082
