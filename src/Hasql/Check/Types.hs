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
    checkColumns,
) where

import Control.Exception (finally)
import Control.Monad (forM)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Traversable (mapAccumL)
import Data.Word (Word32)
import Hasql.Check (renderStmt)
import Hasql.Check.Finding (Finding (..), Rule (..))
import Hasql.Check.Nullability (Annotation (..), Catalogue, baseType, query, strip)
import PostgresqlSyntax qualified as Ast
import Pqi qualified
import Text.Read (readMaybe)
import Prelude


-- | The findings for a statement that prepares.
check :: Pqi.Connection -> Catalogue -> Ast.PreparableStmt -> IO [Finding]
check conn catalogue stmt
    | null (fst (stripAssigned 0 stmt)) && maybe True (null . fst) (strip stmt) = pure []
    | otherwise =
        ( describe "hasql_check_types_annotated" stmt Nothing >>= \case
            Nothing -> pure []
            Just annotated -> do
                -- Every assigned occurrence gets a placeholder of its own, so
                -- that a parameter assigned to two columns is checked against
                -- both; the statement's own parameters keep their annotated
                -- types, declared, since some may now appear nowhere.
                let (assigned, withoutParams) = stripAssigned (length annotated.params) stmt
                    (annotations, stripped) = fromMaybe ([], withoutParams) (strip withoutParams)
                    declared = annotated.params <> map (const 0) assigned
                describe "hasql_check_types_stripped" stripped (Just declared) >>= \case
                    Nothing -> pure []
                    Just found -> (<>) <$> results annotations annotated found <*> params assigned annotated found
        )
            `finally` mapM_ (\name -> query conn ("DEALLOCATE " <> name)) ["hasql_check_types_annotated", "hasql_check_types_stripped"]
  where
    results annotations annotated found
        | length annotations /= length annotated.columns || length found.columns /= length annotated.columns = pure []
        | otherwise =
            fmap catMaybes . sequence $
                [ narrowing conn catalogue from to <&> fmap \(fromName, toName, consequence) ->
                    Finding Narrowing (Text.pack name) ("column " <> show i <> " (" <> name <> ") is annotated " <> toName <> ", but is " <> article fromName <> ": " <> consequence)
                | (i, Annotation{nullable = Just _}, (name, to, _), (_, from, fromTable)) <- zip4 [1 :: Int ..] annotations annotated.columns found.columns
                , fromTable
                ]

    params assigned annotated found =
        fmap catMaybes . sequence $
            [ narrowing conn catalogue from to <&> fmap \(fromName, toName, consequence) ->
                Finding Narrowing (Text.pack ("$" <> show n)) ("parameter $" <> show n <> " is annotated " <> fromName <> ", but is assigned to " <> article toName <> " column: " <> consequence)
            | (n, placeholder) <- assigned
            , Just from <- [lookupParam n annotated]
            , Just to <- [lookupParam placeholder found]
            ]

    lookupParam n d = lookup n (zip [1 ..] d.params)

    describe name s types = do
        prepared <- conn.prepare name (renderStmt s) types
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


-- | The findings for result columns read as the given Postgres types, by
-- name, rather than annotated: those of an inferred statement, cast around
-- it. Every column is checked, expressions too: nobody chose the conversion.
checkColumns :: Pqi.Connection -> Catalogue -> [Text.Text] -> Ast.PreparableStmt -> IO [Finding]
checkColumns conn catalogue types stmt = do
    prepared <- conn.prepare "hasql_check_types_columns" (renderStmt stmt) Nothing
    status <- maybe (pure Pqi.FatalError) (.resultStatus) prepared
    if status /= Pqi.CommandOk
        then pure []
        else
            ( conn.describePrepared "hasql_check_types_columns" >>= \case
                Nothing -> pure []
                Just d -> do
                    n <- d.nfields
                    found <- forM [0 .. n - 1] \i -> (,) <$> (maybe "?column?" (Text.unpack . Text.decodeUtf8Lenient) <$> d.fname i) <*> d.ftype i
                    expected <- traverse oid types
                    fmap catMaybes . sequence $
                        [ narrowing conn catalogue from to <&> fmap \(fromName, toName, consequence) ->
                            Finding Narrowing (Text.pack name) ("column " <> show i <> " (" <> name <> ") is annotated " <> toName <> ", but is " <> article fromName <> ": " <> consequence)
                        | length found == length types
                        , (i :: Int, (name, from), Just to) <- zip3 [1 ..] found expected
                        ]
            )
                `finally` query conn "DEALLOCATE hasql_check_types_columns"
  where
    oid name =
        query conn (mconcat ["SELECT to_regtype('", Text.encodeUtf8 (Text.replace "'" "''" name), "')::oid::text"]) >>= \case
            Right [[Just o]] -> pure (readMaybe (ByteString.Char8.unpack o))
            _ -> pure Nothing


-- | 'lossy', through domains, and through arrays element by element, as
-- Postgres converts them: a bigint[] read as integer[] may overflow.
narrowing :: Pqi.Connection -> Catalogue -> Word32 -> Word32 -> IO (Maybe (String, String, String))
narrowing conn catalogue from to =
    query
        conn
        ( ByteString.Char8.pack
            ( concat
                [ "SELECT f.typelem::text, t.typelem::text FROM pg_catalog.pg_type f, pg_catalog.pg_type t WHERE f.oid = "
                , show from'
                , " AND t.oid = "
                , show to'
                , " AND f.typcategory = 'A' AND t.typcategory = 'A'"
                ]
            )
        )
        >>= \case
            Right [[Just f, Just t]]
                | Just f' <- readMaybe (ByteString.Char8.unpack f)
                , Just t' <- readMaybe (ByteString.Char8.unpack t) ->
                    fmap (\(fromName, toName, consequence) -> (fromName <> "[]", toName <> "[]", consequence)) <$> narrowing conn catalogue f' t'
            _ -> pure (lossy from' to')
  where
    from' = baseType catalogue from
    to' = baseType catalogue to


data Described = Described
    { params :: [Word32]
    , columns :: [(String, Word32, Bool)]
    -- ^ Name, type, and whether it is a table column rather than an expression.
    }


zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []


article :: String -> String
article name@(c : _) | c `elem` ("aeiou" :: String) = "an " <> name
article name = "a " <> name


-- * Stripping assigned parameters


-- | The statement with each parameter it assigns to a column replaced by a
-- new, unannotated placeholder numbered from after the given count, and the
-- pairs of parameter and placeholder.
stripAssigned :: Int -> Ast.PreparableStmt -> ([(Int, Int)], Ast.PreparableStmt)
stripAssigned count = \case
    Ast.InsertPreparableStmt
        ( Ast.InsertStmt
                w
                t
                (Ast.SelectInsertRest cols o (Ast.NoParensSelectStmt (Ast.SelectNoParens w2 (Ast.SimpleSelectSelectClause (Ast.ValuesSimpleSelect (Ast.ValuesClause rows))) s l f)))
                c
                r
            ) ->
            let (acc, rows') = mapAccumL (\a (Ast.ExprList es) -> Ast.ExprList <$> mapAccumL param a es) [] rows
             in (reverse acc, Ast.InsertPreparableStmt (Ast.InsertStmt w t (Ast.SelectInsertRest cols o (Ast.NoParensSelectStmt (Ast.SelectNoParens w2 (Ast.SimpleSelectSelectClause (Ast.ValuesSimpleSelect (Ast.ValuesClause rows'))) s l f))) c r))
    Ast.UpdatePreparableStmt (Ast.UpdateStmt w rel (Ast.SetClauseList clauses) f wh r) ->
        let (acc, clauses') = mapAccumL setClause [] clauses
         in (reverse acc, Ast.UpdatePreparableStmt (Ast.UpdateStmt w rel (Ast.SetClauseList clauses') f wh r))
    other -> ([], other)
  where
    setClause acc = \case
        Ast.TargetSetClause target e -> Ast.TargetSetClause target <$> param acc e
        other -> (acc, other)
    param acc = \case
        Ast.TypecastAExpr (Ast.CExprAExpr (Ast.ParamCExpr n Nothing)) _ ->
            let placeholder = count + length acc + 1
             in ((n, placeholder) : acc, Ast.CExprAExpr (Ast.ParamCExpr placeholder Nothing))
        e -> (acc, e)


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
