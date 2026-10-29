{-# LANGUAGE CPP #-}

-- |
-- Module      : Hasql.Check.Nullability
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Warn about result columns annotated non-null that can be null.
--
-- The annotation on a result column is a cast, and a cast hides where the
-- column came from, so the statement is prepared a second time with the
-- outermost cast of every result column removed. Postgres then reports, for
-- each plain column reference, the table and column it reads, and the
-- catalogue says whether that column is @NOT NULL@.
--
-- The plan of that statement adds two things. Columns from the nullable side
-- of an outer join can be null whatever the catalogue says, as sqlx also
-- finds. And a column the plan filters with @IS NOT NULL@ cannot be null,
-- whatever the catalogue says. The plan rather than the statement's own
-- @WHERE@ clause is used for the second because the plan has the
-- conditions after Postgres has resolved precedence, pushed them down and
-- turned outer joins they make inner into inner joins.
--
-- Anything else - expressions, views - is not checked: Postgres does not say
-- whether they can be null.
module Hasql.Check.Nullability (
    Catalogue,
    baseType,
    prepareSession,
    Annotation (..),
    strip,
    query,
    check,
    resultQuoters,
) where

import Control.Exception (finally)
import Control.Monad (forM)
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Data (Data, cast, gmapT)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32)
import Hasql.Check (renderStmt)
import Hasql.Check.Finding (Finding (..), Rule (..))
import PostgresqlSyntax qualified as Ast
import Pqi qualified
import Text.Read (readMaybe)
import Prelude


-- | The quoters whose statements return rows.
resultQuoters :: [Text]
resultQuoters = ["singletonStatement", "maybeStatement", "vectorStatement", "foldStatement"]


-- * The catalogue


data Column = Column
    { qualifiedName :: Text
    , notNull :: Bool
    , fromView :: Bool
    -- ^ A view's or materialized view's column, whose @attnotnull@ is always
    -- false and so says nothing.
    }


data Catalogue = Catalogue
    { columns :: Map (Word32, Int32) Column
    -- ^ Every column of every relation, by table OID and column number.
    , domains :: Map Word32 Word32
    -- ^ The base type of every domain.
    }


-- | A type with any domains it is built on looked through.
baseType :: Catalogue -> Word32 -> Word32
baseType catalogue t = maybe t (baseType catalogue) (Map.lookup t catalogue.domains)


-- | Get the session ready for 'check': read the catalogue, and force generic
-- plans, so that EXPLAIN EXECUTE with NULL parameters shows the plan for any
-- parameters rather than one with the NULLs folded in.
prepareSession :: Pqi.Connection -> IO (Either String Catalogue)
prepareSession conn = do
    generic <- query conn "SET plan_cache_mode = force_generic_plan"
    catalogue <- loadCatalogue conn
    pure (generic *> catalogue)


loadCatalogue :: Pqi.Connection -> IO (Either String Catalogue)
loadCatalogue conn = do
    result <-
        query
            conn
            ( ByteString.Char8.unwords
                [ "SELECT a.attrelid::text, a.attnum::text, n.nspname || '.' || c.relname || '.' || a.attname,"
                , "a.attnotnull::text, (c.relkind IN ('v', 'm'))::text"
                , "FROM pg_catalog.pg_attribute a"
                , "JOIN pg_catalog.pg_class c ON c.oid = a.attrelid"
                , "JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace"
                , "WHERE a.attnum > 0 AND NOT a.attisdropped"
                ]
            )
    domainRows <- query conn "SELECT oid::text, typbasetype::text FROM pg_catalog.pg_type WHERE typtype = 'd'"
    pure do
        rows <- result
        columns <- Map.fromList <$> traverse row rows
        domains <- Map.fromList <$> (domainRows >>= traverse domain)
        pure (Catalogue columns domains)
  where
    domain = \case
        [Just t, Just base]
            | Just t' <- readMaybe (ByteString.Char8.unpack t)
            , Just base' <- readMaybe (ByteString.Char8.unpack base) ->
                Right (t', base')
        other -> Left ("Unexpected domain row: " <> show other)
    row = \case
        [Just rel, Just num, Just name, Just nn, Just view]
            | Just rel' <- readMaybe (ByteString.Char8.unpack rel)
            , Just num' <- readMaybe (ByteString.Char8.unpack num) ->
                Right ((rel', num'), Column (Text.decodeUtf8Lenient name) (nn == "true") (view == "true"))
        other -> Left ("Unexpected catalogue row: " <> show other)


-- * Stripping the result casts


-- | A result column's outermost cast, if it has one: its nullability, and the
-- expression with and without it.
data Annotation = Annotation
    { nullable :: Maybe Bool
    , removed :: Maybe (Ast.AExpr, Ast.AExpr)
    }


typenameNullable :: Ast.Typename -> Bool
typenameNullable (Ast.Typename _ _ nullable dims) = maybe nullable snd dims


stripExpr :: Ast.AExpr -> (Annotation, Ast.AExpr)
stripExpr = \case
    whole@(Ast.TypecastAExpr e t) -> (Annotation (Just (typenameNullable t)) (Just (whole, e)), e)
    e -> (Annotation Nothing Nothing, e)


stripTargetEl :: Ast.TargetEl -> Maybe (Annotation, Ast.TargetEl)
stripTargetEl = \case
    Ast.AliasedExprTargetEl a i -> Just (second (`Ast.AliasedExprTargetEl` i) (stripExpr a))
    Ast.ImplicitlyAliasedExprTargetEl a i -> Just (second (`Ast.ImplicitlyAliasedExprTargetEl` i) (stripExpr a))
    Ast.ExprTargetEl a -> Just (second Ast.ExprTargetEl (stripExpr a))
    Ast.AsteriskTargetEl -> Nothing


stripTargetList :: Ast.TargetList -> Maybe ([Annotation], Ast.TargetList)
stripTargetList (Ast.TargetList els) = do
    stripped <- traverse stripTargetEl els
    pure (map fst (toList stripped), Ast.TargetList (fmap snd stripped))

#ifdef KRONOR_SYNTAX

-- | Kronor's fork keeps the select list as Haskell targets; once erased, every
-- one of them is a plain SQL target.
stripNormalTargeting :: Ast.HsTargetList -> Maybe ([Annotation], Ast.Targeting)
stripNormalTargeting (Ast.HsTargetList els) = do
    stripped <- traverse one els
    pure (map fst (toList stripped), Ast.NormalTargeting (Ast.HsTargetList (fmap snd stripped)))
  where
    one = \case
        Ast.SqlTargetEl te -> second Ast.SqlTargetEl <$> stripTargetEl te
        _ -> Nothing

#else

stripNormalTargeting :: Ast.TargetList -> Maybe ([Annotation], Ast.Targeting)
stripNormalTargeting l = second Ast.NormalTargeting <$> stripTargetList l

#endif


stripTargeting :: Ast.Targeting -> Maybe ([Annotation], Ast.Targeting)
stripTargeting = \case
    Ast.NormalTargeting l -> stripNormalTargeting l
    Ast.AllTargeting (Just l) -> second (Ast.AllTargeting . Just) <$> stripTargetList l
    Ast.DistinctTargeting e l -> second (Ast.DistinctTargeting e) <$> stripTargetList l
    Ast.AllTargeting Nothing -> Nothing


stripSimple :: Ast.SimpleSelect -> Maybe ([Annotation], Ast.SimpleSelect)
stripSimple = \case
    Ast.NormalSimpleSelect (Just t) b c d e f g -> second (\t' -> Ast.NormalSimpleSelect (Just t') b c d e f g) <$> stripTargeting t
    _ -> Nothing


stripClause :: Ast.SelectClause -> Maybe ([Annotation], Ast.SelectClause)
stripClause = \case
    Ast.SimpleSelectSelectClause s -> second Ast.SimpleSelectSelectClause <$> stripSimple s
    Ast.WithParensSelectClause s -> second Ast.WithParensSelectClause <$> stripWithParens s


stripNoParens :: Ast.SelectNoParens -> Maybe ([Annotation], Ast.SelectNoParens)
stripNoParens (Ast.SelectNoParens a c s l f) = second (\c' -> Ast.SelectNoParens a c' s l f) <$> stripClause c


stripWithParens :: Ast.SelectWithParens -> Maybe ([Annotation], Ast.SelectWithParens)
stripWithParens = \case
    Ast.NoParensSelectWithParens s -> second Ast.NoParensSelectWithParens <$> stripNoParens s
    Ast.WithParensSelectWithParens s -> second Ast.WithParensSelectWithParens <$> stripWithParens s


stripReturning :: Maybe Ast.ReturningClause -> Maybe ([Annotation], Maybe Ast.ReturningClause)
stripReturning = \case
    Just (Ast.ReturningClause l) -> second (Just . Ast.ReturningClause) <$> stripTargetList l
    Nothing -> Nothing


-- | The annotation of every result column, and the statement without them.
--
-- An annotated expression is replaced wherever it occurs, not only in the
-- result columns: @select distinct amount :: int4 ... order by amount :: int4@
-- would otherwise order by an expression it no longer selects, and fail to
-- prepare.
strip :: Ast.PreparableStmt -> Maybe ([Annotation], Ast.PreparableStmt)
strip stmt = do
    (annotations, stripped) <- stripResults stmt
    let replacements = [r | Annotation{removed = Just r} <- annotations]
    pure (annotations, replaceEverywhere replacements stripped)


-- | Replace every occurrence of the given expressions.
replaceEverywhere :: Data a => [(Ast.AExpr, Ast.AExpr)] -> a -> a
replaceEverywhere [] = id
replaceEverywhere replacements = go
  where
    go :: Data b => b -> b
    go x = case cast x of
        Just e | Just replacement <- lookup e replacements, Just r <- cast replacement -> r
        _ -> gmapT go x


stripResults :: Ast.PreparableStmt -> Maybe ([Annotation], Ast.PreparableStmt)
stripResults = \case
    Ast.SelectPreparableStmt (Ast.NoParensSelectStmt s) -> second (Ast.SelectPreparableStmt . Ast.NoParensSelectStmt) <$> stripNoParens s
    Ast.SelectPreparableStmt (Ast.WithParensSelectStmt s) -> second (Ast.SelectPreparableStmt . Ast.WithParensSelectStmt) <$> stripWithParens s
    Ast.InsertPreparableStmt (Ast.InsertStmt a b c d r) -> second (Ast.InsertPreparableStmt . Ast.InsertStmt a b c d) <$> stripReturning r
    Ast.UpdatePreparableStmt (Ast.UpdateStmt a b c d e r) -> second (Ast.UpdatePreparableStmt . Ast.UpdateStmt a b c d e) <$> stripReturning r
    Ast.DeletePreparableStmt (Ast.DeleteStmt a b c d r) -> second (Ast.DeletePreparableStmt . Ast.DeleteStmt a b c d) <$> stripReturning r
    Ast.CallPreparableStmt _ -> Nothing


-- * The plan


data Plan = Plan
    { nodeType :: Text
    , output :: [Text]
    , joinType :: Maybe Text
    , parentRelationship :: Maybe Text
    , filters :: [Text]
    -- ^ The conditions applied to the node's own rows. Not its join filter,
    -- which on an outer join only decides what matches.
    , plans :: [Plan]
    }


instance Aeson.FromJSON Plan where
    parseJSON = Aeson.withObject "Plan" \o -> do
        filters <- catMaybes <$> traverse (o .:?) ["Filter", "Index Cond", "Recheck Cond"]
        Plan
            <$> (fromMaybe "" <$> o .:? "Node Type")
            <*> (fromMaybe [] <$> o .:? "Output")
            <*> o .:? "Join Type"
            <*> o .:? "Parent Relationship"
            <*> pure filters
            <*> (fromMaybe [] <$> o .:? "Plans")


newtype Explained = Explained Plan


instance Aeson.FromJSON Explained where
    parseJSON = Aeson.withObject "EXPLAIN" \o -> Explained <$> o .: "Plan"


-- | What the plan says about the nullability of the expressions it outputs.
data PlanNullability = PlanNullability
    { outerJoined :: Set Text
    -- ^ Output by a node on the nullable side of an outer join: the inner side
    -- of a left join, the outer side of a right join, both sides of a full one.
    , filteredNotNull :: Set Text
    -- ^ Filtered with @IS NOT NULL@ by a node whose condition holds for every
    -- row: not one on such a side, under one branch of a @BitmapOr@, or in a
    -- subplan, whose rows are not the statement's.
    }


instance Semigroup PlanNullability where
    PlanNullability a b <> PlanNullability c d = PlanNullability (a <> c) (b <> d)


instance Monoid PlanNullability where
    mempty = PlanNullability Set.empty Set.empty


planNullability :: Plan -> PlanNullability
planNullability = go False True
  where
    go nullable guarding plan =
        PlanNullability
            (if nullable then Set.fromList plan.output else Set.empty)
            (if guarding && not nullable then Set.fromList (concatMap notNullOperands plan.filters) else Set.empty)
            <> foldMap
                ( \child ->
                    go
                        (nullable || nullableChild plan child)
                        (guarding && plan.nodeType /= "BitmapOr" && child.parentRelationship `notElem` [Just "SubPlan", Just "InitPlan"])
                        child
                )
                plan.plans
    nullableChild parent child = case parent.joinType of
        Just "Left" -> child.parentRelationship == Just "Inner"
        Just "Right" -> child.parentRelationship == Just "Outer"
        Just "Full" -> True
        _ -> False


-- | The operands of the @IS NOT NULL@ conjuncts of a condition as EXPLAIN
-- prints it: @((a.id > 0) AND (a.nickname IS NOT NULL))@ gives @a.nickname@.
notNullOperands :: Text -> [Text]
notNullOperands = mapMaybe (Text.stripSuffix " IS NOT NULL" . unwrap) . conjuncts . unwrap
  where
    conjuncts = splitTopLevel " AND "


-- | Drop parentheses around the whole of an expression.
unwrap :: Text -> Text
unwrap t
    | Just inner <- Text.stripPrefix "(" t >>= Text.stripSuffix ")"
    , balanced inner =
        unwrap inner
    | otherwise = t
  where
    balanced = all (>= 0) . depths


-- | The nesting depth after each character. The inside of a quoted literal or
-- identifier counts as one deeper, so nothing in it is ever at the top level.
depths :: Text -> [Int]
depths = go 0 Nothing . Text.unpack
  where
    go :: Int -> Maybe Char -> String -> [Int]
    go _ _ [] = []
    go d (Just q) (c : cs)
        | c == q = d : go d Nothing cs
        | otherwise = (d + 1) : go d (Just q) cs
    go d Nothing (c : cs) = case c of
        '(' -> (d + 1) : go (d + 1) Nothing cs
        ')' -> (d - 1) : go (d - 1) Nothing cs
        '\'' -> d : go d (Just '\'') cs
        '"' -> d : go d (Just '"') cs
        _ -> d : go d Nothing cs


-- | Split on a separator that is outside any parentheses or quotes.
splitTopLevel :: Text -> Text -> [Text]
splitTopLevel sep t = go 0 (zip [0 ..] (0 : depths t))
  where
    n = Text.length sep
    go start [] = [Text.drop start t]
    go start ((i, d) : rest)
        | d == 0
        , i >= start
        , sep `Text.isPrefixOf` Text.drop i t =
            Text.take (i - start) (Text.drop start t) : go (i + n) (dropWhile ((< i + n) . fst) rest)
        | otherwise = go start rest


-- | Whether the plan mentions an output. EXPLAIN qualifies output columns only
-- when the statement reads more than one relation, but qualifies them in
-- conditions regardless, so an unqualified output matches on the column name.
mentionedIn :: Text -> Set Text -> Bool
mentionedIn out set =
    out `Set.member` set
        || (not ("." `Text.isInfixOf` out) && any ((== out) . Text.takeWhileEnd (/= '.')) set)


-- * Checking


data Warning = Warning
    { column :: Int
    -- ^ From 1.
    , columnName :: Text
    , reason :: Text
    }


-- | A warning as a sentence.
describe :: Warning -> String
describe w =
    concat
        [ "column "
        , show w.column
        , " ("
        , Text.unpack w.columnName
        , ") is annotated non-null, but "
        , Text.unpack w.reason
        ]


-- | The name the stripped statement is prepared under, so that @EXPLAIN
-- EXECUTE@ can refer to it.
statementName :: ByteString
statementName = "hasql_check_nullability"


-- | The warnings for a statement that prepares. A statement this cannot take
-- apart, or whose stripped form does not prepare, gets none.
check :: Pqi.Connection -> Catalogue -> Ast.PreparableStmt -> IO [Finding]
check conn catalogue stmt = map toFinding <$> warningsFor conn catalogue stmt
  where
    toFinding w = Finding{rule = Nullable, subject = w.columnName, message = describe w}


warningsFor :: Pqi.Connection -> Catalogue -> Ast.PreparableStmt -> IO [Warning]
warningsFor conn catalogue stmt = case strip stmt of
    Nothing -> pure []
    Just (annotations, stripped) -> do
        prepared <- conn.prepare statementName (renderStmt stripped) Nothing
        status <- maybe (pure Pqi.FatalError) (.resultStatus) prepared
        if status /= Pqi.CommandOk
            then pure []
            else warnings annotations `finally` query conn ("DEALLOCATE " <> statementName)
  where
    warnings annotations =
        conn.describePrepared statementName >>= \case
            Nothing -> pure []
            Just described -> do
                count <- described.nfields
                params <- described.nparams
                explained <- explain params
                if fromIntegral count /= length annotations
                    then pure []
                    else fmap concat . forM (zip [0 :: Int32 ..] annotations) $ \(i, annotation) -> do
                        table <- described.ftable i
                        tableColumn <- described.ftablecol i
                        name <- maybe "?column?" Text.decodeUtf8Lenient <$> described.fname i
                        let planned = case explained of
                                Just (outputs, nullability)
                                    | Just out <- lookup i (zip [0 ..] outputs) ->
                                        ( out `mentionedIn` nullability.outerJoined
                                        , guardsApply && out `mentionedIn` nullability.filteredNotNull
                                        )
                                _ -> (False, False)
                            warn r = [Warning (fromIntegral i + 1) name r]
                        pure case (annotation, planned, Map.lookup (table, tableColumn) catalogue.columns) of
                            (Annotation{nullable = Just False}, (_, True), _) -> []
                            (Annotation{nullable = Just False}, (True, _), _) -> warn "comes from the nullable side of an outer join"
                            (Annotation{nullable = Just False}, _, Just col)
                                | not col.fromView && not col.notNull -> warn (col.qualifiedName <> " is nullable")
                            _ -> []

    -- A filter in the plan is about the rows read. Those are the rows a SELECT
    -- returns, and those a DELETE returns, but an UPDATE returns the rows it
    -- wrote, and an INSERT the rows it made.
    guardsApply = case stmt of
        Ast.SelectPreparableStmt _ -> True
        Ast.DeletePreparableStmt _ -> True
        _ -> False

    -- The top node's outputs, one per result column, and what the plan says
    -- about them. Nothing if EXPLAIN fails.
    explain params = do
        let nulls = ByteString.Char8.intercalate ", " (replicate (fromIntegral params) "NULL")
            args = if params == 0 then "" else ByteString.Char8.cons '(' (ByteString.Char8.snoc nulls ')')
        result <- query conn ("EXPLAIN (VERBOSE, FORMAT JSON) EXECUTE " <> statementName <> args)
        pure case result of
            Right [[Just json]]
                | Right [Explained plan] <- Aeson.eitherDecodeStrict json ->
                    Just (plan.output, planNullability plan)
            _ -> Nothing


-- | Run a statement, returning its rows as text or the server's error.
query :: Pqi.Connection -> ByteString -> IO (Either String [[Maybe ByteString]])
query conn sql =
    conn.exec sql >>= \case
        Nothing -> Left . maybe "no result" (Text.unpack . Text.decodeUtf8Lenient) <$> conn.errorMessage
        Just result -> do
            status <- result.resultStatus
            rows <-
                if status `elem` [Pqi.CommandOk, Pqi.TuplesOk]
                    then do
                        n <- result.ntuples
                        m <- result.nfields
                        Right <$> forM [0 .. n - 1] (forM [0 .. m - 1] . result.getvalue')
                    else Left . maybe "query failed" (Text.unpack . Text.decodeUtf8Lenient) <$> result.resultErrorMessage
            result.unsafeFreeResult
            pure rows
