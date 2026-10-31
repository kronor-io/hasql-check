{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Hasql.Check.Inferred.TH
Copyright   : (c) 2026 Kronor.io

License     : BSD-3-Clause
Maintainer  : lorenzo@kronor.io
Stability   : experimental
Portability : GHC

Quoters for SQL without annotations: the Haskell types a statement is used
with decide its parameter and result types.

> profile :: Statement Int64 (Maybe (Text, Maybe Text))
> profile = [maybeStatement| select email, nickname from customer where id = $1 |]

sends

> WITH hasql_check_result (c1, c2) AS (SELECT email, nickname FROM customer WHERE id = $1 :: int8)
> SELECT c1 :: text, c2 :: text FROM hasql_check_result

Kronor's Haskell targets and @$1.$field@ param fields work too: the quoter
builds the row decoder and the parameter encoder out of them, and the column
types come from the constructor's fields rather than from a tuple.

GHC can only tell whether those types fit the schema with a database; see
the README for checking them with @hasql-check --inferred@.
-}
module Hasql.Check.Inferred.TH (
    singletonStatement,
    maybeStatement,
    vectorStatement,
    resultlessStatement,
    rowsAffectedStatement,
    Parsed (..),
    Slot (..),
    Tgt (..),
    parsed,
    template,
) where

import Data.Char (isAlphaNum)
import Data.List (foldl1')
import Data.Maybe (fromMaybe)
#ifdef KRONOR_SYNTAX
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
#endif
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Records (getField)
import Hasql.Check (parseForkStmt, parsePlainStmt, renderStmt)
import Hasql.Check.Annotate (annotate, placeholder)
import Hasql.Check.Inferred (
    Piece (..),
    Template (..),
    Wrapped (..),
    resultlessWith,
    rowsAffectedWith,
    slotsAp,
    slotsNone,
    slotsOne,
    statementRow,
    statementWith,
    targetAp,
    targetColumn,
    targetPure,
 )
import Hasql.Decoders qualified as D
import Language.Haskell.TH (Exp, Q, appTypeE, conE, litT, mkName, newName, strTyLit, varE)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import PostgresqlSyntax qualified as Ast
import Text.Read (readMaybe)
import Prelude

{- | One SQL placeholder: which Haskell parameter it comes from, and which of
its fields, for Kronor's @$1.$field@.
-}
data Slot = Slot Int (Maybe Text)
    deriving stock (Eq, Show)

-- | A result column, or a Haskell constructor applied to some.
data Tgt
    = TgtColumn
    | -- | A constructor applied to its arguments in order.
      TgtApply Text [Tgt]
    | {- | A constructor whose fields are named, so the statement may write
      them in any order.
      -}
      TgtRecord Text [(Text, Tgt)]
    deriving stock (Eq, Show)

-- | Everything the quoter reads off a statement.
data Parsed = Parsed
    { parsedTemplate :: Template
    , parsedSlots :: [Slot]
    -- ^ One per SQL placeholder, in the order they are sent.
    , parsedArity :: Int
    -- ^ How many parameters the statement takes in Haskell.
    , parsedUsed :: [Int]
    {- ^ The parameters the statement actually refers to, numbered as it was
    written: lowering renumbers the placeholders, so this is not the same
    as the slots' positions.
    -}
    , parsedTargets :: Maybe [Tgt]
    -- ^ Its result columns, unless they cannot be counted here: a @*@.
    }
    deriving stock (Eq, Show)

quoter :: (Parsed -> Q Exp) -> QuasiQuoter
quoter make =
    QuasiQuoter
        { quoteExp = either fail make . parsed . Text.pack
        , quotePat = const (fail "Not supported in patterns")
        , quoteType = const (fail "Not supported in types")
        , quoteDec = const (fail "Not supported in declarations")
        }

-- | A quoter for a statement that returns rows, given what to make of them.
rowQuoter :: Q Exp -> QuasiQuoter
rowQuoter result = quoter \p -> do
    let t = lift p.parsedTemplate
        s = slotsExp p
    case p.parsedTargets of
        Just ts -> [|statementWith $t $s $(targetsExp ts) $result|]
        Nothing -> [|statementRow $t $s $result|]

singletonStatement, maybeStatement, vectorStatement, resultlessStatement, rowsAffectedStatement :: QuasiQuoter
singletonStatement = rowQuoter [|D.singleRow|]
maybeStatement = rowQuoter [|D.rowMaybe|]
vectorStatement = rowQuoter [|D.rowVector|]
resultlessStatement = quoter \p -> [|resultlessWith $(lift p.parsedTemplate) $(slotsExp p)|]
rowsAffectedStatement = quoter \p -> [|rowsAffectedWith $(lift p.parsedTemplate) $(slotsExp p)|]

{- | The statement's parameter encoder: one slot per placeholder, each picking
its parameter out of the tuple, and its field out of that.
-}
slotsExp :: Parsed -> Q Exp
slotsExp p = case p.parsedSlots of
    [] -> [|slotsNone|]
    ss -> foldl1' (\a b -> [|slotsAp $a $b|]) (map (slotExp p.parsedArity) ss)

slotExp :: Int -> Slot -> Q Exp
slotExp arity (Slot n field) = do
    y <- newName "y"
    -- Wildcards elsewhere: an unused binding warns in the splice's module.
    let pat
            | arity <= 1 = TH.varP y
            | otherwise = TH.tupP [if i == n then TH.varP y else TH.wildP | i <- [1 .. arity]]
        body = case field of
            Nothing -> varE y
            Just f -> [|$(appTypeE (varE 'getField) (litT (strTyLit (Text.unpack f)))) $(varE y)|]
    [|slotsOne $(TH.lamE [pat] body)|]

-- | The statement's row decoder, built out of its target list.
targetsExp :: [Tgt] -> Q Exp
targetsExp = \case
    [t] -> targetExp t
    ts -> applied [|targetPure $(conE (TH.tupleDataName (length ts)))|] ts

targetExp :: Tgt -> Q Exp
targetExp = \case
    TgtColumn -> [|targetColumn|]
    TgtApply ctor args -> applied [|targetPure $(conE (mkName (Text.unpack ctor)))|] args
    TgtRecord ctor fields -> do
        -- Bound in the statement's order and assigned by name: applying the
        -- constructor positionally would cross the values over.
        xs <- traverse (const (newName "c")) fields
        let make =
                TH.lamE
                    (map TH.varP xs)
                    (TH.recConE (mkName (Text.unpack ctor)) [TH.fieldExp (mkName (Text.unpack n)) (varE x) | ((n, _), x) <- zip fields xs])
        applied [|targetPure $make|] (map snd fields)

applied :: Q Exp -> [Tgt] -> Q Exp
applied = foldl \f a -> [|targetAp $f $(targetExp a)|]

{- | Everything the quoters need: the statement cut where its parameters'
annotations go, which parameter each placeholder sends, and the shape of
its result row.
-}
parsed :: Text -> Either String Parsed
parsed sql = do
    forked <- parseForkStmt sql
    -- The targets are read off the lowered statement, before they are erased
    -- for the one that is sent.
    let (sent, lowered, slots') = lowerStmt forked
    t <- template (Text.decodeUtf8Lenient (renderStmt sent))
    let (slots, arity) = fromMaybe (plainSlots t) slots'
        -- Where the statement was lowered the slots carry what each
        -- placeholder was written as; where it was not, the template does.
        used = maybe [i | ParamType i <- t.whole] (\_ -> [n | Slot n _ <- slots]) slots'
    pure
        Parsed
            { parsedTemplate = t
            , parsedSlots = slots
            , parsedArity = arity
            , parsedUsed = used
            , parsedTargets = targets lowered
            }

{- | One slot per placeholder, each sending the whole of its parameter, for a
statement with no param fields to lower.
-}
plainSlots :: Template -> ([Slot], Int)
plainSlots t = ([Slot i Nothing | i <- [1 .. n]], n)
  where
    n = maximum (0 : [i | ParamType i <- t.whole])

#ifdef KRONOR_SYNTAX

-- | The statement as it is sent (lowered, with the Haskell targets erased),
-- as its targets are read (lowered, with them still there), and which
-- parameter each of its placeholders sends.
lowerStmt :: Ast.PreparableStmt -> (Ast.PreparableStmt, Ast.PreparableStmt, Maybe ([Slot], Int))
lowerStmt stmt =
    ( Ast.eraseHaskellTargets lowered
    , lowered
    , -- InputParams maps each ($n, field) to its lowered placeholder.
      Just
        ( [Slot n field | (_, (n, field)) <- Map.toAscList (Map.fromList [(v, k) | (k, v) <- Map.toList inputs])]
        , maximum (0 : [n | ((n, _), _) <- Map.toList inputs])
        )
    )
  where
    (lowered, inputs) = Ast.lower stmt

#else

-- | Upstream hasql-th has no param fields and no targets to erase, so the
-- statement is sent as parsed and its placeholders are its parameters.
lowerStmt :: Ast.PreparableStmt -> (Ast.PreparableStmt, Ast.PreparableStmt, Maybe ([Slot], Int))
lowerStmt stmt = (stmt, stmt, Nothing)

#endif

#ifdef KRONOR_SYNTAX

-- | The outermost SELECT's target list, as Haskell targets: 'Nothing' when it
-- is not a list this can count, such as a @*@, where the Haskell row type
-- decides the columns instead.
targets :: Ast.PreparableStmt -> Maybe [Tgt]
targets = \case
    Ast.SelectPreparableStmt s -> traverse targetOf . NonEmpty.toList =<< selectStmt s
    _ -> Nothing
  where
    selectStmt = \case
        Ast.NoParensSelectStmt (Ast.SelectNoParens _ c _ _ _) -> selectClause c
        Ast.WithParensSelectStmt p -> withParens p
    withParens = \case
        Ast.NoParensSelectWithParens (Ast.SelectNoParens _ c _ _ _) -> selectClause c
        Ast.WithParensSelectWithParens p -> withParens p
    selectClause = \case
        Ast.SimpleSelectSelectClause (Ast.NormalSimpleSelect (Just (Ast.NormalTargeting (Ast.HsTargetList ts))) _ _ _ _ _ _) -> Just ts
        Ast.WithParensSelectClause p -> withParens p
        _ -> Nothing


-- | What a target element reads, or 'Nothing' when it stands for however many
-- columns a table has: a @*@ or a @t.*@, which only the schema can count.
targetOf :: Ast.HsTargetEl -> Maybe Tgt
targetOf = \case
    Ast.SqlTargetEl Ast.AsteriskTargetEl -> Nothing
    Ast.SqlTargetEl (Ast.ExprTargetEl (Ast.CExprAExpr (Ast.ColumnrefCExpr (Ast.Columnref _ (Just (Ast.Indirection is))))))
        | Ast.AllIndirectionEl <- NonEmpty.last is -> Nothing
    Ast.SqlTargetEl _ -> Just TgtColumn
    Ast.HsFuncTargetEl ctor args -> TgtApply ctor <$> traverse targetOf args
    Ast.HsRecTargetEl ctor fields -> TgtRecord ctor <$> traverse (\(Ast.HsFieldEl n t) -> (,) n <$> targetOf t) (NonEmpty.toList fields)

#else

-- | Upstream hasql-th has no Haskell targets, so the Haskell row type always
-- decides the columns.
targets :: Ast.PreparableStmt -> Maybe [Tgt]
targets _ = Nothing

#endif

{- | The statement, rendered as hasql-th would render it, cut where its
parameters' annotations go, and split where its result columns' cast
goes around it.
-}
template :: Text -> Either String Template
template sql = do
    stmt <- parsePlainStmt False sql
    -- Names the statement does not already contain, in a string literal or
    -- as a table, say.
    let plain = Text.toLower (Text.decodeUtf8Lenient (renderStmt stmt))
        unused base = until (not . (`Text.isInfixOf` plain)) (<> "_") base
        prefix = unused "hasql_check_slot_"
        annotated = annotate (\n -> Just (placeholder (prefix <> Text.pack (show n)))) stmt
        cut = cutWith prefix
    pure
        Template
            { whole = cut (Text.decodeUtf8Lenient (renderStmt annotated))
            , wrapped =
                ( \(with, body) ->
                    Wrapped
                        { with = cut . Ast.toText mempty <$> with
                        , body = cut (Text.decodeUtf8Lenient (renderStmt body))
                        , name = unused "hasql_check_result"
                        }
                )
                    <$> splitWith annotated
            }

-- | The statement's own WITH list, and the rest of it.
splitWith :: Ast.PreparableStmt -> Maybe (Maybe Ast.WithClause, Ast.PreparableStmt)
splitWith = \case
    -- "(WITH ... SELECT ...)" is a parenthesised select clause.
    Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens Nothing (Ast.WithParensSelectClause p) s l f)) ->
        let (w, p') = inParens p
         in Just (w, Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens Nothing (Ast.WithParensSelectClause p') s l f)))
    Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens w c s l f)) ->
        Just (w, Ast.SelectPreparableStmt (Ast.NoParensSelectStmt (Ast.SelectNoParens Nothing c s l f)))
    Ast.SelectPreparableStmt (Ast.WithParensSelectStmt s) -> Just (Ast.SelectPreparableStmt . Ast.WithParensSelectStmt <$> inParens s)
    Ast.InsertPreparableStmt (Ast.InsertStmt w a b c d) -> Just (w, Ast.InsertPreparableStmt (Ast.InsertStmt Nothing a b c d))
    Ast.UpdatePreparableStmt (Ast.UpdateStmt w a b c d e) -> Just (w, Ast.UpdatePreparableStmt (Ast.UpdateStmt Nothing a b c d e))
    Ast.DeletePreparableStmt (Ast.DeleteStmt w a b c d) -> Just (w, Ast.DeletePreparableStmt (Ast.DeleteStmt Nothing a b c d))
    Ast.CallPreparableStmt _ -> Nothing

{- | A parenthesised SELECT's WITH list, and the rest of it: the list can go
outside the parentheses, and must, if it has a data-modifying statement.
-}
inParens :: Ast.SelectWithParens -> (Maybe Ast.WithClause, Ast.SelectWithParens)
inParens = \case
    Ast.NoParensSelectWithParens (Ast.SelectNoParens w c s l f) -> (w, Ast.NoParensSelectWithParens (Ast.SelectNoParens Nothing c s l f))
    Ast.WithParensSelectWithParens s -> Ast.WithParensSelectWithParens <$> inParens s

cutWith :: Text -> Text -> [Piece]
cutWith prefix = cut
  where
    cut t = case Text.breakOn (" :: " <> prefix) t of
        (before, "") -> [Sql before | not (Text.null before)]
        (before, rest) ->
            let (slot, after) = Text.span isAlphaNum (Text.drop (Text.length (" :: " <> prefix)) rest)
             in [Sql before | not (Text.null before)] <> piece slot <> cut after

    piece slot = case readMaybe (Text.unpack slot) of
        Just i -> [ParamType i]
        Nothing -> [Sql (" :: " <> prefix <> slot)]
