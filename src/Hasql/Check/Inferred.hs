{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- 'statement' and 'resultless' name their Postgres types through equalities
-- they do not need themselves: with them, GHC works the names out where a
-- statement is used, which is what puts them in the .hie files.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Hasql.Check.Inferred
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Statements whose encoders and decoders are chosen by the Haskell types they
-- are used with, for the quoters in "Hasql.Check.Inferred.TH".
--
-- Each Haskell type stands for one Postgres type, named at the type level by
-- 'PgType', and the statement sends the annotations hasql-th would ask for:
-- @select email from customer@ used as a @Statement Int64 Text@ is sent as
-- @select (email)::text from customer@. A @varchar@ column is then read as
-- 'Text', and an @int4@ one as 'Int64', where an exact decoder would fail.
--
-- Because the names are types, GHC works them out where a statement is used,
-- and records them in its @.hie@ files, where @hasql-check-hie@ reads them:
-- a @newtype@ deriving 'Value' is checked as the type it wraps, with no list
-- of types to keep in step.
module Hasql.Check.Inferred (
    -- * Types
    Value (..),
    typeName,
    Domain (..),
    PgEnum (..),
    GEnum (..),
    qualifiedName,
    Column (..),
    Params (..),
    Row (..),

    -- * Postgres types at the type level
    PgColumnType,
    PgColumnLabels,
    PgParamLabels,
    PgColumnLabelLists,
    PostgresParamLabels,
    PostgresColumnLabels,
    PgParamTypes,
    PgColumnTypes,
    KnownSymbols (..),
    PostgresParams,
    PostgresColumns,

    -- * Statements
    Template (..),
    Wrapped (..),
    Piece (..),
    statement,
    resultless,
    render,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import GHC.Generics (C1, D1, Generic (..), M1 (..), Meta (..), U1 (..), (:+:) (..))
import GHC.TypeLits (AppendSymbol, KnownSymbol, Symbol, symbolVal)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement)
import Hasql.Statement qualified as Statement
import Language.Haskell.TH.Syntax (Lift)
import PostgresqlSyntax qualified as Ast
import Prelude


-- | A Haskell type, and the Postgres type it is sent and read as.
--
-- A @newtype@ can derive the instance of the type it wraps:
--
-- > newtype Email = Email Text
-- >     deriving newtype (Value)
class Value a where
    -- | The Postgres type's name, as an annotation would give it: @"int8"@,
    -- @"public.payment_state"@.
    type PgType a :: Symbol


    -- | The labels of the Postgres enum it is, if it is one, for the checker.
    type PgLabels a :: [Symbol]


    type PgLabels a = '[]


    encodeValue :: E.Value a
    decodeValue :: D.Value a


-- | The name of a type's Postgres type.
typeName :: forall a. KnownSymbol (PgType a) => Text
typeName = Text.pack (symbolVal (Proxy @(PgType a)))


instance Value Bool where
    type PgType Bool = "bool"
    encodeValue = E.bool
    decodeValue = D.bool


instance Value Int16 where
    type PgType Int16 = "int2"
    encodeValue = E.int2
    decodeValue = D.int2


instance Value Int32 where
    type PgType Int32 = "int4"
    encodeValue = E.int4
    decodeValue = D.int4


instance Value Int64 where
    type PgType Int64 = "int8"
    encodeValue = E.int8
    decodeValue = D.int8


instance Value Float where
    type PgType Float = "float4"
    encodeValue = E.float4
    decodeValue = D.float4


instance Value Double where
    type PgType Double = "float8"
    encodeValue = E.float8
    decodeValue = D.float8


instance Value Scientific where
    type PgType Scientific = "numeric"
    encodeValue = E.numeric
    decodeValue = D.numeric


instance Value Text where
    type PgType Text = "text"
    encodeValue = E.text
    decodeValue = D.text


instance Value ByteString where
    type PgType ByteString = "bytea"
    encodeValue = E.bytea
    decodeValue = D.bytea


instance Value UUID where
    type PgType UUID = "uuid"
    encodeValue = E.uuid
    decodeValue = D.uuid


instance Value Day where
    type PgType Day = "date"
    encodeValue = E.date
    decodeValue = D.date


instance Value LocalTime where
    type PgType LocalTime = "timestamp"
    encodeValue = E.timestamp
    decodeValue = D.timestamp


instance Value UTCTime where
    type PgType UTCTime = "timestamptz"
    encodeValue = E.timestamptz
    decodeValue = D.timestamptz


instance Value TimeOfDay where
    type PgType TimeOfDay = "time"
    encodeValue = E.time
    decodeValue = D.time


instance Value Aeson.Value where
    type PgType Aeson.Value = "jsonb"
    encodeValue = E.jsonb
    decodeValue = D.jsonb


-- | A type sent and read as the one it wraps, under the name of a Postgres
-- domain over it, so that Postgres checks the domain's constraints:
--
-- > newtype Cents = Cents Int64
-- >     deriving (Value) via Domain "cents" Int64
newtype Domain (name :: Symbol) a = Domain a


instance Value a => Value (Domain name a) where
    type PgType (Domain name a) = name
    type PgLabels (Domain name a) = PgLabels a
    encodeValue = (\(Domain a) -> a) >$< encodeValue
    decodeValue = Domain <$> decodeValue


-- | A Haskell enumeration sent and read as a Postgres enum, by the enum's
-- name: each constructor, which takes no fields, as the label of the same
-- name.
--
-- > data PaymentState = PENDING | PAID
-- >     deriving stock (Generic)
-- >     deriving (Value) via PgEnum "payment_state" PaymentState
--
-- The checker compares the labels with the enum's in the database.
newtype PgEnum (name :: Symbol) a = PgEnum a


instance (KnownSymbol name, Generic a, GEnum (Rep a)) => Value (PgEnum name a) where
    type PgType (PgEnum name a) = name
    type PgLabels (PgEnum name a) = ConstructorNames (Rep a)
    encodeValue = uncurry E.enum (qualifiedName (Text.pack (symbolVal (Proxy @name)))) (\(PgEnum a) -> glabel (from a))
    decodeValue = uncurry D.enum (qualifiedName (Text.pack (symbolVal (Proxy @name)))) (\t -> PgEnum . to <$> lookup t gvalues)


-- | A type's schema, if its name has one, and its own name, as the
-- identifiers they are: the name is SQL, as a cast takes it, so a quoted
-- part is taken as it is and an unquoted one in lower case, as Postgres
-- takes them.
qualifiedName :: Text -> (Maybe Text, Text)
qualifiedName n = case Ast.parse mempty n of
    Right (Ast.Typename _ (Ast.GenericTypeSimpleTypename (Ast.GenericType i attrs _)) _ _) ->
        -- The last part is the type, and the one before it its schema; a
        -- part before that is the database, which must be the one connected
        -- to.
        case reverse (i : maybe [] (\(Ast.Attrs as) -> toList as) attrs) of
            own : schema : _ -> (Just (ident schema), ident own)
            own : _ -> (Nothing, ident own)
            [] -> (Nothing, n)
    _ -> (Nothing, n)
  where
    ident = \case
        Ast.QuotedIdent t -> t
        Ast.UnquotedIdent t -> t


-- | The constructors of an enumeration, by name.
class GEnum f where
    gvalues :: [(Text, f p)]
    glabel :: f p -> Text


instance GEnum f => GEnum (D1 m f) where
    gvalues = [(l, M1 x) | (l, x) <- gvalues]
    glabel (M1 x) = glabel x


instance (GEnum f, GEnum g) => GEnum (f :+: g) where
    gvalues = [(l, L1 x) | (l, x) <- gvalues] <> [(l, R1 x) | (l, x) <- gvalues]
    glabel = \case
        L1 x -> glabel x
        R1 x -> glabel x


instance KnownSymbol label => GEnum (C1 ('MetaCons label fixity strict) U1) where
    gvalues = [(Text.pack (symbolVal (Proxy @label)), M1 U1)]
    glabel _ = Text.pack (symbolVal (Proxy @label))


-- | An enumeration's constructor names.
type family ConstructorNames (f :: Type -> Type) :: [Symbol] where
    ConstructorNames (D1 m f) = ConstructorNames f
    ConstructorNames (f :+: g) = Append (ConstructorNames f) (ConstructorNames g)
    ConstructorNames (C1 ('MetaCons label fixity strict) U1) = '[label]


type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys


-- | A one-dimensional array, whose elements may be NULL when they are
-- 'Maybe's: a @Vector Int64@ is an @int8[]@.
instance Column a => Value (Vector a) where
    type PgType (Vector a) = AppendSymbol (PgColumnType a) "[]"
    type PgLabels (Vector a) = PgColumnLabels a
    encodeValue = E.foldableArray encodeColumn
    decodeValue = D.vectorArray decodeColumn


-- | A one-dimensional array, as a list. A 'String' is not one: 'Char' has no
-- Postgres type; use 'Text'.
instance Column a => Value [a] where
    type PgType [a] = AppendSymbol (PgColumnType a) "[]"
    type PgLabels [a] = PgColumnLabels a
    encodeValue = E.foldableArray encodeColumn
    decodeValue = D.listArray decodeColumn


-- | A column or parameter, which may be NULL when it is a 'Maybe'.
class Column a where
    encodeColumn :: E.NullableOrNot E.Value a
    decodeColumn :: D.NullableOrNot D.Value a


instance {-# OVERLAPPABLE #-} Value a => Column a where
    encodeColumn = E.nonNullable encodeValue
    decodeColumn = D.nonNullable decodeValue


instance Value a => Column (Maybe a) where
    encodeColumn = E.nullable encodeValue
    decodeColumn = D.nullable decodeValue


-- | A column's enum labels, if it is one.
type family PgColumnLabels (a :: Type) :: [Symbol] where
    PgColumnLabels (Maybe a) = PgLabels a
    PgColumnLabels a = PgLabels a


-- | A column's Postgres type.
type family PgColumnType (a :: Type) :: Symbol where
    PgColumnType (Maybe a) = PgType a
    PgColumnType a = PgType a


-- | A statement's parameters: none, one, or a tuple of them.
class Params a where
    params :: E.Params a


instance Params () where
    params = E.noParams


instance {-# OVERLAPPABLE #-} Column a => Params a where
    params = E.param encodeColumn


instance (Column a, Column b) => Params (a, b) where
    params =
        (fst >$< E.param encodeColumn)
            <> (snd >$< E.param encodeColumn)


instance (Column a, Column b, Column c) => Params (a, b, c) where
    params =
        ((\(x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d) => Params (a, b, c, d) where
    params =
        ((\(x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e) => Params (a, b, c, d, e) where
    params =
        ((\(x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e, Column f) => Params (a, b, c, d, e, f) where
    params =
        ((\(x, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g) => Params (a, b, c, d, e, f, g) where
    params =
        ((\(x, _, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g, Column h) => Params (a, b, c, d, e, f, g, h) where
    params =
        ((\(x, _, _, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, _, _, x) -> x) >$< E.param encodeColumn)


-- | The Postgres types of a statement's parameters.
type family PgParamTypes (p :: Type) :: [Symbol] where
    PgParamTypes () = '[]
    PgParamTypes (a, b) = '[PgColumnType a, PgColumnType b]
    PgParamTypes (a, b, c) = '[PgColumnType a, PgColumnType b, PgColumnType c]
    PgParamTypes (a, b, c, d) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d]
    PgParamTypes (a, b, c, d, e) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e]
    PgParamTypes (a, b, c, d, e, f) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e, PgColumnType f]
    PgParamTypes (a, b, c, d, e, f, g) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e, PgColumnType f, PgColumnType g]
    PgParamTypes (a, b, c, d, e, f, g, h) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e, PgColumnType f, PgColumnType g, PgColumnType h]
    PgParamTypes a = '[PgColumnType a]


-- | The enum labels of a statement's parameters, one list each.
type family PgParamLabels (p :: Type) :: [[Symbol]] where
    PgParamLabels () = '[]
    PgParamLabels (a, b) = '[PgColumnLabels a, PgColumnLabels b]
    PgParamLabels (a, b, c) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c]
    PgParamLabels (a, b, c, d) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d]
    PgParamLabels (a, b, c, d, e) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e]
    PgParamLabels (a, b, c, d, e, f) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e, PgColumnLabels f]
    PgParamLabels (a, b, c, d, e, f, g) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e, PgColumnLabels f, PgColumnLabels g]
    PgParamLabels (a, b, c, d, e, f, g, h) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e, PgColumnLabels f, PgColumnLabels g, PgColumnLabels h]
    PgParamLabels a = '[PgColumnLabels a]


-- | A result row: one column, or a tuple of them.
class Row a where
    row :: D.Row a


instance {-# OVERLAPPABLE #-} Column a => Row a where
    row = D.column decodeColumn


instance (Column a, Column b) => Row (a, b) where
    row = (,) <$> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c) => Row (a, b, c) where
    row = (,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d) => Row (a, b, c, d) where
    row = (,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e) => Row (a, b, c, d, e) where
    row = (,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e, Column f) => Row (a, b, c, d, e, f) where
    row = (,,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g) => Row (a, b, c, d, e, f, g) where
    row = (,,,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g, Column h) => Row (a, b, c, d, e, f, g, h) where
    row = (,,,,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


-- | The Postgres types of a statement's result columns.
type family PgColumnTypes (r :: Type) :: [Symbol] where
    PgColumnTypes (a, b) = '[PgColumnType a, PgColumnType b]
    PgColumnTypes (a, b, c) = '[PgColumnType a, PgColumnType b, PgColumnType c]
    PgColumnTypes (a, b, c, d) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d]
    PgColumnTypes (a, b, c, d, e) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e]
    PgColumnTypes (a, b, c, d, e, f) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e, PgColumnType f]
    PgColumnTypes (a, b, c, d, e, f, g) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e, PgColumnType f, PgColumnType g]
    PgColumnTypes (a, b, c, d, e, f, g, h) = '[PgColumnType a, PgColumnType b, PgColumnType c, PgColumnType d, PgColumnType e, PgColumnType f, PgColumnType g, PgColumnType h]
    PgColumnTypes a = '[PgColumnType a]


-- | The enum labels of a statement's result columns, one list each.
type family PgColumnLabelLists (r :: Type) :: [[Symbol]] where
    PgColumnLabelLists (a, b) = '[PgColumnLabels a, PgColumnLabels b]
    PgColumnLabelLists (a, b, c) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c]
    PgColumnLabelLists (a, b, c, d) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d]
    PgColumnLabelLists (a, b, c, d, e) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e]
    PgColumnLabelLists (a, b, c, d, e, f) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e, PgColumnLabels f]
    PgColumnLabelLists (a, b, c, d, e, f, g) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e, PgColumnLabels f, PgColumnLabels g]
    PgColumnLabelLists (a, b, c, d, e, f, g, h) = '[PgColumnLabels a, PgColumnLabels b, PgColumnLabels c, PgColumnLabels d, PgColumnLabels e, PgColumnLabels f, PgColumnLabels g, PgColumnLabels h]
    PgColumnLabelLists a = '[PgColumnLabels a]


-- | Type-level names, as text.
class KnownSymbols (ss :: [Symbol]) where
    symbolTexts :: [Text]


instance KnownSymbols '[] where
    symbolTexts = []


instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
    symbolTexts = Text.pack (symbolVal (Proxy @s)) : symbolTexts @ss


-- | The Postgres types of a statement's parameters, as GHC worked them out
-- where it is used. A class of its own, rather than 'KnownSymbols' itself,
-- so that @hasql-check-hie@ finds the whole list in a @.hie@ file, and not
-- only the tails 'KnownSymbols' is built from.
class KnownSymbols ps => PostgresParams ps


instance KnownSymbols ps => PostgresParams ps


-- | The Postgres types of a statement's result columns; see 'PostgresParams'.
class KnownSymbols cs => PostgresColumns cs


instance KnownSymbols cs => PostgresColumns cs


-- | The enum labels of a statement's parameters, as GHC worked them out where
-- it is used, for @hasql-check-hie@ to find; see 'PostgresParams'.
class PostgresParamLabels (ls :: [[Symbol]])


instance PostgresParamLabels ls


-- | The enum labels of a statement's result columns; see 'PostgresParams'.
class PostgresColumnLabels (ls :: [[Symbol]])


instance PostgresColumnLabels ls


-- | A statement's text, as the quoter cut it.
--
-- Its result columns are cast in a projection around it,
--
-- > WITH <its own WITH list,> r (c1, c2) AS (<the statement>) SELECT c1 :: int8, c2 :: text FROM r
--
-- rather than where they are selected, so that the statement means what it
-- did: a cast column is another expression to DISTINCT ON, ORDER BY and
-- GROUP BY, and no longer the primary key other columns depend on. The
-- statement's own WITH list goes first, since one with a data-modifying
-- statement must stay at the top.
data Template = Template
    { whole :: [Piece]
    -- ^ The statement, for one returning no rows.
    , wrapped :: Maybe Wrapped
    -- ^ Its parts, unless it cannot be put in a WITH list: a CALL.
    }
    deriving stock (Eq, Show, Lift)


data Wrapped = Wrapped
    { with :: Maybe [Piece]
    -- ^ Its own WITH list, with the WITH.
    , body :: [Piece]
    -- ^ The rest of it.
    , name :: Text
    -- ^ A name for it that it does not use itself.
    }
    deriving stock (Eq, Show, Lift)


-- | Some SQL, or the place of a parameter's annotation.
data Piece
    = Sql Text
    | -- | The annotation of the nth parameter, counting from 1.
      ParamType Int
    deriving stock (Eq, Show, Lift)


-- | The statement's text, given the Postgres types of its parameters and
-- result columns.
render :: [Text] -> [Text] -> Template -> Text
render paramTypes' columnTypes' t = case (columnTypes', t.wrapped) of
    (_ : _, Just w) ->
        mconcat
            [ maybe "WITH " (\with -> fill with <> ", ") w.with
            , w.name
            , " ("
            , Text.intercalate ", " columns
            , ") AS ("
            , fill w.body
            , ") SELECT "
            , Text.intercalate ", " (zipWith (\c ty -> c <> " :: " <> ty) columns columnTypes')
            , " FROM "
            , w.name
            ]
    _ -> fill t.whole
  where
    columns = ["c" <> Text.pack (show i) | i <- [1 .. length columnTypes']]
    fill = foldMap \case
        Sql sql -> sql
        ParamType n -> case drop (n - 1) paramTypes' of
            ty : _ | n >= 1 -> " :: " <> ty
            _ -> ""


-- | A statement returning rows, given what to make of them.
statement ::
    forall p r a ps cs pl cl.
    ( Params p
    , Row r
    , ps ~ PgParamTypes p
    , cs ~ PgColumnTypes r
    , PostgresParams ps
    , PostgresColumns cs
    , pl ~ PgParamLabels p
    , cl ~ PgColumnLabelLists r
    , PostgresParamLabels pl
    , PostgresColumnLabels cl
    ) =>
    Template ->
    (D.Row r -> D.Result a) ->
    Statement p a
statement template result =
    Statement.preparable (render (symbolTexts @ps) (symbolTexts @cs) template) params (result row)


-- | A statement returning no rows.
resultless ::
    forall p ps pl.
    (Params p, ps ~ PgParamTypes p, PostgresParams ps, pl ~ PgParamLabels p, PostgresParamLabels pl) =>
    Template ->
    Statement p ()
resultless template = Statement.preparable (render (symbolTexts @ps) [] template) params D.noResult
