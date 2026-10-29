{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Each Haskell type stands for one Postgres type, and the statement sends the
-- annotations hasql-th would ask for: @select email from customer@ used as a
-- @Statement Int64 Text@ is sent as @select (email)::text from customer@. A
-- @varchar@ column is then read as 'Text', and an @int4@ one as 'Int64',
-- where an exact decoder would fail.
module Hasql.Check.Inferred (
    -- * Types
    Value (..),
    Column (..),
    Params (..),
    Row (..),

    -- * Statements
    Template (..),
    Wrapped (..),
    Piece (..),
    statement,
    resultless,
    render,

    -- * The checker's view
    known,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.Typeable (Typeable, tyConModule, tyConName, typeRep, typeRepTyCon)
import Data.UUID (UUID)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement)
import Hasql.Statement qualified as Statement
import Language.Haskell.TH.Syntax (Lift)
import Prelude


-- | A Haskell type, and the Postgres type it is sent and read as.
class Value a where
    -- | The Postgres type's name, as an annotation would give it.
    typeName :: Text


    encodeValue :: E.Value a
    decodeValue :: D.Value a


instance Value Bool where
    typeName = "bool"
    encodeValue = E.bool
    decodeValue = D.bool


instance Value Int16 where
    typeName = "int2"
    encodeValue = E.int2
    decodeValue = D.int2


instance Value Int32 where
    typeName = "int4"
    encodeValue = E.int4
    decodeValue = D.int4


instance Value Int64 where
    typeName = "int8"
    encodeValue = E.int8
    decodeValue = D.int8


instance Value Float where
    typeName = "float4"
    encodeValue = E.float4
    decodeValue = D.float4


instance Value Double where
    typeName = "float8"
    encodeValue = E.float8
    decodeValue = D.float8


instance Value Scientific where
    typeName = "numeric"
    encodeValue = E.numeric
    decodeValue = D.numeric


instance Value Text where
    typeName = "text"
    encodeValue = E.text
    decodeValue = D.text


instance Value ByteString where
    typeName = "bytea"
    encodeValue = E.bytea
    decodeValue = D.bytea


instance Value UUID where
    typeName = "uuid"
    encodeValue = E.uuid
    decodeValue = D.uuid


instance Value Day where
    typeName = "date"
    encodeValue = E.date
    decodeValue = D.date


instance Value LocalTime where
    typeName = "timestamp"
    encodeValue = E.timestamp
    decodeValue = D.timestamp


instance Value UTCTime where
    typeName = "timestamptz"
    encodeValue = E.timestamptz
    decodeValue = D.timestamptz


instance Value TimeOfDay where
    typeName = "time"
    encodeValue = E.time
    decodeValue = D.time


instance Value Aeson.Value where
    typeName = "jsonb"
    encodeValue = E.jsonb
    decodeValue = D.jsonb


-- | A column or parameter, which may be NULL when it is a 'Maybe'.
class Column a where
    columnType :: Text
    encodeColumn :: E.NullableOrNot E.Value a
    decodeColumn :: D.NullableOrNot D.Value a


instance {-# OVERLAPPABLE #-} Value a => Column a where
    columnType = typeName @a
    encodeColumn = E.nonNullable encodeValue
    decodeColumn = D.nonNullable decodeValue


instance Value a => Column (Maybe a) where
    columnType = typeName @a
    encodeColumn = E.nullable encodeValue
    decodeColumn = D.nullable decodeValue


-- | A statement's parameters: none, one, or a tuple of them.
class Params a where
    paramTypes :: [Text]
    params :: E.Params a


instance Params () where
    paramTypes = []
    params = E.noParams


instance {-# OVERLAPPABLE #-} Column a => Params a where
    paramTypes = [columnType @a]
    params = E.param encodeColumn


instance (Column a, Column b) => Params (a, b) where
    paramTypes = [columnType @a, columnType @b]
    params =
        (fst >$< E.param encodeColumn)
            <> (snd >$< E.param encodeColumn)


instance (Column a, Column b, Column c) => Params (a, b, c) where
    paramTypes = [columnType @a, columnType @b, columnType @c]
    params =
        ((\(x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d) => Params (a, b, c, d) where
    paramTypes = [columnType @a, columnType @b, columnType @c, columnType @d]
    params =
        ((\(x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e) => Params (a, b, c, d, e) where
    paramTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e]
    params =
        ((\(x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e, Column f) => Params (a, b, c, d, e, f) where
    paramTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e, columnType @f]
    params =
        ((\(x, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g) => Params (a, b, c, d, e, f, g) where
    paramTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e, columnType @f, columnType @g]
    params =
        ((\(x, _, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, _, x) -> x) >$< E.param encodeColumn)


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g, Column h) => Params (a, b, c, d, e, f, g, h) where
    paramTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e, columnType @f, columnType @g, columnType @h]
    params =
        ((\(x, _, _, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, x, _, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, x, _, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, x, _, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, x, _, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, x, _, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, _, x, _) -> x) >$< E.param encodeColumn)
            <> ((\(_, _, _, _, _, _, _, x) -> x) >$< E.param encodeColumn)


-- | A result row: one column, or a tuple of them.
class Row a where
    columnTypes :: [Text]
    row :: D.Row a


instance {-# OVERLAPPABLE #-} Column a => Row a where
    columnTypes = [columnType @a]
    row = D.column decodeColumn


instance (Column a, Column b) => Row (a, b) where
    columnTypes = [columnType @a, columnType @b]
    row = (,) <$> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c) => Row (a, b, c) where
    columnTypes = [columnType @a, columnType @b, columnType @c]
    row = (,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d) => Row (a, b, c, d) where
    columnTypes = [columnType @a, columnType @b, columnType @c, columnType @d]
    row = (,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e) => Row (a, b, c, d, e) where
    columnTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e]
    row = (,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e, Column f) => Row (a, b, c, d, e, f) where
    columnTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e, columnType @f]
    row = (,,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g) => Row (a, b, c, d, e, f, g) where
    columnTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e, columnType @f, columnType @g]
    row = (,,,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


instance (Column a, Column b, Column c, Column d, Column e, Column f, Column g, Column h) => Row (a, b, c, d, e, f, g, h) where
    columnTypes = [columnType @a, columnType @b, columnType @c, columnType @d, columnType @e, columnType @f, columnType @g, columnType @h]
    row = (,,,,,,,) <$> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn <*> D.column decodeColumn


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
statement :: forall p r a. (Params p, Row r) => Template -> (D.Row r -> D.Result a) -> Statement p a
statement template result =
    Statement.preparable (render (paramTypes @p) (columnTypes @r) template) params (result row)


-- | A statement returning no rows.
resultless :: forall p. Params p => Template -> Statement p ()
resultless template = Statement.preparable (render (paramTypes @p) [] template) params D.noResult


-- | Each Haskell type the checker knows the Postgres type of, by its
-- qualified name.
known :: [(Text, Text)]
known =
    [ entry @Bool
    , entry @Int16
    , entry @Int32
    , entry @Int64
    , entry @Float
    , entry @Double
    , entry @Scientific
    , entry @Text
    , entry @ByteString
    , entry @UUID
    , entry @Day
    , entry @LocalTime
    , entry @UTCTime
    , entry @TimeOfDay
    , entry @Aeson.Value
    ]
  where
    entry :: forall a. (Typeable a, Value a) => (Text, Text)
    entry =
        let tc = typeRepTyCon (typeRep (Proxy @a))
         in (Text.pack (tyConModule tc <> "." <> tyConName tc), typeName @a)
