{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Every shape the inferred quoters must take. Nothing runs: the types are
the assertion, and a quoter that builds the wrong row, or reads a column at
the wrong type, fails to compile.
-}
module Inferred.Shapes where

import Data.Int (Int64)
#ifdef KRONOR_SYNTAX
import Data.Int (Int32)
import GHC.Generics (Generic)
#endif
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Check.Inferred (Value)
import Hasql.Check.Inferred.TH
import Hasql.Statement (Statement)
import Prelude

-- A plain tuple row, as before.
plain :: Statement Int64 (Maybe (Text, Maybe Text))
plain = [maybeStatement| select email, nickname from customer where id = $1 |]

-- A row of more than eight columns, past the old tuple cap.
wide :: Statement () (Vector (Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64))
wide = [vectorStatement| select a, b, c, d, e, f, g, h, i, j from wide |]

-- Seventeen parameters, the widest statement in the repository.
manyParams :: Statement (Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64) ()
manyParams =
    [resultlessStatement|
        insert into wide
            ( a
            , b
            , c
            , d
            , e
            , f
            , g
            , h
            , i
            , j
            , k
            , l
            , m
            , n
            , o
            , p
            , q
            )
        values
            ( $1
            , $2
            , $3
            , $4
            , $5
            , $6
            , $7
            , $8
            , $9
            , $10
            , $11
            , $12
            , $13
            , $14
            , $15
            , $16
            , $17
            )
    |]

#ifdef KRONOR_SYNTAX

data CartItem = CartItem
    { productId :: Text
    , quantity :: Int32
    , title :: Maybe Text
    }
    deriving stock (Show, Eq)


-- A record target, with its fields deliberately out of declaration order:
-- they are assigned by name, so a positional quoter would not compile.
record :: Statement Int64 (Vector CartItem)
record =
    [vectorStatement|
        select
            $CartItem
                { title = title
                , productId = product_id
                , quantity = quantity
                }
        from cart_item
        where cart_id = $1
    |]


-- A constructor applied positionally, and nested inside a record.
nested :: Statement () (Maybe (Maybe Text))
nested = [maybeStatement| select $Just (name) from account |]


data Payload = Payload
    { reportId :: Int64
    , orgId :: Int64
    }
    deriving stock (Generic)


-- A param field: one placeholder per field of a record parameter.
paramField :: Statement (Payload, Text) ()
paramField =
    [resultlessStatement|
        update report
        set link = $2
        where id = $1.$reportId
          and organization_id = $1.$orgId
    |]


#endif

-- rowsAffected, which had no inferred quoter.
affected :: Statement Int64 Int64
affected = [rowsAffectedStatement| update session set last_seen = now() where id = $1 |]

-- A star, whose columns only the Haskell type can count.
star :: Statement () (Vector (Int64, Text))
star = [vectorStatement| select * from pair |]

newtype Email = Email Text
    deriving newtype (Value)

-- A newtype column, read as the type it wraps.
newtyped :: Statement Int64 Email
newtyped = [singletonStatement| select email from customer where id = $1 |]
