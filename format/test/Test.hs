{-# LANGUAGE CPP #-}

module Main where

import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hasql.ThFormat.SqlFormat (formatSql)
import PostgresqlSyntax qualified as Ast
import System.Exit (ExitCode (..), exitWith)
import Prelude


-- | Parse a SQL snippet, ignoring surrounding whitespace. Returns 'Nothing' if
-- the parser rejects the input. Used to compare the AST of input/expected/actual
-- as a semantic-equivalence guard on top of string equality.
parseAst :: Text -> Maybe Ast.PreparableStmt
parseAst t = either (const Nothing) Just (Ast.parse settings (T.strip t))
  where
    settings =
        mconcat
            [ Ast.nullabilityMarkers True
#ifdef KRONOR_SYNTAX
            , Ast.haskellTargets True
            , Ast.haskellParamFields True
#endif
            ]


data TestCase = TestCase
    { name :: Text
    , qqCol :: Int
    , input :: Text
    , expected :: Text
    }


-- | Prepend @n@ spaces to each non-blank line.
indent :: Int -> Text -> Text
indent n = T.intercalate "\n" . map addPrefix . T.lines
  where
    prefix = T.replicate n " "
    addPrefix line
        | T.null (T.strip line) = line
        | otherwise = prefix <> line


-- | Build a test case. @rawInput@ and @rawExpected@ are dedented SQL snippets;
-- the input is indented to column @qqCol@ (simulating the real quasiquote
-- context), and the expected output is indented to @qqCol + 4@ (the formatter
-- indents content one level deeper than the opening bracket). Both are given a
-- leading newline, matching what a quasiquote block contains.
mkCase :: Text -> Int -> Text -> Text -> TestCase
mkCase nm col rawInput rawExpected =
    TestCase
        { name = nm
        , qqCol = col
        , input = "\n" <> indent col rawInput
        , expected = "\n" <> indent (col + 4) rawExpected
        }


tests :: [TestCase]
tests =
    [ TestCase
        { name = "[01] single-line query (no newlines) is left untouched"
        , qqCol = 4
        , input = " update releaser.config set is_frozen = $1::bool where id = 42 "
        , expected = " update releaser.config set is_frozen = $1::bool where id = 42 "
        }
    , mkCase
        "[02] SELECT ... WHERE: inlined FROM is split out, trailing LIMIT moves to its own line, ANDs are indented under WHERE"
        4
        [__i|
            select response_body::jsonb from history.idempotency
            where customer_id = $1::bigint
                and namespace = $2::text
                and idempotency_key = $3::text
                and request_params = $4::jsonb limit 1
        |]
        [__i|
            select response_body::jsonb
            from history.idempotency
            where customer_id = $1::bigint
              and namespace = $2::text
              and idempotency_key = $3::text
              and request_params = $4::jsonb
            limit 1
        |]
    , mkCase
        "[03] INSERT with 4+ columns and a VALUES tuple: both are split into leading-comma lists"
        4
        [__i|
            insert into history.idempotency (customer_id, namespace, idempotency_key, request_params)
            values ($1::bigint, $2::text, $3::text, $4::jsonb)
        |]
        [__i|
            insert into history.idempotency
                ( customer_id
                , namespace
                , idempotency_key
                , request_params
                )
            values
                ( $1::bigint
                , $2::text
                , $3::text
                , $4::jsonb
                )
        |]
    , mkCase
        "[04] UPDATE ... WHERE normalizes inconsistent AND indentation (2 spaces -> 4) under the WHERE keyword"
        4
        [__i|
            update history.idempotency
            set response_body = $4::jsonb
            where customer_id = $1::bigint
              and namespace = $2::text
              and idempotency_key = $3::text
        |]
        [__i|
            update history.idempotency
            set response_body = $4::jsonb
            where customer_id = $1::bigint
              and namespace = $2::text
              and idempotency_key = $3::text
        |]
    , mkCase
        "[05] JOIN with multi-condition ON: the AND continuation sits at the same indent as ON, not nested further"
        4
        [__i|
            select payment.id::bigint,
                payment.vendor_handle::text?
            from storefront_core.transaction_archive
            join config.vendor_setting
                on vendor_setting.customer_id = transaction_archive.customer_id
            join storefront_core.payment
                on payment.id = transaction_archive.payment_id
                and payment.customer_id = transaction_archive.customer_id
            where transaction_archive.customer_id = $1::bigint
                and transaction_archive.id = $2::bigint
        |]
        [__i|
            select
                payment.id::bigint
              , payment.vendor_handle::text?
            from storefront_core.transaction_archive
            join config.vendor_setting
              on vendor_setting.customer_id = transaction_archive.customer_id
            join storefront_core.payment
              on payment.id = transaction_archive.payment_id
             and payment.customer_id = transaction_archive.customer_id
            where transaction_archive.customer_id = $1::bigint
              and transaction_archive.id = $2::bigint
        |]
#ifdef KRONOR_SYNTAX
    , mkCase
        "[06] hasql-th $CanBeValidated(...) splice is preserved verbatim in a select list"
        4
        [__i|
            select
                $CanBeValidated (api_key::text, 'config.t'::text, 'api_key'::text),
                $CanBeValidated (private_key::text, 'config.t'::text, 'private_key'::text)
            from config.northwind_provider_setting
            where customer_id = $1::bigint
              and id = $2::bigint
        |]
        [__i|
            select
                $CanBeValidated (api_key::text, 'config.t'::text, 'api_key'::text)
              , $CanBeValidated (private_key::text, 'config.t'::text, 'private_key'::text)
            from config.northwind_provider_setting
            where customer_id = $1::bigint
              and id = $2::bigint
        |]
#endif
#ifdef KRONOR_SYNTAX
    , mkCase
        "[07] hasql-th $Name{...} record syntax: inlined braces are reformatted to Haskell-style leading-comma record"
        8
        [__i|
            select $AuthRowData{ isCardSame = x::bool?,
                rowServer = tse.row_server::text,
                country = ae.country::text }
            from app.row_details
            where id = $1::uuid
        |]
        [__i|
            select
                $AuthRowData
                    { isCardSame = x::bool?
                    , rowServer = tse.row_server::text
                    , country = ae.country::text
                    }
            from app.row_details
            where id = $1::uuid
        |]
#endif
#ifdef KRONOR_SYNTAX
    , mkCase
        "[08] hasql-th $1.$field record access survives inside BETWEEN x AND y and coalesce() args"
        8
        [__i|
            select 1
            from t ext
            where ext.org = $1.$orgId::bigint
                and ext.d between $1.$start::date and $1.$end::date
                and ext.acq = coalesce($1.$acq::text?, ext.acq)
        |]
        [__i|
            select 1
            from t ext
            where ext.org = $1.$orgId::bigint
              and ext.d between $1.$start::date and $1.$end::date
              and ext.acq = coalesce($1.$acq::text?, ext.acq)
        |]
#endif
    , mkCase
        "[09] INSERT ... ON CONFLICT DO UPDATE SET puts the updated column assignment on its own indented line"
        4
        [__i|
            insert into t (org_id, batch_id, data)
            select $1::bigint, $2::text, unnest($3::jsonb[])
            on conflict (batch_id, md5(data::text))
            do update set data = excluded.data
        |]
        [__i|
            insert into t
                (org_id, batch_id, data)
            select
                $1::bigint
              , $2::text
              , unnest($3::jsonb[])
            on conflict (batch_id, md5(data::text))
            do update set
                data = excluded.data
        |]
    , mkCase
        "[10] WITH CTE + LEFT JOIN LATERAL fn(...): CTE body is multi-lined, last CTE closes with ')' not '),', ::text? survives inside the lateral function"
        8
        [__i|
            with details as (
                select * from app.row_details
                where id = $1::uuid )
            select details.id::uuid
            from details
            left join lateral fsm.notify ( shard := details.mid, event := $2::text, data := jsonb_build_object( 'error', $3::text? ) )
                on true
        |]
        [__i|
            with details as (
                select *
                from app.row_details
                where id = $1::uuid
            )
            select details.id::uuid
            from details
            left join lateral fsm.notify(
                  shard := details.mid
                , event := $2::text
                , data := jsonb_build_object('error', $3::text?)
            )
              on true
        |]
    , mkCase
        "[11] content inside single-quoted string literals ('U', 'Y', 'FM000') is not lowercased or otherwise touched"
        4
        [__i|
            select (case when x is null then 'U' when x then 'Y' else 'N' end)::text,
                to_char(n, 'FM000')::text
            from t
            where id = $1::uuid
        |]
        [__i|
            select
                (case
                    when x is null then 'U'
                    when x then 'Y'
                    else 'N'
                end)::text
              , to_char(
                  n
                , 'FM000'
            )::text
            from t
            where id = $1::uuid
        |]
    , mkCase
        "[12] chained typecasts like ::text?::uuid? preserve both nullability markers (not just the last)"
        4
        [__i|
            select (x->>'a')::text?,
                (x->>'b')::text?::uuid?,
                (x->>'c')::text?
            from t
            where id = $1::uuid
        |]
        [__i|
            select
                (x->>'a')::text?
              , (x->>'b')::text?::uuid?
              , (x->>'c')::text?
            from t
            where id = $1::uuid
        |]
    , mkCase
        "[13] UPDATE ... SET: assigned expressions keep nullability chains like ::text?::uuid on the right-hand side"
        4
        [__i|
            update app.row_details
            set auth_response = $2::jsonb,
                ds_trans_id = $3::text?::uuid
            where id = $1::uuid
        |]
        [__i|
            update app.row_details
            set
                auth_response = $2::jsonb
              , ds_trans_id = $3::text?::uuid
            where id = $1::uuid
        |]
    , mkCase
        "[14] UNION ALL: both arms are formatted independently with their own multi-line select layout"
        4
        [__i|
            select id::bigint, name::text from table_a where active = true
            union all
            select id::bigint, name::text from table_b where active = true
        |]
        [__i|
            select
                id::bigint
              , name::text
            from table_a
            where active = true
            union all
            select
                id::bigint
              , name::text
            from table_b
            where active = true
        |]
    , mkCase
        "[15] GROUP BY / ORDER BY / LIMIT each land on their own line at the base clause indent after WHERE"
        4
        [__i|
            select provider::text, count(*)::bigint
            from classification.ext
            where org_id = $1::bigint
            group by provider
            order by count(*) desc
            limit 10
        |]
        [__i|
            select
                provider::text
              , count(*)::bigint
            from classification.ext
            where org_id = $1::bigint
            group by provider
            order by count(*) desc
            limit 10
        |]
    , mkCase
        "[16] DELETE FROM with a multi-predicate WHERE: ANDs are split onto their own indented lines"
        4
        [__i|
            delete from history.some_table
            where customer_id = $1::bigint
                and id = $2::bigint
        |]
        [__i|
            delete from history.some_table
            where customer_id = $1::bigint
              and id = $2::bigint
        |]
    , mkCase
        "[17] FROM can be a set-returning function call (not just a table name) without losing the FROM keyword"
        4
        [__i|
            select count(*)::bigint
            from app.set_opts_and_config($1::text, $2::jsonb)
        |]
        [__i|
            select count(*)::bigint
            from app.set_opts_and_config($1::text, $2::jsonb)
        |]
    , mkCase
        "[18] regression: ORDER BY inside an inline subquery is not emitted as 'order by order by' (R.sortClause already includes the keyword)"
        4
        [__i|
            select id::bigint
            from t
            where id = (select id from u order by created_at desc limit 1)
        |]
        [__i|
            select id::bigint
            from t
            where id = (select id from u order by created_at desc limit 1)
        |]
    , mkCase
        "[19] regression: GROUP BY inside an inline subquery is not emitted as 'group by group by' (R.groupClause already includes the keyword)"
        4
        [__i|
            select id::bigint
            from t
            where id = (select max(id) from u group by org_id)
        |]
        [__i|
            select id::bigint
            from t
            where id = (select max(id) from u group by org_id)
        |]
    , mkCase
        "[20] regression: ORDER BY inside an aggregate's argument list (string_agg(x, ',' order by y)) is not doubled"
        4
        [__i|
            select string_agg(name, ', ' order by id)::text
            from t
            where org_id = $1::bigint
        |]
        [__i|
            select string_agg(
                  name
                , ', '
                order by id
            )::text
            from t
            where org_id = $1::bigint
        |]
    , mkCase
        "[21] x = ANY(array_expr) keeps the parens around the array expression (Postgres requires them when the operand isn't a subquery)"
        4
        [__i|
            select id::bigint
            from t
            where customer_id = ANY($1::bigint[])
        |]
        [__i|
            select id::bigint
            from t
            where customer_id = any($1::bigint[])
        |]
    , mkCase
        "[22] CASE WHEN ... THEN ... ELSE ... END arms preserve hasql-th's ::text? nullable marker (don't fall through to the generic renderer)"
        4
        [__i|
            update t
            set x = case when $2::text? = 'Y' then $1::jsonb else x end
            where id = $3::uuid
        |]
        [__i|
            update t
            set x = case when $2::text? = 'Y' then $1::jsonb else x end
            where id = $3::uuid
        |]
    , mkCase
        "[23] FROM with multiple tables and no joins stays on a single line (no 'from t1\\nfrom t2' duplication)"
        4
        [__i|
            select t1.id::bigint
            from t1, t2
            where t1.id = t2.id
        |]
        [__i|
            select t1.id::bigint
            from t1, t2
            where t1.id = t2.id
        |]
    , mkCase
        "[24] nullable array typecast ::text[]? keeps the trailing ? (the second Bool in the array-dims tuple must be rendered)"
        4
        [__i|
            update t
            set statuses = $1::text[]?
            where id = $2::uuid
        |]
        [__i|
            update t
            set statuses = $1::text[]?
            where id = $2::uuid
        |]
#ifdef KRONOR_SYNTAX
    , mkCase
        "[25] array[...] literal preserves hasql-th $1.$field access inside its element expressions"
        4
        [__i|
            select enqueue(array[mk_payload(organization_id => $1.$organizationId::bigint, frequency => $2::text)])::text
        |]
        [__i|
            select enqueue(array[mk_payload(organization_id => $1.$organizationId::bigint, frequency => $2::text)])::text
        |]
#endif
#ifdef KRONOR_SYNTAX
    , mkCase
        "[26] ROW(...) constructor preserves $1.$field access and ::text? nullability on its elements"
        4
        [__i|
            select create_payment(capture_input := ROW($2::bigint, $3::bigint::currency_unit, $4::text?)::capture_input, created_by := $5::text?)
        |]
        [__i|
            select create_payment(
                  capture_input := row($2::bigint, $3::bigint::currency_unit, $4::text?)::capture_input
                , created_by := $5::text?
            )
        |]
#endif
#ifdef KRONOR_SYNTAX
    , mkCase
        "[27] inline subquery's FROM function-call (inside EXISTS (...)) preserves $1.$field in its argument list"
        4
        [__i|
            select exists (select 1::int from gen($1.$orgId::bigint, $1.$cursor::text?))::bool
        |]
        [__i|
            select exists (select 1::int from gen($1.$orgId::bigint, $1.$cursor::text?))::bool
        |]
#endif
#ifdef KRONOR_SYNTAX
    , mkCase
        "[28] UNION ALL inside EXISTS (...) wraps when the inline form gets long; each arm preserves $1.$field"
        4
        [__i|
            select exists (select 1::int from gen1($1.$orgId::bigint, $1.$cursor::text?) union all select 1::int from gen2($1.$orgId::bigint, $1.$cursor::text?))::bool
        |]
        [__i|
            select exists (
                select 1::int
                from gen1($1.$orgId::bigint, $1.$cursor::text?)
                union all
                select 1::int
                from gen2($1.$orgId::bigint, $1.$cursor::text?)
            )::bool
        |]
#endif
#ifdef KRONOR_SYNTAX
    , mkCase
        "[29] EXTRACT(field FROM $1.$field::type) preserves the hasql-th field access inside the extracted expression"
        4
        [__i|
            select extract(epoch from ($2.$expiresAt::timestamptz - now()))
        |]
        [__i|
            select extract(epoch from ($2.$expiresAt::timestamptz - now()))
        |]
#endif
    , mkCase
        "[30] multi-row VALUES emits the 'values' keyword once with comma-separated row tuples (not 'values (...) values (...)')"
        4
        [__i|
            insert into t (a, b, c, d)
            values
                ($1::bigint, 'A', null, 'x'),
                ($1::bigint, 'B', null, 'y')
        |]
        [__i|
            insert into t
                ( a
                , b
                , c
                , d
                )
            values
                ( $1::bigint
                , 'A'
                , null
                , 'x'
                ),
                ( $1::bigint
                , 'B'
                , null
                , 'y'
                )
        |]
    , mkCase
        "[31] LEFT JOIN with multi-condition ON: AND continuations under the ON clause"
        4
        [__i|
            select t1.id::bigint
            from t1
            left join t2 on t1.id = t2.t1_id and t1.kind = t2.kind and t1.org = t2.org
            where t1.id = $1::bigint
        |]
        [__i|
            select t1.id::bigint
            from t1
            left join t2
              on t1.id = t2.t1_id
             and t1.kind = t2.kind
             and t1.org = t2.org
            where t1.id = $1::bigint
        |]
    , mkCase
        "[32] WITH chains multiple CTEs separated by '),' and ends the last with ')'"
        4
        [__i|
            with parsed (parsed_cursor) as (
                select coalesce($1::jsonb, '{}'::jsonb)
            ),
            data as (select 1::int from parsed)
            select 1::int from data
        |]
        [__i|
            with parsed (parsed_cursor) as (
                select coalesce($1::jsonb, '{}'::jsonb)
            ),
            data as (
                select 1::int
                from parsed
            )
            select 1::int
            from data
        |]
    , mkCase
        "[33] CTE with explicit column list: 'with name (col1, col2) as (...)' preserves the column list parens"
        4
        [__i|
            with rolled (a, b) as (
                select 1::int, 2::int
            )
            select rolled.a::int from rolled
        |]
        [__i|
            with rolled (a, b) as (
                select
                    1::int
                  , 2::int
            )
            select rolled.a::int
            from rolled
        |]
    , mkCase
        "[34] CROSS JOIN renders on its own line with no ON/USING qualifier"
        4
        [__i|
            select t1.id::bigint
            from t1
            cross join t2
            where t1.id = $1::bigint
        |]
        [__i|
            select t1.id::bigint
            from t1
            cross join t2
            where t1.id = $1::bigint
        |]
    , mkCase
        "[35] INSERT ... ON CONFLICT DO NOTHING: 'on conflict' and 'do nothing' each go on their own line"
        4
        [__i|
            insert into t (a, b, c)
            values ($1::bigint, $2::text, $3::text)
            on conflict
            do nothing
        |]
        [__i|
            insert into t
                (a, b, c)
            values
                ( $1::bigint
                , $2::text
                , $3::text
                )
            on conflict
            do nothing
        |]
    , mkCase
        "[36] UPDATE ... FROM: the FROM clause goes on its own line between SET and WHERE"
        4
        [__i|
            update t1 set x = t2.y from t2 where t1.id = t2.id and t1.id = $1::bigint
        |]
        [__i|
            update t1
            set x = t2.y
            from t2
            where t1.id = t2.id
              and t1.id = $1::bigint
        |]
    , mkCase
        "[37] DELETE ... USING: the USING clause goes on its own line between DELETE and WHERE"
        4
        [__i|
            delete from t1 using t2 where t1.id = t2.id and t1.id = $1::bigint
        |]
        [__i|
            delete from t1
            using t2
            where t1.id = t2.id
              and t1.id = $1::bigint
        |]
    , mkCase
        "[38] Function call with multiple arguments wraps each argument onto its own line, with closing paren back at the function's column"
        4
        [__i|
            select app.save_records_pre_commit($2::text, $1::jsonb, $3::jsonb, $4::bytea, $5::jsonb)::uuid
        |]
        [__i|
            select app.save_records_pre_commit(
                  $2::text
                , $1::jsonb
                , $3::jsonb
                , $4::bytea
                , $5::jsonb
            )::uuid
        |]
    , mkCase
        "[39] LEFT JOIN LATERAL with a function call that has multiple named args wraps the call onto multiple lines"
        4
        [__i|
            select 1::int
            from t
            left join lateral fsm.notify_state_machine(shard := $1::bigint, machine := $2::bigint, event := 'foo', data := jsonb_build_object('a', $3::text)) on true
        |]
        [__i|
            select 1::int
            from t
            left join lateral fsm.notify_state_machine(
                  shard := $1::bigint
                , machine := $2::bigint
                , event := 'foo'
                , data := jsonb_build_object('a', $3::text)
            )
              on true
        |]
    , mkCase
        "[40] Single-argument function call stays inline (no wrap when args < threshold)"
        4
        [__i|
            select md5($1::text)::text
        |]
        [__i|
            select md5($1::text)::text
        |]
    , mkCase
        "[41] Two-argument function call wraps onto multiple lines (threshold is 2)"
        4
        [__i|
            select to_char(n, 'FM000')::text from t
        |]
        [__i|
            select to_char(
                  n
                , 'FM000'
            )::text
            from t
        |]
    , mkCase
        "[42] string literal containing ' and ' is not split as a boolean AND"
        4
        [__i|
            select id::bigint from t
            where label = 'apple and orange'
              and other = $1::text
        |]
        [__i|
            select id::bigint
            from t
            where label = 'apple and orange'
              and other = $1::text
        |]
    , mkCase
        "[43] string literal containing ' :: ' is preserved verbatim"
        4
        [__i|
            select id::bigint from t
            where note = 'see x :: y for details'
        |]
        [__i|
            select id::bigint
            from t
            where note = 'see x :: y for details'
        |]
    , mkCase
        "[44] FOR UPDATE inside an inline subquery is preserved"
        4
        [__i|
            select id::bigint from t
            where exists (select 1 from u where u.t_id = t.id for update)
        |]
        [__i|
            select id::bigint
            from t
            where exists (select 1 from u where u.t_id = t.id for update)
        |]
    , mkCase
        "[45] Chained typecasts on a function-call select target keep every cast"
        4
        [__i|
            select jsonb_build_object($1::text, $2::text)::text::bytea from t
        |]
        [__i|
            select jsonb_build_object(
                  $1::text
                , $2::text
            )::text::bytea
            from t
        |]
    , mkCase
        "[46] Top-level WINDOW clause is preserved on its own line"
        4
        [__i|
            select row_number() over w::bigint from t window w as (partition by x order by y)
        |]
        [__i|
            select row_number() over w::bigint
            from t
            window w as (partition by x order by y)
        |]
    , mkCase
        "[47] CTE materialization hint (MATERIALIZED) is preserved"
        4
        [__i|
            with cte as materialized (select 1::bigint as x)
            select x::bigint from cte
        |]
        [__i|
            with cte as materialized (
                select 1::bigint as x
            )
            select x::bigint
            from cte
        |]
    , mkCase
        "[48] INSERT ... OVERRIDING SYSTEM VALUE is preserved on its own line"
        4
        [__i|
            insert into t (id, name, kind, source) overriding system value
            select x.id::bigint, x.name::text, x.kind::text, x.source::text from src x
        |]
        [__i|
            insert into t
                ( id
                , name
                , kind
                , source
                )
            overriding system value
            select
                x.id::bigint
              , x.name::text
              , x.kind::text
              , x.source::text
            from src x
        |]
    , mkCase
        "[49] Nested function calls wrap recursively; jsonb_build_object groups (key, value) pairs onto one line when wrapped; trailing args after a wrapped sub-call line up with the leading-comma column"
        4
        [__i|
            select job_queue.enqueue_payload(
                array_agg(
                    job_queue.mk_payload(
                        caller => 'Sample.Vantage.FetchSettlements',
                        provider => 'Vantage',
                        payload => jsonb_build_object(
                            'tag', 'FetchVantageBatchesTag',
                            'contents', jsonb_build_object(
                                'organizationId', inserted.organization_id,
                                'vantageSettlementId', inserted.id,
                                'stateMachineId', $4::bigint
                            )
                        ),
                        run_at => now(),
                        timeout => '1 day'::interval
                    )
                )::jsonb[],
                priority_ := 100
            )::text
            from inserted
        |]
        [__i|
            select job_queue.enqueue_payload(
                  array_agg(
                      job_queue.mk_payload(
                          caller => 'Sample.Vantage.FetchSettlements'
                        , provider => 'Vantage'
                        , payload => jsonb_build_object(
                              'tag', 'FetchVantageBatchesTag'
                            , 'contents', jsonb_build_object(
                                  'organizationId', inserted.organization_id
                                , 'vantageSettlementId', inserted.id
                                , 'stateMachineId', $4::bigint
                            )
                        )
                        , run_at => now()
                        , timeout => '1 day'::interval
                    )
                )::jsonb[]
                , priority_ := 100
            )::text
            from inserted
        |]
    , mkCase
        "[50] EXISTS subquery in a CTE wraps multi-line when the inline form gets too long"
        4
        [__i|
            with customer_note as (
                select exists (select * from storefront_core.customer_note where customer_id = current_setting('app.customer_id')::bigint and customer_reference = current_setting('app.customer_reference')::text and fingerprint = $2::uuid) card_exists
            )
            select (save_card_details and not customer_note.card_exists)::boolean
            from app.row_details, customer_note
            where row_details.customer_id = current_setting('app.customer_id')::bigint
              and row_details.id = $1::uuid
        |]
        [__i|
            with customer_note as (
                select exists (
                    select *
                    from storefront_core.customer_note
                    where customer_id = current_setting('app.customer_id')::bigint
                      and customer_reference = current_setting('app.customer_reference')::text
                      and fingerprint = $2::uuid
                ) card_exists
            )
            select (save_card_details and not customer_note.card_exists)::boolean
            from app.row_details, customer_note
            where row_details.customer_id = current_setting('app.customer_id')::bigint
              and row_details.id = $1::uuid
        |]
    , mkCase
        "[51] SQL keyword-form functions (position / substring / trim / overlay) emit no space before '('"
        4
        [__i|
            select position(' ' in name)::int
                 , substring(s from 1 for 5)::text
                 , substring(s, 1, 5)::text
                 , trim(both ' ' from name)::text
                 , overlay(name placing 'x' from 1 for 1)::text
            from t
            where id = $1::bigint
        |]
        [__i|
            select
                position(' ' in name)::int
              , substring(s from 1 for 5)::text
              , substring(s, 1, 5)::text
              , trim(both ' ' from name)::text
              , overlay(name placing 'x' from 1 for 1)::text
            from t
            where id = $1::bigint
        |]
    , mkCase
        "[52] array[fn(...)] literal whose element is a wrap-eligible call expands the inner call across multiple lines"
        4
        [__i|
            select job_queue.enqueue_payload(
                array[job_queue.mk_payload(caller => 'X', provider => 'PayWallet', payload => jsonb_build_object('tag', 'FetchTag', 'contents', jsonb_build_object('organizationId', $1::bigint, 'ledgerId', $2::bigint, 'cursor', $3::text)))]
                , priority_ := 5000
            )::text
        |]
        [__i|
            select job_queue.enqueue_payload(
                  array[
                      job_queue.mk_payload(
                          caller => 'X'
                        , provider => 'PayWallet'
                        , payload => jsonb_build_object(
                              'tag', 'FetchTag'
                            , 'contents', jsonb_build_object(
                                  'organizationId', $1::bigint
                                , 'ledgerId', $2::bigint
                                , 'cursor', $3::text
                            )
                        )
                    )
                ]
                , priority_ := 5000
            )::text
        |]
    , mkCase
        "[53] FROM-list `from fn(args)` wraps when the function call has multiple args"
        4
        [__i|
            select count(*)::bigint
            from job_queue.enqueue_payload(array[job_queue.mk_payload(customer_id => $1::bigint, caller => 'Foo', payload => jsonb_build_object('tag', 'X', 'contents', jsonb_build_object('orgId', $2::bigint, 'cursor', $3::text)))]::jsonb[], priority_ := 5000)
        |]
        [__i|
            select count(*)::bigint
            from job_queue.enqueue_payload(
                  array[
                      job_queue.mk_payload(
                          customer_id => $1::bigint
                        , caller => 'Foo'
                        , payload => jsonb_build_object(
                              'tag', 'X'
                            , 'contents', jsonb_build_object(
                                  'orgId', $2::bigint
                                , 'cursor', $3::text
                            )
                        )
                    )
                ]::jsonb[]
                , priority_ := 5000
            )
        |]
    , mkCase
        "[54] Single-arg outer call (e.g. unnest) with a long array arg wraps via the select-target length gate"
        4
        [__i|
            select unnest(array[format('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', $1::text), format('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', $2::text)])::text
            from t
        |]
        [__i|
            select unnest(
                  array[
                      format(
                          'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
                        , $1::text
                    )
                    , format(
                          'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
                        , $2::text
                    )
                ]
            )::text
            from t
        |]
    , mkCase
        "[55] (expr)::type — parenthesized wrap-eligible expression with a typecast wraps when too long"
        4
        [__i|
            select (json_build_object('https://hasura.io/jwt/claims', $2::jsonb?, 'iat', extract(epoch from now()), 'exp', extract(epoch from now() + interval '1 hour'), 'tid', $4::text?, 'ttype', $3::text?, 'tname', null, 'associated_email', $1::text?)::jsonb)::json
        |]
        [__i|
            select (
                json_build_object(
                      'https://hasura.io/jwt/claims', $2::jsonb?
                    , 'iat', extract(epoch from now())
                    , 'exp', extract(epoch from now() + interval '1 hour')
                    , 'tid', $4::text?
                    , 'ttype', $3::text?
                    , 'tname', null
                    , 'associated_email', $1::text?
                )::jsonb
            )::json
        |]
    , mkCase
        "[56] FROM unnest(...) AS alias (col1, col2, ..., colN) wraps the column list when it has more than 3 columns"
        4
        [__i|
            select count(*)::bigint from unnest($1::int[]) as txn_row (entry_reference, external_status, currency, amount, transaction_type, name, booking_date, value_date)
        |]
        [__i|
            select count(*)::bigint
            from unnest($1::int[]) as txn_row
                ( entry_reference
                , external_status
                , currency
                , amount
                , transaction_type
                , name
                , booking_date
                , value_date
                )
        |]
#ifdef KRONOR_SYNTAX
    , mkCase
        "[57] hasql-th nested $Record{...} fields expand multi-line when an inner record's inline form is long"
        8
        [__i|
            select $PaymentSource{ wallet = $Wallet{ experienceContext = $ExperienceContext{ brandName = customer.name::text, returnUrl = wallet_payment.return_url::text, cancelUrl = wallet_payment.return_url::text } } }
            from t
            where id = $1::uuid
        |]
        [__i|
            select
                $PaymentSource
                    { wallet = $Wallet
                        { experienceContext = $ExperienceContext
                            { brandName = customer.name::text
                            , returnUrl = wallet_payment.return_url::text
                            , cancelUrl = wallet_payment.return_url::text
                            }
                        }
                    }
            from t
            where id = $1::uuid
        |]
#endif
    , mkCase
        "[58] CROSS JOIN (VALUES ...) with many tuples wraps multi-line; each tuple > 3 cols also expands to bracket-list"
        4
        [__i|
            select m.id::bigint, payment_method.column1::text, payment_method.column2::int
            from config.customer m
            cross join (values ('SUCCESSFUL_WALLET_PAYMENT', 71, 400, 'SEK', 'Wallet payment charge'), ('SUCCESSFUL_DIRECT_DEBIT_PAYMENTS', 72, 300, 'SEK', 'Direct Debit (Open Banking)'), ('SUCCESSFUL_CREDIT_CARD_PAYMENTS', 83, 45, 'SEK', 'Credit card fee')) as payment_method
        |]
        [__i|
            select
                m.id::bigint
              , payment_method.column1::text
              , payment_method.column2::int
            from config.customer m
            cross join (
                values
                    ( 'SUCCESSFUL_WALLET_PAYMENT'
                    , 71
                    , 400
                    , 'SEK'
                    , 'Wallet payment charge'
                    ),
                    ( 'SUCCESSFUL_DIRECT_DEBIT_PAYMENTS'
                    , 72
                    , 300
                    , 'SEK'
                    , 'Direct Debit (Open Banking)'
                    ),
                    ( 'SUCCESSFUL_CREDIT_CARD_PAYMENTS'
                    , 83
                    , 45
                    , 'SEK'
                    , 'Credit card fee'
                    )
            ) as payment_method
        |]
    , mkCase
        "[59] CASE expressions in SET RHS expand multi-line when the inline form gets long; body aligns under the CASE keyword"
        4
        [__i|
            update app.row_details
            set
                challenge_response = case when $3::text? = 'Y' then $2::jsonb else challenge_response end
              , post_auth_response = case when $3::text? = 'Y' then $2::jsonb else post_auth_response end
              , error_response = case when $3::text? != 'Y' or $3::text? is null then $2::jsonb else error_response end
            where id = $1::uuid
        |]
        [__i|
            update app.row_details
            set
                challenge_response = case
                    when $3::text? = 'Y' then $2::jsonb
                    else challenge_response
                end
              , post_auth_response = case
                    when $3::text? = 'Y' then $2::jsonb
                    else post_auth_response
                end
              , error_response = case
                    when $3::text? != 'Y' or $3::text? is null then $2::jsonb
                    else error_response
                end
            where id = $1::uuid
        |]
#ifdef KRONOR_SYNTAX
    , mkCase
        "[60] hasql-th $Foo (...) splice wraps multi-line when inline form is long; SQL inside its args (CASE, parens, casts) wraps the same way it would at the top-level SELECT-target position"
        4
        [__i|
            select
                mobile_charge.id::uuid
              , $CanBeValidated ((case mobile_charge.payment_category when 'StandAlone' then (select client_credentials_public from customer_as_target) else (select client_credentials_public from vendor_as_target) end)::text, 'config.wallet_config'::text, 'client_credentials_public'::text)
            from history.mobile_charge
        |]
        [__i|
            select
                mobile_charge.id::uuid
              , $CanBeValidated (
                  (case mobile_charge.payment_category
                      when 'StandAlone' then (
                          select client_credentials_public
                          from customer_as_target
                      )
                      else (
                          select client_credentials_public
                          from vendor_as_target
                      )
                  end)::text
                , 'config.wallet_config'::text
                , 'client_credentials_public'::text
            )
            from history.mobile_charge
        |]
#endif
    , mkCase
        "[61] CASE expression in a top-level SELECT target always wraps multi-line, even when its inline form would fit under the length threshold"
        4
        [__i|
            select
                pma.country::text
              , (case when row_details.row_method_invoked then 'Y' else 'N' end)::text
              , row_details.row_trans_id::uuid
            from app.row_details
            join config.payment_method_provider pma on pma.id = row_details.pma_id
            where row_details.id = $1::uuid
        |]
        [__i|
            select
                pma.country::text
              , (case
                    when row_details.row_method_invoked then 'Y'
                    else 'N'
                end)::text
              , row_details.row_trans_id::uuid
            from app.row_details
            join config.payment_method_provider pma
              on pma.id = row_details.pma_id
            where row_details.id = $1::uuid
        |]
    , mkCase
        "[62] bare CASE in SELECT target (no surrounding parens, optional trailing typecast on @end@) wraps with body indented relative to the @case@ keyword's column, not the outer block's nest level"
        4
        [__i|
            select
                id::text
              , currency::text
              , case transaction_type when 'CAPTURE' then 'Purchase' when 'REFUND' then 'Refund' end::text
              , amount::int
            from storefront_core.ledger
            where customer_id = $1::bigint
        |]
        [__i|
            select
                id::text
              , currency::text
              , case transaction_type
                    when 'CAPTURE' then 'Purchase'
                    when 'REFUND' then 'Refund'
                end::text
              , amount::int
            from storefront_core.ledger
            where customer_id = $1::bigint
        |]
    , mkCase
        "[63] single-target SELECT whose target is a top-level CASE forces block-list layout (`select` ends the line, `case` starts at item-content column) so @end@ lines up under @case@ — instead of @case@ landing mid-line after `select ` and the body / @end@ being pushed far right by 'align'"
        4
        [__i|
            select case when status = 'won' then fsm.notify_state_machine(shard := organization_id, machine := state_machine_id, event := 'dispute.won') when status = 'lost' then fsm.notify_state_machine(shard := organization_id, machine := state_machine_id, event := 'dispute.lost') end::text
            from classification.provider_dispute dispute
            where organization_id = $1::bigint
              and dispute.id = $2::uuid
        |]
        [__i|
            select
                case
                    when status = 'won' then fsm.notify_state_machine(
                          shard := organization_id
                        , machine := state_machine_id
                        , event := 'dispute.won'
                    )
                    when status = 'lost' then fsm.notify_state_machine(
                          shard := organization_id
                        , machine := state_machine_id
                        , event := 'dispute.lost'
                    )
                end::text
            from classification.provider_dispute dispute
            where organization_id = $1::bigint
              and dispute.id = $2::uuid
        |]
    , mkCase
        "[64] inline subquery `SELECT DISTINCT ON (...)` keeps the `ON (...)` clause — regression guard"
        4
        [__i|
            insert into reference.code_bins (start_num, end_num, num_length)
            select (row_data->>'start_num')::numeric, (row_data->>'end_num')::numeric, (row_data->>'num_length')::bigint
            from (select distinct on (row_data->>'start_num', row_data->>'end_num') row_data from jsonb_array_elements($1::jsonb) as row_data) s
            on conflict (start_num, end_num) do nothing
        |]
        [__i|
            insert into reference.code_bins
                (start_num, end_num, num_length)
            select
                (row_data->>'start_num')::numeric
              , (row_data->>'end_num')::numeric
              , (row_data->>'num_length')::bigint
            from (
                select distinct on (row_data ->> 'start_num', row_data ->> 'end_num') row_data
                from jsonb_array_elements($1::jsonb) as row_data
            ) s
            on conflict (start_num, end_num)
            do nothing
        |]
    ]


main :: IO ()
main = do
    let results = map runTest tests
        failures = filter (not . fst) results
    mapM_
        ( \(_, msg) -> do
            TIO.putStrLn "---"
            TIO.putStrLn msg
        )
        failures
    if null failures
        then do
            putStrLn $ "All " <> show (length tests) <> " tests passed."
        else do
            putStrLn $ "\n" <> show (length failures) <> " of " <> show (length tests) <> " tests FAILED."
            exitWith (ExitFailure 1)


runTest :: TestCase -> (Bool, Text)
runTest TestCase{name, qqCol, input, expected} =
    -- 'formatSql' now returns 'Left' on AST mismatch; treating that as a
    -- failure is the whole point of this test suite — if the formatter ever
    -- emits semantically-different SQL, the test reports it as such.
    let actual = either ("ERROR: " <>) id (formatSql qqCol (T.stripEnd input))
        stringEq = T.stripEnd actual == T.stripEnd expected
        -- Semantic guard on the test fixture itself: the hand-written
        -- `expected` must parse to the same AST as `input`. Catches a
        -- fixture-authoring mistake even before the formatter runs.
        astFixtureEq = case (parseAst input, parseAst expected) of
            (Just a, Just b) -> a == b
            _ -> True -- can't parse one side; defer to string check
            -- Semantic guard on the formatter output: parsing `actual` must
            -- yield the same AST as `input`. Catches a renderer that drops
            -- or alters a clause but still emits valid SQL (the
            -- `DISTINCT ON (...)` regression that motivated this guard).
        astFormatterEq = case (parseAst input, parseAst actual) of
            (Just a, Just b) -> a == b
            _ -> True
        pass = stringEq && astFixtureEq && astFormatterEq
        reason
            | not stringEq = "string mismatch"
            | not astFixtureEq = "AST mismatch: expected fixture is not semantically equivalent to input"
            | not astFormatterEq = "AST mismatch: formatter output is not semantically equivalent to input"
            | otherwise = ""
        msg =
            if pass
                then "PASS: " <> name
                else
                    "FAIL ("
                        <> reason
                        <> "): "
                        <> name
                        <> "\n=== INPUT ===\n"
                        <> showWhitespace input
                        <> "\n=== EXPECTED ===\n"
                        <> showWhitespace expected
                        <> "\n=== ACTUAL ===\n"
                        <> showWhitespace actual
     in (pass, msg)


-- | Show whitespace characters for debugging
showWhitespace :: Text -> Text
showWhitespace = T.pack . concatMap showC . T.unpack
  where
    showC '\n' = "\\n\n"
    showC c = [c]
