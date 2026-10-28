## hasql-check

`hasql-check` finds the [hasql-th](https://hackage.haskell.org/package/hasql-th)
quasiquotes in Haskell source files and `PREPARE`s each statement against a
live PostgreSQL database. A statement that names a missing table, column or
function, or that no longer type-checks against the schema, fails here rather
than at runtime.

The SQL is parsed and rendered with `postgresql-syntax`, the library hasql-th
itself uses, so what is prepared is what the splice would send.

```
hasql-check [--database CONNINFO] [--quoter NAME]... [--nullability] [--list] FILES...
```

- `--database`: a libpq connection string or URI. Without it the database is
  taken from `DATABASE_URL`, then `psql_conn_string`, then libpq's own
  environment variables and defaults (`PGHOST`, `PGPORT`, `PGDATABASE`, ...).
- `--quoter`: also check quasiquotes with this quoter name, for your own
  quoters built on hasql-th. Repeatable.
- `--nullability`: also warn about result columns annotated non-null that can
  be null. See below.
- `--list`: print the quasiquotes that would be checked, without connecting.

The quoters checked by default are `singletonStatement`, `maybeStatement`,
`vectorStatement`, `foldStatement`, `resultlessStatement` and
`rowsAffectedStatement`, qualified or not. `uncheckedSql` is not checked: it is
meant for scripts that `PREPARE` rejects.

Statements are parsed the way upstream hasql-th parses them, nullability
markers (`text?`) included. Building with the `kronor-syntax` flag also accepts
the Haskell targets (`$f (a :: int8)`) and parameter fields (`$1.$field`) of
Kronor's hasql-th fork; that needs Kronor's fork of `postgresql-syntax`.

Quotes inside comments and string literals are ignored. Haskell syntax is not
otherwise checked.

For each failing statement the file, line and column of the quote are printed
with PostgreSQL's error; the exit code is 1 if any statement failed.

### Tests

`cabal test` starts a throwaway PostgreSQL server, so `initdb`, `pg_ctl` and
`psql` must be on `PATH`.

### Nullability

With `--nullability`, a result column annotated non-null (`name :: text`, no
`?`) gets a warning when it can be null:

- it reads a table column that is not `NOT NULL`, or
- it comes from the nullable side of an outer join,

unless the statement filters it with `IS NOT NULL`. Warnings do not change the
exit code.

Postgres does not say whether an expression or a view's column can be null, so
those are not checked; neither are statements `hasql-check` cannot take apart,
such as `UNION`s and `SELECT *`. The approach is the one sqlx takes: the
statement is prepared again without the annotations, so that Postgres reports
the table column each result column reads, and its plan, from
`EXPLAIN (VERBOSE)` with a generic plan, shows the outer joins and the
`IS NOT NULL` filters.

Planning evaluates `current_setting(...)` calls, so a statement that reads a
setting the session does not have cannot be planned, and only its table
columns are checked. Give such settings placeholder values in the connection,
for example `PGOPTIONS="-c app.tenant_id=0"`.

