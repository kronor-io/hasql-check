## hasql-check

`hasql-check` finds the [hasql-th](https://hackage.haskell.org/package/hasql-th)
quasiquotes in Haskell source files and `PREPARE`s each statement against a
live PostgreSQL database. A statement that names a missing table, column or
function, or that no longer type-checks against the schema, fails here rather
than at runtime.

The SQL is parsed and rendered with `postgresql-syntax`, the library hasql-th
itself uses, so what is prepared is what the splice would send.

```
hasql-check [--database CONNINFO] [--quoter NAME]... [--list] FILES...
```

- `--database`: a libpq connection string or URI. Without it the database is
  taken from `DATABASE_URL`, then `psql_conn_string`, then libpq's own
  environment variables and defaults (`PGHOST`, `PGPORT`, `PGDATABASE`, ...).
- `--quoter`: also check quasiquotes with this quoter name, for your own
  quoters built on hasql-th. Repeatable.
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
