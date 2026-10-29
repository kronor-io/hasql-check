## hasql-check

`hasql-check` finds the [hasql-th](https://hackage.haskell.org/package/hasql-th)
quasiquotes in Haskell source files and `PREPARE`s each statement against a
live PostgreSQL database. A statement that names a missing table, column or
function, or that no longer type-checks against the schema, fails here rather
than at runtime.

The SQL is parsed and rendered with `postgresql-syntax`, the library hasql-th
itself uses, so what is prepared is what the splice would send.

```
hasql-check [--database CONNINFO] [--quoter NAME]... [--nullability] [--types]
            [--baseline FILE] [--write-baseline FILE] [--fail-on-warnings]
            [--files0-from FILE] [--list] FILES...
```

- `--database`: a libpq connection string or URI. Without it the database is
  taken from `DATABASE_URL`, then `psql_conn_string`, then libpq's own
  environment variables and defaults (`PGHOST`, `PGPORT`, `PGDATABASE`, ...).
- `--quoter`: also check quasiquotes with this quoter name, for your own
  quoters built on hasql-th. Repeatable.
- `--nullability`: also warn about result columns annotated non-null that can
  be null. See below.
- `--types`: also warn about annotations that convert a value to a narrower
  type. See below.
- `--baseline`, `--write-baseline`, `--fail-on-warnings`: see "Suppressing
  warnings" below.
- `--files0-from`: also check the NUL-separated file names in a file, or on
  stdin with `-`, as `find -print0` writes them. Prefer it to `xargs`, which
  may split the files between several runs.
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

### Narrowing annotations

An annotation is a cast, and a cast between incompatible types already fails
to prepare. A narrowing one does not: `bigint_column :: int4` prepares, and
fails or loses data at runtime only for the values that do not fit. With
`--types`, `hasql-check` warns about:

- a result column that reads a table column of a wider type:
  `amount :: int4` from a `bigint` column, `rate :: float4` from a
  `double precision` one, `at :: date` from a `timestamptz` one;
- a parameter assigned to a column of a narrower type, in `INSERT ... VALUES`
  or `UPDATE ... SET`: `$1 :: int8` into an `integer` column.

Expressions are not checked, since converting one is almost always the point
(`count(*) :: int4`), nor are parameters that are only compared, since a
comparison widens the column rather than narrowing the parameter. Domains are
looked through to their base types. The statement is prepared again without
those annotations, so that Postgres reports the types underneath them.

### Suppressing warnings

Most warnings are best fixed: a `?` on the annotation, an `IS NOT NULL` in
the statement, a `NOT NULL` constraint on the column, a wider annotation. For
a deliberate case, a directive in the comment lines directly above the line
the quote starts on suppresses findings by rule and by column name, or `$n`
for a parameter:

```haskell
-- hasql-check: allow nullable(street, city), narrowing($4)
[singletonStatement| ... |]
```

A directive that suppresses nothing is itself warned about, so stale ones do
not linger. (SQL comments cannot be used: hasql-th does not accept them in a
quote.)

To start failing CI on new warnings without first fixing the existing ones,
write them to a baseline and check against it:

```
hasql-check --nullability --types --write-baseline .hasql-check-baseline ...
hasql-check --nullability --types --baseline .hasql-check-baseline --fail-on-warnings ...
```

A baseline entry is a hash of the statement as Postgres receives it, the rule
and the column, so moving a quote or changing its layout keeps it, and
changing the statement drops it. `--fail-on-warnings` exits 1 if anything is
left to warn about.

### Checking while compiling

`Hasql.Check.TH` exports hasql-th's quasiquoters, unchanged except that they
check their statements against a database as they compile. Import it instead
of `Hasql.TH`:

```haskell
import Hasql.Check.TH (maybeStatement)
```

Nothing is checked unless `HASQL_CHECK_DATABASE_URL` is set, so builds without
a database behave exactly as with `Hasql.TH`. When it is set, a statement that
does not prepare is a compile error at its quote. If the database cannot be
reached, the build warns once and goes on.

- `HASQL_CHECK_DATABASE_URL`: a libpq connection string or URI.
- `HASQL_CHECK_SCHEMA_FILE`: a file that changes whenever the schema does,
  such as a migration plan. Modules with checked quotes depend on it, so a
  schema change rechecks them. Cabal only notices it if it is also in the
  package's `extra-source-files`; GHCi notices it regardless.
- `HASQL_CHECK_BASELINE`: a baseline file, as above.
- `HASQL_CHECK_NULLABILITY`, `HASQL_CHECK_TYPES`: when set, the
  `--nullability` and `--types` warnings are reported too, in GHC's format on
  stderr, so that `-Werror` does not fail the build on them.

GHC runs a quote only when it compiles the module, so a module compiled
before the database was set, or before the schema changed, is not checked
until it recompiles. Keep running the `hasql-check` executable in CI.

One connection serves every quote the compiler runs; a dropped connection is
replaced.

