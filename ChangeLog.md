# Changelog

## 0.5.1.0

The quoters of `Hasql.Check.Inferred.TH` now take the Haskell targets and
`$1.$field` param fields of the `kronor-syntax` flag: the quoter builds the
row decoder and the parameter encoder from them, and a record's columns are
read at the type of the field each one names, so a statement may write the
fields in any order. Add `rowsAffectedStatement`, and widen the tuple
instances from eight components to twenty-four.

`Target` and `Slots` carry the Postgres and the Haskell types at the type
level, as `PostgresParams`, `PostgresColumns`, `HaskellParams` and
`HaskellColumns`, so `hasql-check-hie` finds them in the `.hie` files.

`--inferred` annotated the statement as written rather than as the quoter
sends it, so a statement that writes its placeholders out of order, such as
`set name = $2 where id = $1`, had its parameters' types paired with the
wrong ones and was rejected. It now lowers the statement first, and reports
an unused parameter by the number the statement was written with.

Known gap: a param field reaches its type through `HasField`, whose
`PgColumnType` stays stuck in the recorded type, so such a statement is
reported as having no concrete type rather than being checked.

## 0.5.0.0

Add `Hasql.Check.Inferred.TH`: quoters for statements without annotations,
whose parameter and result types come from the Haskell types they are used
at. Each type names its Postgres type at the type level, so a `newtype`
deriving `Value`, or deriving it via `Domain`, is checked as such, and
`Vector`s and lists are arrays, and an enumeration deriving it via `PgEnum`
is an enum, whose labels the checker compares with the database's. Add
`--inferred`, which checks them, and the `hasql-check-ghc` package, which
gets their types from GHC: `Hasql.Check.Plugin`, a GHC plugin that checks
them while they compile, and `hasql-check-hie`, which reads them from GHC's
`.hie` files.

## 0.4.0.0

Add `--types`, and `HASQL_CHECK_TYPES` for compile-time checks: warn about
annotations that convert a table column, or a parameter assigned to one, to a
narrower type.

Suppress warnings with `hasql-check: allow` directives above a quote, or with
a baseline file (`--baseline`, `--write-baseline`, `HASQL_CHECK_BASELINE`).
Add `--fail-on-warnings`, and `--files0-from` for checking every file in one
run.

## 0.3.0.0

Add `Hasql.Check.TH`: hasql-th's quasiquoters, checking their statements
against a database while they compile.

## 0.2.0.0

Add `--nullability`: warn about result columns annotated non-null that can be
null.

## 0.1.0.0

Initial release.
