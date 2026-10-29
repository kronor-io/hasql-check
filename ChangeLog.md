# Changelog

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
