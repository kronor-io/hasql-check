# hasql-check-ghc

The GHC half of [hasql-check](https://hackage.haskell.org/package/hasql-check).

`Hasql.Check.Inferred.TH`'s quoters take no annotations: a statement's
parameter and result types come from the Haskell type it is used at. GHC only
knows those types once it has type-checked the module, which is after the
quoter ran, so this package gets them from GHC itself:

- **`Hasql.Check.Plugin`**, a GHC plugin that checks each statement against a
  database while its module compiles. Add it to the package that holds the
  statements:

  ```
  build-depends: hasql-check-ghc
  ghc-options:   -fplugin=Hasql.Check.Plugin
  ```

  and set `HASQL_CHECK_DATABASE_URL`. A statement that does not fit the schema
  is then a compile error at its quote. Without the variable nothing is
  checked; if the database cannot be reached the plugin says so once and the
  build carries on.

- **`hasql-check-hie`**, which reads the same types out of the `.hie` files GHC
  writes with `-fwrite-ide-info`, for `hasql-check --inferred`:

  ```
  $ cabal build --ghc-options='-fwrite-ide-info -hiedir=.hie'
  $ hasql-check-hie $(find .hie -name '*.hie') > statements.jsonl
  $ hasql-check --inferred statements.jsonl --nullability --types
  ```

  It needs no database, so it can run inside a sandboxed build, such as a Nix
  derivation, with only the checking step outside it.

Both use GHC's own library, whose internals change with each release, so this
package supports the series it has been built against — currently **GHC 9.12
and 9.14**. Build it with the GHC the checked project is built with.

The full documentation, including everything the checks report, is in the
[main README](https://github.com/kronor-io/hasql-check#readme).

## Development

`cabal test` compiles fixture modules with GHC itself, against a throwaway
PostgreSQL server, so `initdb`, `pg_ctl` and `psql` must be on `PATH`. The
compiler finds the plugin through the package environment cabal writes
(`write-ghc-environment-files: always`, in the project's `cabal.project`), so
run the tests from a checkout of the whole repository rather than from this
directory's sdist.
