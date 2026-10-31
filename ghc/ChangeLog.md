# Changelog

## 0.5.1.0

`hasql-check-hie` identified an inferred quote by its `Params` dictionary,
which the quoters that build their own encoder and decoder do not carry, so
it emitted nothing for them; the plugin matched only `statement` and
`resultless` and skipped them too. Both now read the Haskell types from
`HaskellParams` and `HaskellColumns`, and evaluate the `Append` that GHC
records unreduced.

## 0.5.0.0

Initial release, alongside `hasql-check` 0.5.0.0: `Hasql.Check.Plugin`, a GHC
plugin that checks the statements of `Hasql.Check.Inferred.TH` against a
database while their module compiles, and `hasql-check-hie`, which reads their
types from GHC's `.hie` files for `hasql-check --inferred`.
