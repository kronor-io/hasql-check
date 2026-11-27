# Changelog

## 0.6.0.0

The quasiquote lexer is a hand-written one instead of an Alex lexer and a
Happy grammar, and reads Haskell as GHC does in places the old one did not,
each of which could hide a quote or make one up:

- `[e|`, `[d|`, `[t|` and `[p|` open Template Haskell brackets, whose
  contents are Haskell; they were read as quasiquotes ending at the first
  `|]`, even one inside a string.
- A pragma is read to its `#-}` with its string literals, so a `-}` or a
  quote in a pragma's string is text.
- Identifiers and operators may be Unicode: `⊕--` is an operator, not a
  comment; `é'` is an identifier, not the start of a character literal; and
  a quoter may be named `rø`, or with a combining mark.
- Columns count characters, and a tab to the next multiple of eight; they
  counted bytes.

`Hasql.Check.Lexer` now exports `tokenize`, which splits a source into code,
comments, pragmas, string and character literals and quasiquotes with their
positions, for tools that need more than the quotes. `AlexPosn`,
`QuasiQuote`, `parseQuasiQuotes` and `parseFile` are as they were; the
Alex and Happy internals (`runAlex`, `parse`, `scanner`, `lexFile`,
`Lexeme`) are gone, and so are the `alex`, `happy` and `array` dependencies.

## 0.5.2.0

A statement with `$1.$field` param fields could not be checked from a `.hie`
file, nor by the plugin: `hasql-check-hie` said its types were not known
where it is written. A field's type is only known once GHC has solved its
`HasField`, after it has already settled each slot's Postgres type as an
unsolved `PgColumnType a`; so GHC recorded `PgColumnType Text` rather than
`"text"`, which `hasql-check-hie` cannot reduce. A slot's Postgres type and
labels now come from `SlotType`, whose instances GHC cannot choose between
until the field's type is known, so they are recorded reduced.

`--inferred` took the number of parameters a statement with param fields is
sent to be its number of placeholders, so it reported each placeholder past
the first parameter's as a parameter the statement does not use. It now
counts the statement's parameters, as the quoter's pattern takes them.

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
