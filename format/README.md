# hasql-th-format

Formats SQL inside hasql-th quasiquotes (`[singletonStatement|...|]`,
`[maybeStatement|...|]`, `[resultlessStatement|...|]`, `[vectorStatement|...|]`,
`[rowsAffectedStatement|...|]`, etc.) using a `postgresql-syntax` AST and
`prettyprinter` Doc combinators.

Lives at `_dev/tools/hasql-th-format/`. Wired into pre-commit and the
`hooks-haskell` CI check so any unformatted quasiquote fails the build.

## Tools in this directory

| Executable        | Purpose                                                            |
|-------------------|--------------------------------------------------------------------|
| `hasql-th-format` | The formatter. Used by pre-commit + CI. Exits non-zero (with the offending file:line) if its own output is AST-divergent from the input. |
| `hasql-th-audit`  | Compares hasql-th quasiquotes across two file versions at the AST level. Catches a different class of regression — see [hasql-th-audit](#hasql-th-audit) for when each tool applies. |

## Round-trip AST safety

`formatSql` parses the input SQL, renders it, **re-parses the rendered
output, and only emits the formatted version if the re-parsed AST equals
the original AST**. A rendering bug that drops or alters a clause but
still produces valid SQL is treated as a **hard error**: the file is
left bytewise untouched, the CLI exits non-zero, and pre-commit / CI
fail with a message pointing at the offending QQ's line number. The
earlier silent fallback let exactly this kind of regression slip into
production via PR #5324 (`DistinctTargeting`); making it a hard error
means the next one is caught the moment it happens.

There's no false-positive risk — `postgresql-syntax` parses
semantically-equivalent text to identical ASTs (whitespace, keyword
case, `::` spacing all normalize), so an AST mismatch is always a real
renderer bug.

### What the error looks like

```
$ hasql-th-format src/Report/Ledger.hs
Error processing src/Report/Ledger.hs: line 164: AST mismatch: formatter rendered semantically-different SQL (renderer bug)
```

Two distinct error reasons:

- `AST mismatch: formatter rendered semantically-different SQL` — the
  renderer dropped or altered a clause. **Fix the renderer.** Don't
  silence the check.
- `AST mismatch: formatter output did not re-parse (<lex/parse error>)`
  — the renderer emitted malformed SQL. Should be unreachable, but
  surfaces the underlying parser error if it ever happens.

### Debugging the formatter

If you're actively iterating on the renderer and getting blocked by
this gate, `SKIP=hasql-th-format git commit ...` bypasses the
pre-commit hook for one commit. CI still runs the fresh build of the
formatter against the PR base, so the safety net is enforced before
merge — not silenced.

Concrete debugging loop: take the offending QQ, render via
`hasql-th-format --stdin`, inspect the result, compare AST. The
fixture test suite (`test/Test.hs`) is the right home for a minimised
reproducer once you've narrowed it down — every fixture's AST
equivalence is also asserted on every test run.

## hasql-th-audit

Companion executable for verifying that a refactor or reformat preserved
SQL semantics across the codebase. Takes two file paths and reports any
hasql-th quasiquote whose AST changed between the two versions.

The formatter's own AST safety net (see above) catches renderer bugs
*at format time, per QQ, against the input the formatter just received*.
`hasql-th-audit` complements it by working *cross-version, file-pair,
independent of the formatter*:

- The in-formatter check would let through a regression introduced by
  a hand-edit, a non-formatter mass refactor (rename, quasiquoter
  swap), or any change where `formatSql` isn't involved.
- The in-formatter check requires the formatter to be working at all
  — if its parser/renderer crashes or refuses to run, the safety net
  is bypassed. The auditor uses only `postgresql-syntax`, so it still
  works.
- The auditor answers historical questions ("did PR #5324 change any
  SQL anywhere?") that the in-formatter check can't, since that
  check is point-in-time only.

Two independent safety layers, different blast radii.

```
hasql-th-audit <old-file> <new-file>
```

Extracts every supported quasiquote from each file (using the same
`hasql-check` lexer the formatter uses), parses each SQL string with
`PostgresqlSyntax.parse` at `PreparableStmt`, and compares pairs by AST
equality. Exit codes:

- `0` — all QQs are semantically equivalent
- `1` — one or more QQs differ (or the QQ count itself differs)
- `2` — usage error or lexer failure

### When to run it

- After editing the formatter and re-running it across the codebase.
  CI does this automatically on any PR that touches
  `_dev/tools/hasql-th-format/**`; see `audit-hasql-th-format-reformat`
  in `.github/workflows/build-test.yml`.
- After any "mechanical" tree-wide refactor that's supposed to be
  semantics-preserving (renaming, quasiquoter swap, etc.).

### How to run it manually across a branch

```bash
BIN=$(cd _dev/tools/hasql-th-format && cabal list-bin hasql-th-audit)
base=$(git merge-base origin/master HEAD)

while read f; do
    [[ "$f" == _dev/tools/hasql-th-format/* ]] && continue
    old=$(mktemp); new=$(mktemp)
    git show "$base:$f" > "$old" 2>/dev/null || { rm "$old"; continue; }
    git show "HEAD:$f"  > "$new" 2>/dev/null || { rm "$old" "$new"; continue; }
    "$BIN" "$old" "$new" || echo "REGRESSION: $f"
    rm -f "$old" "$new"
done < <(git diff --name-only "$base" HEAD -- '*.hs')
```

## Workflow

```bash
# Build & test
cd _dev/tools/hasql-th-format
cabal build         # rebuild the executables (don't skip — see "Gotchas")
cabal test          # fixture tests

# Reformat the codebase
cd $(git rev-parse --show-toplevel)
HASQL_TH_FORMAT=$(cd _dev/tools/hasql-th-format && cabal list-bin hasql-th-format)
find . -name '*.hs' \
  -not -path '*/dist-*' \
  -not -path '*/third-party*' \
  -not -path '*/openapi3*' \
  -not -path '*/.stack-work/*' \
  | xargs "$HASQL_TH_FORMAT"

# Verify build + pre-commit
cabal build all                            # ~5–10 min
pre-commit run fourmolu hlint --all-files  # catches what hooks-haskell catches
```

If the reformat is bundled with formatter changes, also run
`hasql-th-audit` against `origin/master` to catch silent regressions
(CI does this automatically, but local pre-push is cheaper than a failed
CI cycle).

## Formatter design

### Wrap-aware variants

Most renderers come in three flavours:

- `renderFooInline` — single-line, no soft breaks.
- `renderFooWrapped` — multi-line block-list layout.
- `renderFooMaybeWrap` — measures the inline form and chooses based on a
  length threshold + structural eligibility predicate.

Length thresholds:

- `inlineLengthThreshold = 60` — function-arg / in-expression positions.
- `subqueryInlineThreshold = 100` — in-expression parenthesized SELECTs.
- `selectTargetInlineThreshold = 150` — top-level SELECT targets where
  the arg-count rule doesn't fire (single-arg outer call with deeply
  nested content).

### Pair grouping for key/value-shaped argument lists

`looksLikeKeyValuePairs` detects shape from the AST: ≥4 args, even count,
every even-indexed arg is a string literal. Catches all the JSON-builder
functions (`jsonb_build_object`, `json_build_object`, `jsonb_object`,
`json_object`) without carrying a function-name list.

When the outer call is wrapped, pair-shaped sub-calls **always** wrap
(via `hasPairShapedCall`) regardless of length — keeps visual
consistency so a short nested `jsonb_build_object` doesn't sit inline
next to wrapped siblings.

### `nest indentSize` vs `align`

- **`nest indentSize`** — body indent is `current_nest + indentSize`.
  Used everywhere except the outer-level hasql-th record. Matches what a
  hand-formatter would do: body indented past the outer context, not
  past wherever the keyword happened to land mid-line.
- **`align` then `indent indentSize`** — body indent is
  `keyword_column + indentSize`. Used only for the outermost hasql-th
  `$Record{...}` in `renderHsTargetEl`, because that's called mid-line
  after `renderBlockListWith`'s explicit `firstPad`. Without `align`,
  hardlines inside return to `current_nest` which is at the SELECT target
  indent, not the firstPad-shifted column.

### Things that didn't work / had to be undone

- **`renderBlockListWith` first-row using `indent` instead of explicit
  `firstPad`** — tried so the SELECT-target list's nest stack would
  propagate into the item's body. Reverted: it added 4 cols of indent to
  ALL multi-line items, pushing them deeper than expected. The fix is
  targeted (`renderHsTargetElWrappedInner` for inner records only), not
  global.
- **`align` for CASE wrap** — first implementation of
  `renderCaseExprWrapped` used `align` so the body indent was relative
  to the `case` keyword's column. Gave `WHEN`/`ELSE` at col 30+ for
  SET-RHS cases — far to the right, inconsistent with other wrapped
  constructs. Switched to `nest indentSize`.

## Hook + CI wiring

- `hookConfigs.hasql-th-format` in `flake.nix` mirrors `fourmolu` /
  `hlint`. Files pattern `\.hs$`, runs the formatter binary in
  pre-commit + the `hooks-haskell` CI check. An AST mismatch returns
  non-zero, so the hook (and CI) fails with the offending file:line.
- `audit-hasql-th-format-reformat` job in
  `.github/workflows/build-test.yml` runs `hasql-th-audit` across the
  changed `.hs` files on any PR that modifies
  `_dev/tools/hasql-th-format/**`. Defence in depth — independent
  machinery from `formatSql`, so a bug in the in-formatter check
  doesn't disable this one. Also catches a class of regression the
  in-formatter check can't see at all: changes the formatter never
  ran on (hand-edits to QQ contents, refactors using something other
  than `hasql-th-format`).

## Open issues / known limits

### Out-of-scope long lines

- **Haskell `[i|...|]` string interpolation literals** (e.g. JWT tokens
  in test fixtures, log messages). Out of scope — formatter only touches
  SQL inside hasql-th QQs.
- **hasql-th `$Record{...}` deeply-nested values** in a few test files
  with 4+ levels of nesting. The wrapped form kicks in, but on
  pathological nesting the result is still wide. Could lower the
  threshold for `renderHsTargetElMaybeWrap` if it becomes an issue.
- **Multi-row `VALUES (...), (...), ...` with very wide tuples**.
  Tuples wrap to bracket-list when > 3 cols, but values are emitted by
  `postgresql-syntax` inline and can be long. Would need a custom
  inline-vs-multiline gate per tuple.
- **SQL comments** (`-- ...`) preserved verbatim through
  `postgresql-syntax`. Out of scope.

### `(SELECT ...)` in JOIN double-parens

`renderTableRef`'s `Ast.SelectTableRef lp swp mAlias ->` case wraps the
already-parenthesized `swp` in another set of parens (`"(" <>
renderSelectWithParens swp <> ")"`). Looks like double parens to a
careful reader. `postgresql-syntax` may parse round-trip OK, but the
output isn't clean. Worth a follow-up fix.

### Inline-only AExpr/CExpr constructors

Several AExpr constructors aren't function-call-shaped and stay inline
even when long: `BetweenAExpr`, `InAExpr`, `DistinctFromAExpr`,
`OverlapsAExpr`, `ReversableOpAExpr`, `SymbolicBinOpAExpr`,
`VerbalExprBinOpAExpr`, `CollateAExpr`, `AtTimeZoneAExpr`,
`Prefix/SuffixQualOpAExpr`. None surface as long lines today, but if
they ever do, the `renderExprMaybeWrap` pattern applies.

## Gotchas

### `cabal test` doesn't rebuild the executable

`cabal test` rebuilds the test suite (linked against the in-tree library
modules) but **not** the executable at `cabal list-bin hasql-th-format`.
If you edit the formatter and immediately run

```bash
HASQL_TH_FORMAT=$(cabal list-bin hasql-th-format)
find ... | xargs "$HASQL_TH_FORMAT"
```

the binary will be the OLD object code. Always run `cabal build
hasql-th-format` (or `cabal build all`) between editing the formatter
and running it across the codebase.

### Pre-commit runs all hooks-haskell tools

`hooks-haskell` CI runs `pre-commit run --all-files`, so fourmolu /
hlint / hasql-th-format failures all go through the same gate. Before
pushing:

```bash
pre-commit run fourmolu hlint --all-files
```

## Quick map

| Concern | Symbol(s) | Approximate location |
|---|---|---|
| Round-trip safety (returns `Either Text Text`; `Left` = AST mismatch error) | `formatSql` | top of `SqlFormat.hs` |
| Per-QQ error tagging with line numbers | `makeReplacement` | `ThFormat.hs` |
| Per-file error collection + non-zero exit | `processBytes`, `processFile`, `Main` | `ThFormat.hs`, `Main.hs` |
| Wrap eligibility | `isWrapEligibleExpr`, `isWrapEligibleCExpr`, `isWrapEligibleArray`, `isWrapEligibleSubexpr`, `hasPairShapedCall` | ~365–425 |
| Length thresholds | `inlineLengthThreshold` (60), `subqueryInlineThreshold` (100), `selectTargetInlineThreshold` (150) | scattered |
| Wrap-aware arg/expr renderers | `renderFuncArgExprMaybeWrap`, `renderFuncArgExprWrapped`, `renderExprMaybeWrap`, `renderExprWrapped` | ~440–520 |
| Multi-line wrap implementations | `renderFuncApplicationWrapped`, `renderArrayExprWrapped`, `renderInParensWrapped`, `renderExplicitRowWrapped`, `renderImplicitRowWrapped`, `renderCaseExprWrapped`, `renderFuncExprCommonSubexprWrapped`, `renderSelectWithParensWrapped` | ~270–600 |
| Pair-grouped layout | `looksLikeKeyValuePairs`, `renderArgPairRows` | ~340–360 |
| FROM-list / JOIN function call wrap | `wrapEligibleFuncTable`, `renderTableRefDoc`, `renderFromClause` | ~830–1230 |
| `as alias (col, ...)` wrap | `renderFuncAliasClauseDoc` | ~860 |
| Nested hasql-th records | `renderHsTargetElWrappedInner`, `renderHsTargetElMaybeWrap`, `isHsTargetWrapEligible` | ~1240–1290 |
| Inline-path targeting (incl. `DISTINCT ON`) | `renderTargetingInline` | ~1110 |
| Multi-line targeting (incl. `DISTINCT ON`) | `renderTargeting` | ~1215 |
| Auditor (per-QQ AST diff) | `app/Audit.hs` | `hasql-th-audit` exe |
| Test suite | fixture tests with per-case AST-equivalence guard | `test/Test.hs` |


## The `kronor-syntax` flag

Off by default, the formatter reads and writes the SQL that upstream
`postgresql-syntax` accepts, plus hasql-th's `::type?` nullability markers.

Kronor's fork of hasql-th adds two more forms, and `+kronor-syntax` turns on
parsing *and* rendering for both:

- Haskell targets -- `$Just (name :: text)` and `$Row { a = x, b = y }` in the
  select list.
- Haskell param fields -- `$1.$accountId` on a placeholder.

The flag needs Kronor's fork of `postgresql-syntax`, which is where those
constructors live, so a build against Hackage must leave it off. It is the
same flag, meaning the same thing, as `hasql-check`'s.
