# Add validators for attribute placement and name uniqueness

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`, and
`Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: COMPLETE

PLANS.md is not present in this repository.

## Purpose / big picture

After this change a user running `ddlint` on a DDlog program will see
span-accurate diagnostics for two categories of mistake that the parser
currently ignores:

1. Misplaced attributes: an attribute (`#[…]`) preceding an item other than
   `typedef`/`type`, `function`, or `relation` is rejected with a clear error
   message and the byte range of the offending attribute.
2. Duplicate definitions: a program containing two type definitions with the
   same name, two relations with the same name, two indexes with the same name,
   two transformers with the same name, two imports with the same path, or two
   functions with the same `(name, arity)` pair is rejected with a diagnostic
   pointing at the second definition.

These two validators close the final open item in Phase 2 of the project
roadmap (`docs/roadmap.md`, line 192). Once all quality gates pass, that
roadmap entry is marked done and Phase 2 is complete.

Observable success looks like this: running `make test` passes with new tests
exercising every acceptance case. Running
`ddlint::parse("#[cold]\nindex I on R(x)")` returns an error whose message
contains "attribute" and whose span covers `#[cold]`. Running
`ddlint::parse("typedef A = u32\ntypedef A = string")` returns an error whose
message contains "duplicate" and "A".

## Constraints

- Follow `docs/differential-datalog-parser-syntax-spec-updated.md` sections
  5.1, 8, 9, and 12 as the normative specification.
- Preserve existing parser architecture: tokenizer -> span scanners ->
  `ParsedSpans` builder -> CST green tree -> AST wrappers -> `Parsed`.
- Do not add new crate dependencies.
- Keep every Rust source file under 400 lines.
- Every new Rust module must start with a `//!` module comment.
- All public items must have `///` Rustdoc comments.
- Comments and documentation must use en-GB-oxendict spelling.
- Errors are `chumsky::error::Simple<SyntaxKind>` with byte-offset spans,
  consistent with all existing parser diagnostics.
- Use `rstest` for parameterised tests and shared fixtures.
- Use Make targets; run required gates with `set -o pipefail` and `tee`.
- Record design decisions in this ExecPlan's `Decision Log` section.

## Tolerances (exception triggers)

- Scope: if implementation requires changes to more than 15 files or 600 net
  new lines, stop and escalate.
- Interface: if any existing public API signature (on `Parsed`, `Root`, or any
  AST wrapper) must change, stop and escalate.
- Dependencies: if a new external crate dependency is required, stop and
  escalate.
- Iterations: if `make test` still fails after three focused fix iterations,
  stop and escalate with failing test names.
- Grammar ambiguity: if the spec is unclear on whether a particular item kind
  is a permitted attribute target, document the ambiguity in `Decision Log` and
  escalate.
- AST blast radius: if name extraction requires modifying existing AST wrapper
  implementations (`.name()`, `.path()`, `.parameters()`), stop and escalate.

## Risks

- Risk: existing span scanners rely on `T_HASH` tokens "falling through" as
  unrecognised tokens. Introducing an attribute scanner that consumes `T_HASH`
  tokens could cause the rule scanner to see fewer tokens, changing rule span
  boundaries. Severity: medium Likelihood: medium Mitigation: attribute spans
  are added to `non_rule_spans` so the rule scanner skips over them, exactly as
  it skips import/typedef/relation spans today. Add a regression test with
  attributes interleaved with rules.

- Risk: `T_HASH` not followed by `T_LBRACKET` (for example `#` as a comment
  character in data) should not be treated as an attribute. Severity: low
  Likelihood: low Mitigation: the scanner only recognises `T_HASH` immediately
  followed by `T_LBRACKET` (after optional inline whitespace) as an attribute
  start. A lone `T_HASH` is skipped.

- Risk: the name uniqueness validator depends on AST wrapper `.name()` methods
  returning `Some(…)` for well-formed items. Malformed items (parse errors)
  return `None` and must not cause false-positive duplicate errors. Severity:
  medium Likelihood: medium Mitigation: the validator skips items whose
  `.name()` returns `None`.

- Risk: function overloading by arity means `(name, arity)` is the uniqueness
  key, not just name. If `Function::parameters()` returns an incorrect count
  for malformed functions the validator could produce false positives.
  Severity: low Likelihood: low Mitigation: skip functions whose `.name()`
  returns `None`. Document the arity-extraction approach in the `Decision Log`.

## Progress

- [x] (2026-02-16) Draft ExecPlan.
- [x] (2026-02-16) Create attribute span scanner
  (`src/parser/span_scanners/attributes.rs`).
- [x] (2026-02-16) Integrate attribute spans into `ParsedSpans`, CST
  builder, and `parse_tokens()`.
- [x] (2026-02-16) Create name uniqueness validator
  (`src/parser/validators/name_uniqueness.rs`).
- [x] (2026-02-16) Wire name uniqueness validator into `parse()`.
- [x] (2026-02-16) Add unit tests for attribute scanner.
- [x] (2026-02-16) Add parser-level tests for attribute placement.
- [x] (2026-02-16) Add integration/behavioural tests
  (`tests/attribute_placement.rs`, `tests/name_uniqueness.rs`).
- [x] (2026-02-16) Run quality gates and mark roadmap entry done.

## Surprises & discoveries

- Observation: `chumsky::error::Simple<SyntaxKind>` does not implement
  `Display`, only `Debug`. All test error inspection must use
  `format!("{e:?}")` rather than `.to_string()`. Evidence: compilation errors
  when using `.to_string()` on `Simple`. Impact: test assertion patterns must
  use Debug formatting.

- Observation: consecutive relation declarations (e.g.
  `input relation R(…)\noutput relation R(…)`) get merged into a single
  `N_RELATION_DECL` span by the existing relation span scanner. This is because
  `relation_columns()` uses `inline_ws()` which consumes `\n` tokens, causing
  the cursor to overshoot into the next line. Evidence: CST dump showing a
  single `N_RELATION_DECL@0..54` node. Impact: duplicate-relation tests must
  separate declarations with another item type (e.g. a typedef) to get two
  distinct CST nodes.

## Decision log

- Decision: implement attribute recognition as a new span scanner rather than
  a post-parse CST walk. Rationale: every other item type (import, typedef,
  relation, index, function, transformer, apply, rule) has a dedicated span
  scanner that feeds into `ParsedSpans`. Attributes follow the same pattern,
  which keeps the architecture uniform and allows the CST builder to wrap
  attribute tokens in `N_ATTRIBUTE` nodes. Date/Author: 2026-02-16 (assistant)

- Decision: attribute placement validation happens inside the attribute span
  scanner, not as a separate pass. Rationale: the scanner already peeks at the
  following token to determine when the attribute block ends; validating
  placement at that point is zero additional cost and avoids a second scan.
  Date/Author: 2026-02-16 (assistant)

- Decision: name uniqueness is a post-parse pass over the AST, not a
  scan-time check. Rationale: uniqueness requires whole-program visibility
  across all items of each category. The AST wrappers already expose `.name()`
  and `.parameters()` accessors, making extraction trivial. Scan-time checks
  would require cross-scanner state sharing. Date/Author: 2026-02-16 (assistant)

- Decision: each `#[…]` attribute gets its own `N_ATTRIBUTE` CST node; item
  spans are not extended to include preceding attributes. Rationale: the spec
  defines `Attributed(X) ::= Attribute+ X` as separate production — attributes
  and items are siblings, not a single compound node. This also means stacked
  attributes (`#[a] #[b] typedef …`) produce two `N_ATTRIBUTE` nodes followed
  by one `N_TYPE_DEF` node. Date/Author: 2026-02-16 (assistant)

- Decision: function uniqueness key is `(name, parameter_count)` — i.e.,
  arity-based overloading is permitted per spec section 8. Date/Author:
  2026-02-16 (assistant)

## Outcomes & retrospective

Implementation complete. All quality gates pass (`make check-fmt`, `make lint`,
`make test`). The roadmap entry is marked done.

Summary of delivered artefacts:

- Attribute span scanner with placement validation
  (`src/parser/span_scanners/attributes.rs`).
- Name uniqueness validator
  (`src/parser/validators/name_uniqueness.rs`).
- Unit tests for both features.
- Parser-level tests (`src/parser/tests/attributes.rs`).
- Integration tests (`tests/attribute_placement.rs`,
  `tests/name_uniqueness.rs`).

Lessons learned: the relation span scanner has a pre-existing issue where
consecutive relation declarations on adjacent lines get merged into a single
CST span. Tests for duplicate relation names must work around this by
interleaving a different item type. This issue is worth tracking for a future
fix.

## Context and orientation

The DDlog parser lives under `src/parser/`. Parsing proceeds through a pipeline:

1. Tokenizer (`src/tokenizer.rs`): produces `Vec<(SyntaxKind, Span)>` where
   `Span = Range<usize>`. Token kinds include `T_HASH` (the `#` character) and
   every bracket, keyword, and operator. The `N_ATTRIBUTE` node kind exists in
   `SyntaxKind` (`src/language.rs`, line 144) but is not yet used anywhere.

2. Span scanners (`src/parser/span_scanners/*.rs`): each module scans the
   token stream for a specific item type and returns
   `(Vec<Span>, Vec<Simple<SyntaxKind>>)`. They use `SpanCollector` (defined in
   `src/parser/span_collector.rs`) for state and the `token_dispatch!` macro
   (defined in `src/parser/lexer_helpers.rs`, line 72) for iteration.

3. Orchestrator (`src/parser/span_scanner.rs`): `parse_tokens()` calls every
   span scanner, merges non-rule spans, feeds them to rule scanning, and builds
   `ParsedSpans` via the builder.

4. CST builder (`src/parser/cst_builder/tree.rs`): `build_green_tree()` walks
   the token stream and uses `SpanCursors` (one per `ParsedSpans` category) to
   start/finish `rowan` nodes at span boundaries. The dispatch table is the
   `SPAN_CURSOR_KINDS` constant (line 13).

5. AST wrappers (`src/parser/ast/*.rs`): thin wrappers around
   `rowan::SyntaxNode<DdlogLanguage>` providing `.name()`, `.path()`,
   `.parameters()`, and similar accessors.

6. Entry point (`src/parser/mod.rs`): `pub fn parse(src) -> Parsed` runs the
   full pipeline and returns the green tree, the `Root` wrapper, and collected
   errors.

Tests live in two places: `src/parser/tests/*.rs` for parser-level tests (using
`crate::parse()`), and `tests/*.rs` for integration/behavioural tests (using
`ddlint::parse()`). Both use `rstest` for parameterisation and helper functions
from `src/test_util/`.

The `text_range_to_span()` utility in `src/parser/ast/rule.rs` (line 642)
converts `rowan::TextRange` to `Span`; the name uniqueness validator will use
it to produce error spans from AST nodes.

## Plan of work

Stage A: scaffold the attribute span scanner, integrate it into the pipeline,
and add the `attributes` field to `ParsedSpans`. This makes `#[…]` tokens
visible to the CST builder as `N_ATTRIBUTE` nodes and validates placement.

Stage B: scaffold the name uniqueness validator and wire it into `parse()`.
This emits duplicate-definition diagnostics for all item categories.

Stage C: add all unit tests, parser-level tests, and integration tests for both
features. Verify that every acceptance case from the spec is covered.

Stage D: run quality gates, update the roadmap, and write any final doc updates.

Each stage ends with `make check-fmt && make lint && make test` passing.

## Concrete steps

All commands run from the repository root (`/home/user/project`).

### Step 1 — Add `attributes` to `ParsedSpans`

In `src/parser/cst_builder/spans.rs`:

- Add `attributes: Vec<Span>` field to `ParsedSpans` (after `expressions`).
- Add `attributes: Vec<Span>` field to `ParsedSpansBuilder`.
- Add a builder method `pub fn attributes(mut self, spans: Vec<Span>) -> Self`.
- Add an accessor `pub fn attributes(&self) -> &[Span]`.
- Update `span_lists()` to include `("attributes", &self.attributes)` and
  change the array size from 9 to 10.
- Update `into_parsed_spans()` to destructure and forward `attributes`.

### Step 2 — Register `N_ATTRIBUTE` in the CST builder

In `src/parser/cst_builder/tree.rs`, line 13: add
`(ParsedSpans::attributes, SyntaxKind::N_ATTRIBUTE)` to `SPAN_CURSOR_KINDS`.
The `SPAN_CURSOR_INLINE` constant on line 24 is derived from
`SPAN_CURSOR_KINDS.len()` and updates automatically.

### Step 3 — Create the attribute span scanner

Create `src/parser/span_scanners/attributes.rs`. The public function:

    pub(crate) fn collect_attribute_spans(
        tokens: &[(SyntaxKind, Span)],
        src: &str,
    ) -> (Vec<Span>, Vec<Simple<SyntaxKind>>)

The implementation:

- Uses `SpanCollector<Vec<Simple<SyntaxKind>>>` as state.
- Dispatches on `T_HASH` via `token_dispatch!`.
- When `T_HASH` is encountered: peek ahead (skipping inline whitespace). If
  the next token is `T_LBRACKET`, consume the attribute: record
  `start = hash_span.start`, advance past `T_HASH`, advance past `T_LBRACKET`,
  then advance until a matching `T_RBRACKET` is found (tracking bracket depth
  for nested brackets). Record `start..rbracket_span.end` as an attribute span.
  If no `T_RBRACKET` is found before end-of-line or end-of-input, emit an
  unclosed-bracket error and skip.
- If `T_HASH` is NOT followed by `T_LBRACKET`, just advance (not an
  attribute).
- After recording consecutive attribute spans, peek at the next
  non-whitespace, non-comment token. This is the "target" keyword.
- Permitted targets: `K_TYPEDEF`, `K_TYPE`, `K_FUNCTION`, `K_EXTERN` (only
  when the next keyword after `extern` is `K_TYPE` or `K_FUNCTION`), `K_INPUT`,
  `K_OUTPUT`, `K_RELATION`, `K_STREAM`, `K_MULTISET`.
- If the target is not permitted (or is absent/EOF), emit a
  `Simple::custom` error with the message
  `"attribute not permitted on this item"`.

### Step 4 — Register the attribute scanner in the module index

In `src/parser/span_scanners/mod.rs`:

- Add `pub(super) mod attributes;` and
  `pub(super) use attributes::collect_attribute_spans;`.

### Step 5 — Wire the attribute scanner into `parse_tokens()`

In `src/parser/span_scanner.rs`:

- Import `collect_attribute_spans` alongside the other scanners.
- Call `let (attribute_spans, attribute_errors) =
  collect_attribute_spans(tokens, src);` at the start of `parse_tokens()`.
- Add `attribute_spans` to `non_rule_span_capacity` and `non_rule_spans`.
- Add `.attributes(attribute_spans)` to the builder chain.
- Extend `all_errors` with `attribute_errors`.

### Step 6 — Create the name uniqueness validator

Create `src/parser/validators/mod.rs`:

    //! Post-parse validation passes.
    //!
    //! Validators run after CST construction and emit diagnostics for
    //! semantic constraints that require whole-program visibility.

    pub(crate) mod name_uniqueness;
    pub(crate) use name_uniqueness::validate_name_uniqueness;

Create `src/parser/validators/name_uniqueness.rs`. The public function:

    pub(crate) fn validate_name_uniqueness(
        root: &Root,
    ) -> Vec<Simple<SyntaxKind>>

The implementation:

- For each category (types, relations, indexes, transformers, imports), iterate
  the corresponding `root.*()` accessor. For each item, call `.name()` (or
  `.path()` for imports). If `None`, skip. Otherwise check a
  `HashMap<String, Span>`: if the name is already present, emit
  `Simple::custom(duplicate_span, "duplicate {category} name '{name}'")`;
  otherwise insert the name with its span.
- For functions: key on `(name, arity)` where arity =
  `func.parameters().len()`. Use `HashMap<(String, usize), Span>`. Error
  message: `"duplicate function '{name}' with arity {arity}"`.
- To obtain spans, use `item.syntax().text_range()` converted via
  `text_range_to_span()` from `src/parser/ast/rule.rs`.
- Extract the duplicate-checking loop into a helper function to avoid
  repetition across categories.

### Step 7 — Wire name uniqueness into `parse()`

In `src/parser/mod.rs`:

- Add `mod validators;` after `pub mod ast;`.
- In `parse()`, change `errors` to `mut errors` and append:

      errors.extend(validators::validate_name_uniqueness(&root));

### Step 8 — Add unit tests for the attribute scanner

In `src/parser/span_scanners/tests.rs`, add test functions:

- `collect_attribute_spans_valid_on_typedef`: source `"#[cold]\ntypedef T =
  u32\n"`, expect one attribute span covering `#[cold]`, no errors.
- `collect_attribute_spans_valid_on_function`: source
  `"#[inline]\nfunction f() {}\n"`, expect one attribute span, no errors.
- `collect_attribute_spans_valid_on_relation`: source
  `"#[hot]\ninput relation R(x: u32)\n"`, expect one attribute span, no errors.
- `collect_attribute_spans_rejected_on_index`: source
  `"#[cold]\nindex I on R(x)\n"`, expect one attribute span, one error
  containing "attribute".
- `collect_attribute_spans_rejected_on_apply`: source
  `"#[cold]\napply T(R) -> (S)\n"`, expect one attribute span, one error.
- `collect_attribute_spans_rejected_on_import`: source
  `"#[cold]\nimport foo\n"`, expect one attribute span, one error.
- `collect_attribute_spans_stacked`: source `"#[a]\n#[b]\ntypedef T = u32\n"`,
  expect two attribute spans, no errors.
- `collect_attribute_spans_hash_without_bracket`: source
  `"# typedef T = u32\n"`, expect no attribute spans, no errors.

### Step 9 — Add parser-level attribute tests

Create `src/parser/tests/attributes.rs` with `//!` module comment. Register it
in `src/parser/tests/mod.rs` as `mod attributes;`.

Tests using `crate::parse()`:

- `attribute_on_typedef_no_error`: parse `"#[cold]\ntypedef T = u32"`, assert
  `parsed.errors().is_empty()`.
- `attribute_on_function_no_error`: parse `"#[inline]\nfunction f() {}"`.
- `attribute_on_relation_no_error`: parse `"#[hot]\ninput relation R(x:
  u32)"`.
- `attribute_on_extern_function_no_error`: parse `"#[cold]\nextern function
  f()"`.
- `attribute_on_index_emits_error`: parse `"#[cold]\nindex I on R(x)"`, assert
  exactly one error whose display contains "attribute".
- `attribute_on_rule_emits_error`: parse `"#[cold]\nR(x) :- S(x)."`, assert
  one error.
- `stacked_attributes_on_typedef`: parse `"#[a]\n#[b]\ntypedef T = u32"`, no
  errors, two `N_ATTRIBUTE` CST nodes.
- `attribute_on_apply_emits_error`: parse `"#[cold]\napply T(R) -> (S)"`,
  assert error.

### Step 10 — Add name uniqueness unit tests

In `src/parser/validators/name_uniqueness.rs`, add a `#[cfg(test)] mod tests`
block:

- `no_duplicates_no_errors`: parse a program with distinct names across all
  categories, assert no errors from the validator.
- `duplicate_typedef_names`: parse `"typedef A = u32\ntypedef A = string"`,
  assert one error containing "duplicate" and "A".
- `duplicate_relation_names`: parse two relations with the same name, assert
  error.
- `duplicate_index_names`: parse two indexes with the same name, assert error.
- `duplicate_transformer_names`: parse two transformers with the same name,
  assert error.
- `duplicate_import_paths`: parse `"import foo\nimport foo"`, assert error.
- `duplicate_function_name_and_arity`: parse two functions with the same name
  and parameter count, assert error.
- `function_overload_different_arity_no_error`: parse two functions with the
  same name but different parameter counts, assert no error.
- `malformed_item_skipped`: parse a program with a name-less malformed typedef
  followed by a valid one, assert no duplicate error.

### Step 11 — Add integration/behavioural tests

Create `tests/attribute_placement.rs`:

- Uses `ddlint::parse` and `ddlint::test_util::assert_no_parse_errors`.
- `rstest`-parameterised happy paths (typedef, function, relation, extern
  variants).
- `rstest`-parameterised error paths (index, apply, import, rule).
- Spec section 12 example: `"#[cold]\nindex Ix(a: T) on A[a]."` produces an
  error.

Create `tests/name_uniqueness.rs`:

- Uses `ddlint::parse`.
- `rstest`-parameterised cases for each category.
- Mixed program with no duplicates produces no errors.
- Function arity overloading is permitted.

### Step 12 — Run quality gates and mark roadmap done

Run from repository root:

    set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
    set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
    set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log

If documentation files were modified:

    set -o pipefail && make fmt 2>&1 | tee /tmp/ddlint-fmt.log
    set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log

Expected output: all three Rust gates report zero failures, zero warnings.

Then in `docs/roadmap.md`, line 192, change `- [ ]` to `- [x]` for the
attribute validators item.

## Validation and acceptance

Acceptance is defined as all of the following holding simultaneously:

- `make check-fmt` exits 0 (formatting clean).
- `make lint` exits 0 (no Clippy warnings).
- `make test` exits 0 with all new tests passing.
- Parsing `"#[cold]\ntypedef T = u32"` produces no errors.
- Parsing `"#[cold]\nindex I on R(x)"` produces exactly one error whose
  display contains "attribute".
- Parsing `"typedef A = u32\ntypedef A = string"` produces exactly one error
  whose display contains "duplicate".
- Parsing `"function f(x: u32) {}\nfunction f(x: u32) {}"` produces exactly
  one error whose display contains "duplicate" and "f".
- Parsing `"function f(x: u32) {}\nfunction f(x: u32, y: u32) {}"` produces
  no duplicate errors (arity-based overloading is permitted).
- `docs/roadmap.md` line 192 is marked `[x]`.

## Idempotence and recovery

All steps are re-runnable. No step is destructive; each creates or modifies
files additively. If a quality gate fails, fix the specific failure, re-run the
failed target, then re-run all gates. The attribute scanner and name uniqueness
validator are independent of each other, so either can be reverted without
affecting the other.

## Artifacts and notes

Keep command logs under `/tmp/ddlint-*.log` and summarise relevant pass/fail
lines in the `Progress` section.

## Interfaces and dependencies

No new external dependencies.

New internal interfaces:

In `src/parser/span_scanners/attributes.rs`:

    pub(crate) fn collect_attribute_spans(
        tokens: &[(SyntaxKind, Span)],
        src: &str,
    ) -> (Vec<Span>, Vec<Simple<SyntaxKind>>)

In `src/parser/validators/name_uniqueness.rs`:

    pub(crate) fn validate_name_uniqueness(
        root: &Root,
    ) -> Vec<Simple<SyntaxKind>>

In `src/parser/cst_builder/spans.rs` (additions to existing types):

    // On ParsedSpansBuilder:
    pub fn attributes(mut self, spans: Vec<Span>) -> Self

    // On ParsedSpans:
    pub fn attributes(&self) -> &[Span]
