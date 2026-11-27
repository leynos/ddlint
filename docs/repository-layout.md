# Repository layout

This document describes the main repository paths for `ddlint` contributors. It
is the canonical guide to where source code, documentation, examples, and
validation assets belong.

## Tree overview

The following tree is intentionally compact. It shows the stable top-level
shape and the subdirectories that contributors most often need when navigating
the project.

```plaintext
.
├── .github/
│   └── workflows/
├── cli/
│   ├── src/
│   │   ├── config/
│   │   └── main.rs
│   └── tests/
│       └── support/
├── docs/
│   ├── archive/
│   └── execplans/
├── examples/
├── src/
│   ├── linter/
│   ├── parser/
│   ├── sema/
│   └── test_util/
├── tools/
│   └── verus/
├── verus/
│   └── smoke/
└── tests/
    ├── linter_runner/
    ├── rule_behaviour/
    └── support/
```

_Figure 1: Compact repository tree for contributor orientation._

## Top-level paths

- `.github/`: GitHub automation, dependency updates, and continuous
  integration workflows. Keep repository automation here rather than in ad hoc
  scripts.
- `.vale/` and `.vale.ini`: Prose linting configuration and vocabulary. Update
  these when documentation terminology changes deliberately.
- `AGENTS.md`: Repository-specific agent and contributor operating
  instructions. This file supersedes inherited template guidance for this
  repository.
- `Cargo.toml` and `Cargo.lock`: Rust workspace package metadata, the
  authoritative `[workspace.lints.*]` baseline, and dependency lock state. Keep
  dependency requirements compatible with the policy in `AGENTS.md`.
- `clippy.toml`: Numeric Clippy thresholds shared by the whole workspace. They
  match CodeScene's ceilings; see the
  [lint baseline](developers-guide.md#lint-baseline).
- `Makefile`: Canonical command entrypoints for build, tool checks and setup,
  format, lint, test, and documentation validation. Prefer these targets over
  direct tool invocations.
- `README.md`: Public project overview and entrypoint. Keep it high-level and
  link to deeper documentation rather than duplicating it.
- `docs/`: Long-lived project documentation, design records, guides, and
  execution plans. Use [documentation contents](contents.md) as the index.
- `examples/`: Differential Datalog examples used for manual inspection and
  parser behaviour checks. Keep examples small and representative.
- `cli/`: The `ddlint-cli` workspace package. It owns the `ddlint` binary,
  application configuration, and the application's integration tests, and it
  depends on the reusable `ddlint` library rather than the reverse.
- `src/`: Reusable `ddlint` library implementation. Modules must start with
  module-level documentation comments.
- `scripts/`: Runnable Python validation contracts, including the Clippy policy
  contract launched separately from Cargo's test process.
- `tests/`: Integration and behavioural tests. Shared integration helpers
  belong under `tests/support/`.
- `tools/`: Pinned data for local build-tool provisioning. `tools/mold/VERSION`
  names the `mold` release and `tools/mold/SHA256SUMS` holds the archive
  digests that `make install-build-tools` verifies before extraction.
  `tools/verus/VERSION` and `tools/verus/SHA256SUMS` pin the Verus release that
  the `install-verus` and `verus` targets read.
- `verus/`: Verus proof sources, outside the Cargo workspace. `make verus` runs
  them through the pinned `rust-prover-tools` CLI; see
  [the developer's guide](developers-guide.md#formal-verification).

## Source layout

The `src/` tree is organized by domain responsibility rather than by generic
layer names. The `cli/` package holds the application boundary described in
[ADR-0008](adrs/adr-0008-ortho-config-cli-boundary.md); OrthoConfig and Clap
stay confined to that package.

- `cli/src/main.rs`: Command-line entrypoint for the `ddlint` binary.
- `cli/src/lib.rs`: Application library surface exposing the configuration
  spine to integration tests.
- `cli/src/config/mod.rs`: Application configuration types, OrthoConfig
  loading, source precedence, and domain validation.
- `cli/tests/`: Application integration tests for configuration precedence,
  discovery, inheritance, error paths, and the binary's observable behaviour.
- `cli/tests/support/`: Shared fixtures for explicit configuration sources.
- `src/lib.rs`: Library crate root and public module surface.
- `src/language.rs`: Language-facing definitions shared across parser,
  semantic analysis, and linting.
- `src/tokenizer.rs`: Tokenization support.
- `src/syntax_utils.rs`: Shared syntax helpers.
- `src/parser/`: Parser, concrete syntax tree construction, span scanning,
  abstract syntax tree projections, and parser tests.
- `src/sema/`: Semantic analysis support such as symbol and scope handling.
- `src/linter/`: Lint rule abstractions, rule storage, runner integration, and
  concrete lint rules.
- `src/test_util/`: Shared test utilities that are part of the crate source
  tree.

Modules with child files use a directory entrypoint named `mod.rs`, keeping
module implementations beside their children without changing Rust module paths.

Relation primary-key suffix parsing and recovery live in the private
`src/parser/span_scanners/relations/suffix.rs` child module. Its two parent
entrypoints reuse the existing cursor helpers; the module belongs only to the
relation scanner and adds no public API.

## Parser module ownership

Each parser module owns the responsibilities named here. The conventions those
modules share, including delimiter and block-state rules, live in the
[developer's guide](developers-guide.md#boundary-rules).

### `src/parser/ast/expr/block.rs`

- Owns `BlockExpr`, the brace-delimited block value stored in
  `Expr::Block`.
- `statements` holds items followed by `;`; `value` holds the final item when
  it is not followed by `;`, so `None` means the block yields unit.
- `from_items` is the only constructor that applies the trailing-separator
  rule; `items()` and `into_items()` are the only traversal order.
- Keeps block-shape concerns out of `expr/mod.rs`, which owns the `Expr`
  variants themselves.

### `src/parser/expression/block.rs`

- Owns block item parsing: `parse_block`, `parse_block_items`,
  `finish_block`, and the private `classify_block_follow` classifier.
- Reads every item at `BLOCK_ITEM_MIN_BP` so an item cannot swallow the
  following `;`, and owns the empty-item and missing-separator diagnostics
  through `error_messages.rs` constants.
- Should remain the only place that maps `ItemPosition` and the next token to
  a block action; callers should not duplicate that classification.

### `src/parser/expression/pratt/mod.rs`

- Owns the Pratt parser entry points. `parse_expression` parses a free
  expression; `parse_rule_body_expression` parses a rule-body literal and
  rejects `return`, which a rule body cannot honour.
- The rule-body context lives in `Pratt::in_rule_body` and is entered only
  through `parse_rule_body_expression`. A closure body restores legality with
  `with_closure_body_scope`, which clears the flag for the body and restores
  the previous value afterwards, so a closure nested in a rule body keeps
  `return` while the surrounding literal still rejects it.
- `src/parser/expression_span.rs` validates rule-body literals through
  `parse_rule_body_expression`, so both the span-scanner path and the
  expression path report once, at the `return` keyword.

### `src/parser/ast/expr/sexpr.rs`

- Owns S-expression rendering helpers for `Expr`.
- Supports test and fixture comparisons without coupling callers to debug
  formatting.
- Should remain presentation-only; parsing and semantic classification belong
  elsewhere.

### `src/parser/ast/rule/classification.rs`

- Owns rule-body term classification for raw literals.
- Handles assignment parsing, aggregation detection, and `for`-loop lowering
  within the rule-body helper path.
- Keeps rule-body classification separate from the public `Rule` wrapper so
  `rule/mod.rs` stays focused on the surface API.

### `src/parser/span_scanners/rules_assign.rs`

- Owns the assignment alternative of the rule-body scanner: `lhs = rhs`, where
  the right-hand side may begin with `if`, `for`, or `skip`.
- Exists because the flat expression loop stops at those keywords; without it,
  `var y = if x > 1 { x } else { 0 }` scans as a left-hand side followed by a
  separate statement. Keyword statements are tried first, so a leading `if` or
  `for` keeps its own grammar.
- Scans each delimiter on the left-hand side as a balanced unit, so an `=`
  inside braces is not mistaken for the assignment separator.

### `src/parser/ast/relation/mod.rs`

- Owns the typed relation declaration surface for role, kind, ref marker, and
  body form.
- Treats `role()` and `kind()` as the canonical APIs for new code.
  `is_input()` and `is_output()` are derived helpers kept for callers that only
  need role predicates.
- Uses `role_keyword_present()` and `kind_keyword_present()` when callers need
  source-fidelity rather than the defaulted semantic value.
- Exposes declaration-level reference relations through `is_ref()`. Do not
  infer ref status from raw `&` tokens in downstream callers.
- Exposes `body()`, `element_type()`, `columns()`, and `primary_key()` as
  fallible queries returning `Result<_, RelationParseErrors>`. A valid record
  body yields `Ok(RelationBody::Fields(..))` with `element_type()` as
  `Ok(None)`; a valid bracket body yields `Ok(RelationBody::ElementType(..))`
  with `columns()` as `Ok(Vec::new())`. Malformed or missing bodies return
  `Err(..)` rather than an empty `Fields` vector, so direct AST querying stays
  reliable for malformed or synthetic nodes.
- Exposes `primary_key()` as the only supported way to read a key clause; it
  returns `PrimaryKey`, whose `binders()`, `binder_names()`, `span()`, and
  `expression()` cover the binder list, the clause span, and the optional
  spec-form expression. `PrimaryKeyExpression::parse()` parses the preserved
  text lazily with source-absolute error spans. Lint rules must not re-tokenize
  relation declaration text themselves; extend this surface instead.
  `Parsed::errors()` remains the parser-level diagnostic channel.

### `src/parser/span_scanners/relations/mod.rs`

- Owns top-level relation-candidate discovery, declaration orchestration,
  record and bracket body parsing, primary-key validation,
  relation-versus-rule/fact disambiguation, span collection, and recovery.
- Applies delimiter-depth-aware line-start filtering, so only genuine top-level
  declarations are treated as candidates, and emits the `D-REL-*` diagnostics.
- Produces relation declaration spans for CST construction;
  `src/parser/ast/relation/mod.rs` then exposes the typed AST accessors over
  the resulting nodes.

### `src/parser/span_scanners/relations/cursor.rs`

- Owns shared cursor movement, trivia handling, token inspection, and
  balanced-delimiter traversal (including delimiter-stack maintenance) for the
  relation scanner.
- Holds no grammar decisions; those remain in `relations/mod.rs`.

### `src/parser/span_scanners/relations/preamble.rs`

- Parses only the optional role/kind preamble keywords and enforces
  `D-REL-001`, `D-REL-002`, and `D-REL-003`.
- Keeps relation names, ref markers, bodies, primary keys, candidate discovery,
  and recovery in `relations/mod.rs`.

### `src/parser/span_scanners/utils.rs`

- Owns the shared span-collection mechanics: the `State` alias,
  `parse_and_record`, and `collect_with_parser`.
- `collect_with_parser` adds a single-keyword token walk on top of
  `parse_and_record`; it holds no grammar, and callers pass their grammar as a
  per-match parser factory.
- Only scanners whose policy equals `parse_and_record` may call it. See the
  [boundary rules](developers-guide.md#boundary-rules) for the excluded
  scanners and their reasons.

### `src/parser/span_scanners/typedefs.rs`

- Collects spans for `type` and `extern type` declarations; it finds statement
  boundaries rather than validating the complete type grammar. A top-level
  semicolon or newline ends a span, while nested delimiters keep internal
  newlines within it.
- Reuses `TypeDepths` and `adjust_type_depths` from
  `src/parser/span_scanners/indexes_support.rs` to track `()`, `[]`, `{}`, and
  `<>`; `>>` closes two angle levels. This support is shared with the index
  scanner. Legacy `typedef` diagnostics remain owned by
  `src/parser/reserved_tokens.rs`.

### `src/parser/expression/pratt/postfix.rs`

- Owns postfix dispatch for the Pratt parser.
- Routes function calls, bit slices, field access, tuple indexing, method
  calls, and delay postfixes to the appropriate helper.
- Coordinates the pending diff-marker state across the postfix chain.

### `src/parser/expression/pratt/diff.rs`

- Owns diff-marker tracking and validation.
- Wraps completed postfix expressions in `Expr::AtomDiff` when a diff marker
  is pending.
- Emits the targeted diagnostics for duplicate, misplaced, or dangling diff
  markers.

### `src/parser/expression/pratt/delay.rs`

- Owns `expr -<N>` postfix parsing.
- Consumes the `-<` token pair, reads the delay literal, and returns
  `Expr::AtomDelay` on success.
- Keeps delay-specific validation separate from the generic postfix loop.

### `src/parser/expression/pattern_collection.rs`

- Owns the shared pattern-collection context for `match` arms and `for`
  headers. A `PatternContext` carries a `PatternSite` (`MatchArm` or `ForLoop`)
  and supplies the diagnostics and delimiter rules for that site.
- Builds the context with `for_match_pattern()` or `for_for_loop_pattern()`.
  A match arm ends at `->`; a for-loop header ends at `in`.
- Applies site-specific behaviour through `validate_on_eof()` and
  `use_for_paren()`, which are true only for a for-loop pattern. An unbalanced
  for-loop header is therefore diagnosed before the missing `in`, and the
  `for (x) in xs` form is accepted.
- Renders site-specific text through `missing_terminator_msg()`,
  `unmatched_delimiter_msg()`, and `unexpected_end_msg()`.
- Defines `DelimiterKind` (`Paren`, `Brace`, `Bracket`).
  `unmatched_delimiter_msg()` takes it to name the offending delimiter in the
  site's message.

## Documentation layout

The `docs/` directory is the source of truth for project context that should
survive beyond one implementation branch.

- `docs/contents.md`: Index for the documentation set.
- `docs/documentation-style-guide.md`: Documentation authoring and formatting
  rules.
- `docs/repository-layout.md`: Reference for major documentation paths,
  including document roles and ownership responsibilities.
- `docs/ddlint-design.md`: Primary design document for `ddlint`.
- `docs/developers-guide.md`: Maintainer workflow and implementation guidance.
- `docs/roadmap.md`: Delivery roadmap and task sequencing.
- `docs/adr-*.md`: Architectural decision records.
- `docs/execplans/`: Execution plans for non-trivial implementation work.
- `docs/archive/`: Historical documents kept for traceability but no longer
  treated as current guidance.

## Test layout

Library integration tests live under `tests/`, with feature-oriented files for
parser, semantic, and linter behaviours. Shared fixtures and helper code belong
under `tests/support/`, while command-line runner scenarios belong under
`tests/linter_runner/`. Rule-body cases belong under `tests/rule_behaviour/`,
whose `main.rs` is the single integration entrypoint; its child files split
for-loop and delimiter cases without adding test crates.

Application integration tests live under `cli/tests/`, one file per behaviour:
configuration precedence (`config_layers.rs`), discovery and inheritance
(`config_discovery.rs`), error paths (`config_errors.rs`), and the binary's
observable placeholder behaviour (`placeholder_cli.rs`). Shared application
fixtures belong under `cli/tests/support/`.

Keep each Rust file within 400 lines. When an integration suite grows beyond
that limit, use a module directory with `main.rs` as the Cargo test entrypoint
and private child modules grouped by behaviour. The `rule_behaviour` suite
keeps assertion helpers in its entrypoint and groups aggregation, head,
for-loop, and body-boundary cases in sibling modules. Its fallible extractor and
`mixed_rule` fixture live in `tests/support/rule_behaviour.rs`; only the two
mixed-rule cases consume that fixture, so it does not imply broader reuse.

Keep test data close to the tests that own it unless it becomes large enough to
justify a dedicated fixture directory. When a helper mutates process-global
state, wrap that state behind a shared guard rather than duplicating local
locking in individual tests.
