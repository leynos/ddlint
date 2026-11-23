# 2.6.9 enforce the `LcName` case restriction on primary-key binders

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: DRAFT (awaiting design review and user approval)

Branch: `2-6-9-enforce-lc-name-case-restriction-on-primary-key-binders`

Roadmap item: `2.6.9` in `docs/roadmap.md`.

## Purpose / big picture

`ddlint` is a linter for Differential Datalog (DDlog). Its parser produces a
lossless concrete syntax tree (CST) built with the `rowan` crate. A first pass,
the "span scanner", walks the token stream and decides which byte ranges form
each top-level declaration; it also emits most declaration-level diagnostics.
Thin typed wrappers over the CST, the "AST wrappers", then give lint rules
structured access to each declaration.

A DDlog `input` relation may declare a primary key. The key starts with a
parenthesized list of "binders", names that stand for the row while the
optional key expression is evaluated:

```ddlog
input relation Book(row: BookRow) primary key (row) (row.author, row.title)
input relation R(a: u32, b: u32) primary key (a, b)
```

The syntax specification spells each binder as `LcName`: an identifier whose
first character is a lowercase ASCII letter or `_`, matching
`^(_|[a-z])[A-Za-z0-9_]*$`. Upstream DDlog agrees: its `key_expr` parser reads
`parens varIdent`, and `varIdent` is the lowercase identifier class. Today the
ddlint scanner accepts any identifier token, so this is accepted silently:

```ddlog
input relation R(x: u32) primary key (Foo)
```

After this change, that line reports exactly one diagnostic,
`D-REL-007: unexpected or malformed primary key clause`, whose span covers
`Foo`. The relation is not exposed, which is the existing recovery for every
`D-REL-007`. Binders such as `row`, `_x`, and `rowId` remain valid; only the
first character is case-restricted.

The roadmap item also asks for the same decision on import aliases
(`import foo::bar as baz;`). The specification currently spells the alias as
`LcName`, but upstream DDlog parses the alias with `modname`, a `::`-separated
path of identifiers of any case. Enforcing `LcName` there would reject valid
upstream programs. This plan therefore keeps alias acceptance case-independent
and corrects the specification, pinning the behaviour with regression tests.

Observable success: `make test` runs new tests that fail before the change and
pass after it, the conformance register's item 13 no longer lists the binder
gap, the specification's section 5.2 no longer claims an unenforced `LcName`
alias, and roadmap item 2.6.9 is checked.

## Constraints

- Implementation begins only after the user explicitly approves this plan.
- No new diagnostic code and no new message text. The case violation reuses
  `DiagnosticCode::RelationMalformedPrimaryKey` (`D-REL-007`) and its canonical
  `RELATION_MALFORMED_PRIMARY_KEY_ERROR` text in
  `src/parser/error_messages.rs`. The frozen `D-REL-001` to `D-REL-008` set in
  `src/parser/diagnostics/code.rs` must not change.
- No public API signature changes. `Relation::primary_key()`, `PrimaryKey`,
  `PrimaryKeyBinder`, `PrimaryKeyExpression`, and `Import::alias()` keep their
  signatures. Rustdoc may gain postcondition statements. New helpers are
  `pub(crate)` or narrower.
- Recovery for a rejected clause is unchanged: exactly one `D-REL-007`, the
  scanner skips the rest of the line, and no `N_RELATION_DECL` node is
  produced. ADR-0002 invariants `INV-INDEP`, `INV-SHIFT`, `INV-AGREE`, and
  `INV-TERM` must keep holding.
- No dependency, `Cargo.toml`, `Cargo.lock`, `Makefile`, or CI workflow
  changes. No new verification tooling (Kani, Verus targets, or crates).
- ddlint is pre-release: update callers and tests in place, add no migration
  diagnostics, no migration-guide entry, and no "breaking change" changelog
  framing.
- Every source file stays at or below 400 lines (Whitaker `module-max-lines`),
  and every new module, including `#[cfg(test)]` modules, starts with a `//!`
  inner doc comment (Whitaker `module-must-have-inner-docs`). Files already
  near the cap must not grow: `src/parser/tests/relations/mod.rs` (392 lines)
  and `src/parser/tests/relation_primary_key_strategies.rs` (384 lines).
- Every `assert!`, `assert_eq!`, and `assert_ne!` carries a diagnostic message.
  Tests prefer `expect(...)` to `unwrap()`. Production code uses no `expect`.
- Comments and documentation use en-GB-oxendict spelling.
- Completed ExecPlans in `docs/execplans/` are historical and must not be
  edited.
- Transformer declaration grammar (register item 12) is out of scope even
  though this plan records an upstream divergence there (see `D9`).

## Tolerances (exception triggers)

Comparison base: `git merge-base HEAD origin/main`. All counts exclude every
file under `docs/execplans/`, including this plan. Counts measure net lines
(additions minus deletions) per category, using `git diff --numstat`. Each
category is enforced independently; spare capacity in one category never
offsets another.

Classification rules (whole-file):

- Code: `*.rs` anywhere, and `tests/features/*.feature` (executable
  behavioural specifications).
- Configuration: `Cargo.toml`, `Cargo.lock`, `Makefile`, `.github/**`,
  `typos*.toml`, `clippy.toml`, `rust-toolchain*`, `.cargo/**`.
- Documentation: `docs/**/*.md` except `docs/execplans/**`, `CHANGELOG.md`,
  `README.md`.
- Generated exclusion: files under `proptest-regressions/` are generated by
  proptest; they are excluded from line counts but any new file there must be
  reviewed and mentioned in `Surprises & discoveries`.

Counting commands, run from the repository root:

```sh
base=$(git merge-base HEAD origin/main)
git diff --numstat "$base" -- '*.rs' 'tests/features/*.feature' \
  | awk '{a+=$1; d+=$2; n++} END {printf "code: %d files, net %d\n", n, a-d}'
git diff --numstat "$base" -- 'docs/*.md' 'docs/**/*.md' CHANGELOG.md README.md \
  ':(exclude)docs/execplans/**' \
  | awk '{a+=$1; d+=$2; n++} END {printf "docs: %d files, net %d\n", n, a-d}'
git diff --numstat "$base" -- Cargo.toml Cargo.lock Makefile '.github/**' \
  'typos*.toml' clippy.toml 'rust-toolchain*' '.cargo/**' \
  | awk '{a+=$1; d+=$2; n++} END {printf "config: %d files, net %d\n", n, a-d}'
```

Limits:

- Code: stop and escalate beyond 16 files or 900 net lines.
- Documentation: stop and escalate beyond 12 files or 320 net lines.
- Configuration: stop and escalate on any change (budget 0 files, 0 lines).
  `make spelling` regenerating `typos.toml` is the one foreseeable exception;
  if it happens, stop and follow the `AGENTS.md` rule of committing an
  unrelated regeneration in a separate base pull request.
- Interface: stop if any public signature must change, or if a new
  `DiagnosticCode` variant or message appears necessary.
- Dependencies: stop if any new crate or tool is needed.
- Scanner scope: stop if enforcing the binder rule requires changing scanner
  recovery (`skip_line` behaviour), the `;` terminator rule, or the role and
  bracket precedence of `D-REL-004`, `D-REL-006`, and `D-REL-008`.
- Refactor scope (`EP-M4`): stop and leave a call site unconsolidated if its
  input is not provably a single identifier token, recording the reason.
- Exhaustive test runtime: if the bounded enumeration in `EP-M2` takes more
  than 10 seconds under `make test`, reduce the bound by one and record the
  change; stop if the reduced bound no longer reaches every transition named in
  `Verification plan`.
- Iterations: stop if a gate still fails after three fix rounds.
- Time: stop if any milestone takes more than four hours of active work.
- Ambiguity: stop if the import-alias decision (`D6`) or the span-anchor
  decision (`D3`) is challenged by evidence found during implementation.

Budget rationale. The production change is small: a crate-private predicate
module (about 60 lines with rustdoc and unit tests), a verdict return type in
`src/parser/span_scanners/relations/suffix.rs` (about 40 lines), and a
`try_map` in `src/parser/ast/parse_utils/relation.rs` (about 10 lines).
Historical plans show tests dominate: 2.6.6.1 set 1,100 net Rust lines and
finished at 2,062, with the excess attributed to tests (decision `D13` there);
2.6.8 set 1,600 code lines and finished at 3,120. 2.6.5, the transformer-case
precedent, set 260 lines and did not overrun. This plan budgets tests
explicitly: about 120 lines of `rstest` and snapshot cases, about 220 lines of
bounded enumeration, about 200 lines of property tests, about 60 lines of BDD
scenarios and steps, and about 40 lines of import-alias regression tests,
giving roughly 750 lines; 900 leaves 20% contingency. The `EP-M4` refactor is
net negative. Documentation touches about 11 files at 10 to 50 lines each.

## Risks

- Risk: an existing test, fixture, example, or proptest strategy generates an
  uppercase-initial binder and starts failing. Severity: low. Likelihood: low.
  Mitigation: reconnaissance grep found none (binder strategy is
  `prop::sample::select(vec!["a", "b", "c"])` in
  `src/parser/tests/relation_primary_key_strategies.rs`); `make test` in
  `EP-M1` confirms.
- Risk: the scanner and the accessor disagree on some binder list once the
  case check is added to both, breaking ADR-0002 `INV-AGREE`. Severity: medium.
  Likelihood: low. Mitigation: one shared predicate, plus the `EP-M2` bounded
  enumeration that compares both against an independent oracle.
- Risk: the span-anchor rule (`D3`) surprises users when a list is both
  structurally malformed and case-invalid, such as `(Foo,)`. Severity: low.
  Likelihood: medium. Mitigation: a deterministic precedence rule (structure
  first) with named tests and documentation.
- Risk: consolidating the duplicated lowercase helpers in `EP-M4` changes
  pattern or qualified-call behaviour because a call site passes text that is
  not a single identifier token. Severity: medium. Likelihood: low. Mitigation:
  lemma `L-LEX` holds only for single tokens; each call site is checked, and
  the refactor tolerance leaves unproven sites unchanged.
- Risk: the bounded enumeration is slow in debug builds because the accessor
  runs a `chumsky` parser per case. Severity: low. Likelihood: medium.
  Mitigation: the runtime tolerance and a documented bound reduction.
- Risk: reviewers expect import aliases to be lowercase because the
  specification said so. Severity: low. Likelihood: medium. Mitigation: `D6`
  cites the upstream `modname` grammar and the specification is corrected in
  the same change.

## Progress

- [x] (2026-11-23 18:00Z) Branch renamed; Lody session titled.
- [x] (2026-11-23 18:20Z) Reconnaissance completed with a Wyvern agent team
  (scanner and AST, imports and transformers, verification and documentation
  infrastructure) and upstream grammar lookup.
- [x] (2026-11-23 18:50Z) Initial draft written.
- [ ] Community-of-experts design review applied.
- [ ] Plan approved by the user.
- [ ] `EP-M1` binder case enforcement in scanner and accessor.
- [ ] `EP-M2` bounded enumeration, property tests, and snapshot.
- [ ] `EP-M3` import-alias decision pinned and specification corrected.
- [ ] `EP-M4` consolidate duplicated lowercase-identifier helpers (refactor).
- [ ] `EP-M5` documentation, register, roadmap, and delivery closure.

## Surprises & discoveries

- Observation: upstream DDlog import aliases accept any case and even a
  scoped path. Evidence: upstream `Parse.hs` (fetched 2026-11-23) defines
  `imprt = Import nopos <$ reserved "import" <*> modname <*> (option
  (ModuleName []) $ reserved "as" *> modname)`
  with ``modname = ModuleName <$> modIdent `sepBy1` reservedOp "::"`` and
  `modIdent = identifier`. Impact: enforcing `LcName` on aliases would reject
  valid upstream DDlog, so `D6` corrects the specification instead. ddlint
  accepts only a single-segment alias, which is a separate subset gap recorded
  in the register by `EP-M3`.
- Observation: upstream transformer names are uppercase, not lowercase.
  Evidence: upstream `Parse.hs` defines `transIdent = ucScopedIdentifier`, and
  `lib/graph.dl` declares `extern transformer SCC(...) -> (SCCLabels: ...)`.
  ddlint's section 5.4 and scanner require a lowercase transformer name and a
  `:` output list. Impact: the roadmap item's model ("the way transformer names
  already do") is itself a ddlint-specific rule. It does not change this plan,
  because the binder rule is independently grounded in upstream `varIdent`, but
  `EP-M5` records the divergence (see `D9`).
- Observation: upstream accepts primary keys on bracket-bodied relations
  (`relation R[T] primary key (x) ...`), while ddlint rejects them with
  `D-REL-004`. Evidence: the upstream `relation` parser calls `key_expr` in
  both body branches. Impact: none for this item; noted for the register's item
  13 follow-up in `EP-M5`.
- Observation: CodeGraph MCP failed to connect during planning, so
  reconnaissance used text search. Impact: none on the plan's facts, which were
  verified by reading the cited files at revision `7adbd8bc`.

## Decision log

- D1. Decision: enforce `LcName` on primary-key binders rather than record an
  intentional superset. Rationale: the specification, the conformance register,
  and the roadmap all describe the acceptance of `primary key (Foo)` as a gap,
  and upstream `key_expr` uses `varIdent`, the lowercase class. Recording a
  superset would legitimize input upstream DDlog rejects. The rejected
  alternative (document the superset) costs less but contradicts the normative
  grammar and the upstream-fidelity direction. Date/Author: 2026-11-23,
  planning agent.

- D2. Decision: report the violation as `D-REL-007` with its canonical message;
  add neither a new code nor a case-specific message. Rationale: the roadmap
  names `D-REL-007`; `DiagnosticCode` maps one canonical message per code
  through `message_text()`; the stable-surface rule in
  `docs/parser-conformance-register.md` says message text is not an identifier;
  and the span (`D3`) already tells the user which binder is wrong. Rejected
  alternatives: a new `D-REL-009` (additive but unnecessary, and it fragments
  one clause-shape contract), and a second `D-REL-007:` message (breaks the
  one-message-per-code shape the macro encodes). Date/Author: 2026-11-23,
  planning agent.

- D3. Decision: anchor a case-violation `D-REL-007` at the first binder whose
  name is not `LcName`. A structurally malformed list (anything other than a
  comma-separated identifier list) keeps its existing anchor, the binder list's
  opening `(`, and takes precedence over case. Rationale: the transformer
  precedent anchors at the offending name, and a single canonical message needs
  a precise span to be actionable. Structure first keeps existing diagnostics
  byte-for-byte unchanged. Rejected alternative: always anchor at `(`, which is
  simpler but leaves the user to find the binder. Date/Author: 2026-11-23,
  planning agent.

- D4. Decision: enforce the same predicate in the accessor
  (`primary_key_clause` in `src/parser/ast/parse_utils/relation.rs`), not only
  in the scanner. Rationale: ADR-0002 `INV-AGREE` states that the scanner and
  the accessor agree on clause well-formedness. Enforcing only in the scanner
  would keep `parse()` correct but let hand-built CSTs expose uppercase binders
  through `PrimaryKey::binders()`. Rejected alternative: scanner-only, the
  smaller change, which silently weakens a documented invariant. Date/Author:
  2026-11-23, planning agent.

- D5. Decision: add one crate-private predicate,
  `is_lc_name(text: &str) -> bool`, implementing the full section 2.2 regex, in
  a new module `src/parser/ident_case.rs`, and use it from the scanner and the
  accessor. `EP-M4` then replaces the private duplicates in
  `src/parser/span_scanners/transformers.rs`, `src/parser/pattern/mod.rs`, and
  `src/parser/expression/qualified.rs`, in a separate refactor commit.
  Rationale: the change needs the predicate in two places and the repository
  already has four private copies; one predicate removes drift risk. A
  full-regex predicate is correct for any string, whereas the first-character
  copies are correct only for lexer identifier tokens (lemma `L-LEX`). Rejected
  alternative: a third private first-character copy in `suffix.rs`.
  Date/Author: 2026-11-23, planning agent.

- D6. Decision: do not enforce a case class on import aliases; correct
  specification section 5.2 to the upstream rule and pin current behaviour with
  tests. Rationale: upstream parses the alias as `modname`, any-case
  identifiers separated by `::`. Enforcing `LcName` would make ddlint reject
  valid upstream programs, the opposite of the roadmap's conformance aim. This
  is a specification correction, not an intentional superset. ddlint's
  single-segment restriction is a subset gap recorded in the register, not
  fixed here. Rejected alternative: enforce `LcName` for symmetry with binders.
  Date/Author: 2026-11-23, planning agent.

- D7. Decision: no Verus proof and no Kani harness. Use a bounded exhaustive
  enumeration over the production recognizers, a property test at the public
  `parse()` boundary with generator-labelled classes, `rstest` tables, BDD
  scenarios, and an inline `insta` snapshot. Rationale: see
  `Formal verification decision`. Date/Author: 2026-11-23, planning agent.

- D8. Decision: the import-alias tests in `EP-M3` are characterization tests
  and are expected to pass on first run, so that milestone has no red stage.
  Rationale: `D6` preserves behaviour; the tests guard the decision against a
  later "fix" that enforces `LcName`. The negative control is a seeded fault
  (temporarily rejecting uppercase aliases) that must turn them red.
  Date/Author: 2026-11-23, planning agent.

- D9. Decision: record, but do not act on, the transformer divergence from
  upstream (uppercase `transIdent`, `->` outputs). `EP-M5` adds a known
  limitation to register item 12 and proposes a new unchecked roadmap item to
  decide transformer alignment. Rationale: it is a separate grammar decision
  with its own blast radius. The roadmap entry is proposed for user approval
  with this plan. Date/Author: 2026-11-23, planning agent.

## Outcomes & retrospective

Not started. To be completed at each milestone and at closure.

## Context and orientation

Inspected base revision: `7adbd8bcba7ad139c6cb26dfcce59c66da826634`
(`codify brace blocks and adopt upstream map literals (2.6.8) (#408)`). Line
numbers below refer to that revision; relocate symbols with
`grep -rn '<symbol>' src tests`.

### Pipeline

`ddlint::parse(src)` in `src/parser/mod.rs` tokenizes with the `logos` lexer in
`src/tokenizer.rs`, runs the span scanners in `src/parser/span_scanners/`,
builds the CST, and returns `Parsed`, whose `errors()` holds
`chumsky::error::Simple<SyntaxKind>` diagnostics. `parse_with_observer()`
additionally reports diagnostic records to a `ParseObserver`; codes are derived
from the `D-REL-xxx:` message prefix by `DiagnosticCode::from_message` in
`src/parser/diagnostics/code.rs`.

The lexer's identifier rule is `#[regex(r"[A-Za-z_][A-Za-z0-9_]*")] Ident`
(`src/tokenizer.rs`, line 23). Keywords become `K_*` kinds through
`keyword_kind`; notably `_` alone lexes as `K_UNDERSCORE`, not `T_IDENT`.

### Relation scanner (owner of the binder rule)

`collect_relation_spans` in `src/parser/span_scanners/relations/mod.rs` (279
lines) calls `parse_relation_decl`, which calls `parse_relation_suffix` in
`src/parser/span_scanners/relations/suffix.rs` (237 lines).
`parse_primary_key_clause` (lines 83 to 113) consumes `primary` and `key`,
records `binder_list_span = current_span(...)` (the opening `(` token), runs
`parse_balanced_block` to the matching `)`, then:

```rust
if !has_column || !is_binder_identifier_list(tokens, list_start, *cursor) {
    return Err(Box::new(custom_error(&binder_list_span, D_REL_007)));
}
```

`is_binder_identifier_list` (lines 193 to 225) is a two-state recognizer over
token kinds: it skips trivia (`T_WHITESPACE`, `T_COMMENT`, via
`cursor::is_trivia`), accepts `T_IDENT` when an identifier is expected and
`T_COMMA` otherwise, and rejects everything else. It never reads token text, so
`Foo` passes. `D_REL_007` is
`DiagnosticCode::RelationMalformedPrimaryKey.message_text()`, aliased at
`relations/mod.rs` line 22. Any `Err` from the clause reaches
`handle_candidate` in `relations/mod.rs`, which pushes the error and calls
`st.skip_line()`; no relation span is recorded. The clause is parsed before the
role and body checks, so `output R(x: u32) primary key (Foo)` reports
`D-REL-007`, not `D-REL-006`; existing malformed lists behave the same way.

### Accessor (typed AST)

`Relation::primary_key()` in `src/parser/ast/relation/primary_key/mod.rs` (349
lines) calls `interpret_suffix` in
`src/parser/ast/relation/primary_key/clause.rs` (206 lines), which calls
`parse_binder_list`, which runs `primary_key_clause(rest)` from
`src/parser/ast/parse_utils/relation.rs` (82 lines). That `chumsky` 0.9 parser
binds `just(SyntaxKind::T_IDENT)` to `(name, span)` without a case check. Parse
errors are rendered by `render_parse_error` in
`src/parser/ast/relation/mod.rs`; a `SimpleReason::Custom` message is returned
verbatim, so a `Simple::custom(span, D_REL_007)` surfaces as the canonical
text. Hand-built CST tests live in
`src/parser/ast/relation/primary_key/tests.rs` (354 lines).

### Import aliases

`collect_import_spans` in `src/parser/span_scanners/imports.rs` (55 lines)
parses `just(K_AS).padded_by(ws).ignore_then(ident_parser)` with `ident()` from
`src/parser/lexer_helpers.rs`, a bare `T_IDENT` match. `Import::alias()` in
`src/parser/ast/import.rs` (99 lines) returns the first `T_IDENT` after `K_AS`.
Tests in `src/parser/tests/imports.rs` (56 lines) use only lowercase aliases.
No uppercase alias exists in any test, fixture, example, or document.

### Existing case helpers (all private)

- `is_lowercase_ident` in `src/parser/span_scanners/transformers.rs`
  (lines 276 to 281): first character is `_` or ASCII lowercase. Used by
  `push_invalid_name_diagnostic_if_needed`, which emits the uncoded
  `CAPITALIZED_TRANSFORMER_NAME_ERROR`.
- `is_lowercase_ident` and `is_uppercase_ident` in
  `src/parser/pattern/mod.rs` (lines 332 to 344, file 347 lines): the same
  first-character checks, applied to single identifier tokens at lines 163,
  230, and 300.
- `is_lowercase_identifier_segment` in `src/parser/expression/qualified.rs`
  (lines 42 to 47): the full `LcName` regex over a path segment.

### Tests and tooling

Dev-dependencies (`Cargo.toml` lines 34 to 46): `rstest` 0.28, `rstest-bdd` and
`rstest-bdd-macros` 0.6.0, `insta` 1.47.2, `proptest` 1.6.0, `trybuild`,
`googletest` 0.14.4, `pretty_assertions` 1.4.1. Primary-key coverage lives in
`src/parser/tests/relation_terminators.rs` (160 lines; `custom_error_messages`
helper and `non_identifier_binders_are_rejected`),
`src/parser/tests/relation_primary_key_props.rs` (303 lines),
`src/parser/tests/relation_primary_key_strategies.rs` (384 lines),
`tests/relation_primary_key_bdd.rs` (340 lines) with
`tests/features/relation_primary_key.feature`. Inline `insta` snapshots are
already used in `src/parser/tests/block_expression_snapshots.rs` and
`src/parser/ast/relation/primary_key/tests.rs`. There are no `.snap` files.

Verification tooling: `make verus` runs the pinned `prover-tools` Verus lane on
standalone files under `verus/` (only `verus/smoke/main.rs` exists); it is
outside `make test` and `make lint`. Kani is not configured. The `cli/` crate
does not invoke the parser, so the public end-to-end boundary is the library API
(`ddlint::parse`, `parse_with_observer`) exercised from `tests/`.

Gates: `make check-fmt`, `make lint` (interrogate, Clippy, Whitaker, spelling),
`make test` (all targets plus doctests), `make markdownlint`, `make nixie`.

### Budget survey

Comparable plans:
`docs/execplans/2-6-5-align-transformer-declaration-grammar.md` (the case-rule
precedent; 10 files and 260 lines, no overrun),
`docs/execplans/2-6-6-1-ast-access-for-spec-form-primary-key-exprs.md` (28
files and 1,100 lines; finished at 2,062 lines, excess in tests, escalated as
`D13`), and `docs/execplans/2-6-8-decide-brace-group-extension-policy.md` (code
1,600 lines; finished at 3,120; documentation 1,200 lines, finished at 835).
The repeated overruns were missed test scope, not approved scope growth, so
this plan itemizes test volume in `Tolerances`.

### Related documentation and skills

- `docs/differential-datalog-parser-syntax-spec-updated.md`, sections 2.2, 5.2,
  5.5, 9, 12, and 14.
- `docs/parser-conformance-register.md`, items 12 and 13 and the stable
  diagnostic compatibility surface.
- `docs/adrs/adr-0002-relation-role-kind-modelling.md`, the primary-key
  amendment and its invariants.
- `docs/parser-implementation-notes.md`, `docs/ddlint-design.md` (section 2),
  `docs/users-guide.md` ("Relation declarations", "Relation diagnostics"),
  `docs/developers-guide.md` ("Parser test helpers", "Property-test ownership",
  "Formal verification").
- `docs/building-an-error-recovering-parser-with-chumsky.md`,
  `docs/rust-parser-testing-comprehensive-guide.md`,
  `docs/rust-testing-with-rstest-fixtures.md`,
  `docs/complexity-antipatterns-and-refactoring-strategies.md`.
- Skills: `execplans`, `rust-router`, `rust-unit-testing`, `proptest`,
  `rust-verification`, `rust-state-machines` (recognizer shape),
  `hexagonal-architecture` (keep the case policy a pure domain predicate with
  no I/O), `en-gb-oxendict-style`, `commit-message`, `pr-creation`.

## Conformance basis

Upstream artefacts, inspected at revision `7adbd8bc`:

- Roadmap: `docs/roadmap.md` item 2.6.9.
- Normative grammar: `docs/differential-datalog-parser-syntax-spec-updated.md`
  section 2.2 (case classes), section 5.2 (`Import`), section 5.5
  (`PrimaryKey ::= 'primary' 'key' '(' LcName (',' LcName)* ')' Expr?`).
- Register: `docs/parser-conformance-register.md` items 12 and 13.
- ADR: `docs/adrs/adr-0002-relation-role-kind-modelling.md` (primary-key
  amendment; `INV-INDEP`, `INV-SHIFT`, `INV-AGREE`, `INV-TERM`).
- Upstream DDlog: `vmware/differential-datalog`, `master`,
  `src/Language/DifferentialDatalog/Parse.hs` and `lib/graph.dl`, fetched
  2026-11-23.
- No Terms of Reference document exists for this item.

Requirements:

- `REQ-BINDER-LC`: a primary-key binder whose name is not `LcName` is
  rejected with `D-REL-007`.
- `REQ-BINDER-SPAN`: that diagnostic's span is the offending binder, and
  existing structural `D-REL-007` spans are unchanged.
- `REQ-AGREE`: scanner and accessor accept exactly the same binder lists.
- `REQ-ALIAS-DECIDED`: the import-alias case question is decided and recorded
  in the specification, with tests pinning the decision.
- `REQ-DOCS`: the specification, register, users' guide, implementation
  notes, ADR-0002, changelog, and roadmap reflect the outcome.

Gaps:

- `GAP-BINDER-CASE`: `is_binder_identifier_list` and `primary_key_clause`
  accept any `T_IDENT` (register item 13 known limitation).
- `GAP-ALIAS-SPEC`: specification section 5.2 claims an unenforced `LcName`
  alias that contradicts upstream.

Trace links:

```plaintext
roadmap 2.6.9 -> spec 5.5 -> REQ-BINDER-LC, GAP-BINDER-CASE -> EP-M1 -> relation_binder_case::uppercase_binders_are_rejected
roadmap 2.6.9 -> spec 5.5 -> REQ-BINDER-SPAN -> EP-M1 -> relation_binder_case::binder_case_diagnostics_snapshot
ADR-0002 INV-AGREE -> REQ-AGREE -> EP-M1, EP-M2 -> binder_list_enumeration::scanner_and_accessor_match_oracle
roadmap 2.6.9 -> spec 5.2 -> REQ-ALIAS-DECIDED, GAP-ALIAS-SPEC -> EP-M3 -> imports::alias_case_is_not_restricted
roadmap 2.6.9 -> REQ-DOCS -> EP-M5 -> make markdownlint, make nixie, roadmap checkbox
```

## Verification plan

### Abstract model

Let the binder list be the token sequence strictly between the `(` and its
matching `)` after `primary key`. Classify each token as one of:

- `I(t)`: a `T_IDENT` token with text `t`;
- `C`: `T_COMMA`;
- `W`: trivia (`T_WHITESPACE` or `T_COMMENT`);
- `X(k)`: any other kind `k` (including `K_UNDERSCORE`, `T_DOT`, `T_NUMBER`).

Let `strip(w)` delete every `W`. Define:

- Structural well-formedness `S(w)`: `strip(w)` matches `I (C I)*`.
- Case predicate `LC(t)`: `t` matches `^(_|[a-z])[A-Za-z0-9_]*$`.
- Verdict `V(w)`:
  - `Malformed` when `not S(w)`;
  - `NonLc(i)` when `S(w)` and `i` is the index of the first `I(t)` with
    `not LC(t)`;
  - `WellFormed` otherwise.

The required behaviour, for every relation whose clause reaches the binder
check:

- `WellFormed`: the scanner accepts; `PrimaryKey::binder_names()` returns the
  `t` of each `I(t)` in order.
- `Malformed`: exactly one `D-REL-007` spanning the `(` token; no relation.
- `NonLc(i)`: exactly one `D-REL-007` spanning token `i`; no relation.
- The accessor returns `Ok` exactly when `V(w) = WellFormed`.

Precondition: the list is balanced (otherwise `parse_balanced_block` reports
its own error first, unchanged by this plan). Malformed input outside the list
(for example `primary value`) keeps its existing behaviour.

### Invariants and lemmas

- `INV-VERDICT` (`REQ-BINDER-LC`, `REQ-BINDER-SPAN`): the scanner's outcome and
  diagnostic span equal those dictated by `V(w)` for every binder list.
- `INV-CASE` (`REQ-BINDER-LC`): every binder exposed by
  `Relation::primary_key()` satisfies `LC`, whether the CST came from `parse()`
  or was built by hand.
- `INV-ONE` (preserved): a rejected clause yields exactly one `D-REL-007`, no
  relation node, and the next line is scanned normally.
- `INV-AGREE` (preserved and strengthened, `REQ-AGREE`): for every `w`, the
  scanner accepts if and only if the accessor accepts.
- `INV-ALIAS` (`REQ-ALIAS-DECIDED`): `import p as a;` is accepted and
  `Import::alias()` returns `a` verbatim for any `T_IDENT` text `a`, whatever
  its case.
- `INV-REFAC` (`EP-M4`): transformer-name, pattern, and qualified-call
  classification are unchanged after the helpers are consolidated.
- `L-LEX`: for every string `t` in the lexer's identifier language
  `[A-Za-z_][A-Za-z0-9_]*`, `LC(t)` holds if and only if the first character of
  `t` is `_` or ASCII lowercase. This connects the full-regex predicate to the
  first-character helpers it replaces, and is what makes `INV-REFAC` hold for
  single-token call sites.
- `L-STRUCT-FIRST`: if `not S(w)`, the verdict is `Malformed` irrespective of
  any `LC` value; case is examined only on structurally well-formed lists. This
  makes the anchor rule deterministic.
- `L-FINITE`: the production recognizer is a finite-state machine whose state
  is `(expect_identifier, saw_identifier, first_non_lc)`, where `first_non_lc`
  is written at most once. Every reachable state-and-symbol transition is
  exercised by some sequence of length at most 4. Combined with the bounded
  enumeration, this extends `INV-VERDICT` and `INV-AGREE` from the bound to all
  lengths, provided the implementation keeps no counters or unbounded state
  (checked by review of the final code).

### Axioms (trusted, not verified here)

- `AXIOM-LEXER`: `logos` 0.13 tokenizes exactly the strings matching
  `[A-Za-z_][A-Za-z0-9_]*` that are not keywords as `T_IDENT`, and `_` alone as
  `K_UNDERSCORE`. Boundary: the public `parse()` property test in `EP-M2`
  exercises the real lexer.
- `AXIOM-CHUMSKY`: `chumsky` 0.9 `try_map` reports a `Simple::custom` error at
  the given span and `separated_by(...).at_least(1)` has its documented
  semantics. Boundary: the accessor enumeration runs the real parser.
- `AXIOM-UPSTREAM-PK`: upstream `key_expr = parens varIdent` with
  `varIdent = lcIdentifier` (identifier start `_` or lowercase).
- `AXIOM-UPSTREAM-IMPORT`: upstream alias is `modname`, any-case identifiers
  separated by `::`.

### Methods per obligation

- Obligation: `INV-VERDICT`, `INV-AGREE`, and `L-STRUCT-FIRST` for all binder
  lists of bounded length.
  - Method: bounded exhaustive enumeration in an ordinary `cargo test`.
  - Rationale: the recognizers are finite-state and the alphabet is small, so
    exhaustive enumeration of every short sequence is cheap and reaches every
    transition; this checks the production code itself, not a model.
  - Domain: every sequence of length 0 to 6 over the seven symbols `I("a")`,
    `I("_b")`, `I("Cd")`, `C`, `W(" ")`, `W("/*c*/")`, and `X(T_DOT)`
    (137,257 sequences), each framed as `primary key (` … `)`. Tokens are
    constructed directly with computed spans, bypassing the lexer so adjacent
    identifiers stay distinct tokens.
  - Artefact: `src/parser/span_scanners/relations/binder_list_tests.rs`
    (`#[cfg(test)]`, declared from `relations/mod.rs`).
  - Evidence: `cargo test --lib binder_list_enumeration` passes; before
    `EP-M1`'s green step, the oracle comparison fails on `(Cd)`.
  - Non-vacuity: the test counts sequences per verdict class and asserts each
    of `WellFormed`, `Malformed`, `NonLc(0)`, and `NonLc(i > 0)` is reached at
    least once, and that a `Malformed` sequence containing `I("Cd")` exists
    (exercising `L-STRUCT-FIRST`). The oracle implements `V` directly from the
    EBNF (strip trivia, split on `C`, check segment shapes, then case), which
    is structurally different from the two-state scanner loop and from the
    `chumsky` combinator. Seeded fault: temporarily make the scanner accept
    `I("Cd")`; the enumeration must fail with a counterexample naming that
    sequence. Revert the fault before committing.
- Obligation: `INV-VERDICT`, `INV-ONE`, and `INV-CASE` through the public
  boundary over a broad name space.
  - Method: `proptest` property over `ddlint::parse`.
  - Rationale: covers the real lexer, scanner recovery, and accessor together
    with names beyond the enumeration's fixed spellings.
  - Domain: one to four binders, each drawn from a generator-labelled class:
    `Lower` (`[a-z][A-Za-z0-9_]{0,6}`), `Underscore` (`_[A-Za-z0-9_]{1,6}`),
    `Upper` (`[A-Z][A-Za-z0-9_]{0,6}`); names that lex as keywords are
    discarded. Lists are rendered with `", "` separators, an optional trailing
    expression, and a second relation on the next line.
  - Artefact: `src/parser/tests/relation_binder_case_props.rs`.
  - Evidence: the property passes under `make test`. The expected verdict
    comes from the generator's class labels, never from `is_lc_name`.
  - Non-vacuity: a fixed-seed companion test asserts that every class, the
    first-binder-upper case, and the later-binder-upper case are each
    generated, and that keyword rejection discards under 5% of cases. Seeded
    fault: as above.
- Obligation: named contract cases for `REQ-BINDER-LC` and `REQ-BINDER-SPAN`.
  - Method: `rstest` table plus one inline `insta` snapshot of rendered
    diagnostics (`code`, byte span, sliced source text) for a small program.
  - Artefact: `src/parser/tests/relation_binder_case.rs`.
  - Cases: `(Foo)`, `(a, Bad)`, `(Foo, bar)`, `(Foo,)` (structure first, span
    at `(`), `(_x)` accepted, `(rowId)` accepted, `(a_B9)` accepted,
    `output relation R(x: u32) primary key (Foo)` (still `D-REL-007`), and
    `input relation R(row: T) primary key (Row) Row.id` (expression does not
    rescue the binder), and
    `"input relation R(x: u32) primary key (Foo)\noutput relation S(x: u32)"`
    (recovery: only `S` is exposed).
  - Non-vacuity: every rejected case asserts exactly one `D-REL-007` and zero
    relations; every accepted case asserts zero errors and the exact
    `binder_names()`.
- Obligation: `INV-CASE` for hand-built CSTs (accessor without the scanner).
  - Method: named unit test.
  - Artefact: `src/parser/ast/relation/primary_key/tests.rs` if it stays at or
    below 400 lines; otherwise a new sibling `case_tests.rs`.
  - Evidence: `primary_key()` on a synthetic relation with binder `Foo`
    returns `Err` containing the `D-REL-007` text.
- Obligation: `INV-ALIAS`.
  - Method: `rstest` table in `src/parser/tests/imports.rs` covering `as baz`,
    `as Baz`, `as BAZ`, and `as _baz`, asserting no errors and the exact
    `alias()`.
  - Non-vacuity: seeded fault (reject uppercase aliases in
    `collect_import_spans`) must fail the `Baz` and `BAZ` rows.
- Obligation: `L-LEX` and `INV-REFAC`.
  - Method: `proptest` over `is_lc_name` with generator-labelled classes, plus
    a property that, for strings from `[A-Za-z_][A-Za-z0-9_]{0,8}`,
    `is_lc_name` agrees with the first-character rule; existing transformer,
    pattern, and qualified-call tests stay green unchanged.
  - Artefact: the `#[cfg(test)]` module in `src/parser/ident_case.rs`.
  - Non-vacuity: generated classes include empty strings, digit-led strings,
    strings containing `-` or `:`, and non-ASCII letters, all expected false.
- Obligation: `INV-ONE` recovery and ADR-0002 invariants.
  - Method: existing tests in `relation_terminators.rs`,
    `relation_primary_key_props.rs`, and the BDD feature stay green; the new
    BDD scenarios add a following-line relation to show recovery.

### Formal verification decision

Verus is not warranted for this item, and Kani is not adopted. The obligations
concern two finite-state recognizers of a regular language over tokens. Lemma
`L-FINITE` shows that every distinct behaviour appears at short lengths, so a
bounded exhaustive check over the production code, rather than over a
transcription, gives the decisive evidence. The repository's Verus lane
verifies standalone files under `verus/`, not the production crate, so a Verus
proof here would establish the property for a model and still need a
correspondence test to say anything about the shipped scanner; it would add
proof maintenance without strengthening the link to the code users run. Kani
would check the production code, but it is not configured, and adding it is a
tooling and configuration change this item does not justify when plain
enumeration reaches the same bound. The precedent is 2.6.6.1 decision `D5` and
ADR-0006, which made the same choice for scanner work.

Residual gap: `L-FINITE` relies on review that the final recognizer keeps no
counters. Revisit trigger: if the ADR-001 crate split extracts a pure binder
recognizer shared by scanner and accessor, a Verus proof of `INV-AGREE` over
unbounded sequences becomes cheap and would replace the bound argument. Because
no Verus milestone exists, no `make verus` acceptance criterion is added.

### Suggested BDD features

Extend `tests/features/relation_primary_key.feature` (bound in
`tests/relation_primary_key_bdd.rs`) with three scenarios and one new step,
`Then the parse reports the diagnostic {code:string} at {text:string}`. The
step asserts that exactly one error carries the code and that its span slices
the source to `text`; to support it, `record_inspection` additionally records
each error's span beside its message. The existing step
`the relation names are {expected:string}` compares the comma-joined names, so
`""` asserts that no relation was exposed. Recovery onto a following line is
covered by the `rstest` table rather than BDD, because the feature's string
parameter carries a single line.

```gherkin
  Scenario: Capitalized binder is rejected at the binder
    Given the DDlog source "input relation R(x: u32) primary key (Foo)"
    When the relations are listed
    Then the relation names are ""
    And the parse reports the diagnostic "D-REL-007" at "Foo"

  Scenario: A capitalized later binder is reported at that binder
    Given the DDlog source "input relation R(a: u32, b: u32) primary key (a, Bad)"
    When the relations are listed
    Then the parse reports the diagnostic "D-REL-007" at "Bad"

  Scenario: Underscore-led and camel-case binders are accepted
    Given the DDlog source "input relation R(x: u32, y: u32) primary key (_x, rowId)"
    When the first relation's primary key is inspected
    Then the primary-key binders are "_x, rowId"
    And the parse has no parser-level errors
```

Links: all three scenarios trace to `REQ-BINDER-LC` and `EP-M1`; the first two
also trace to `REQ-BINDER-SPAN`.

## Plan of work

### `EP-M1` binder case enforcement in scanner and accessor

Assigned: `REQ-BINDER-LC`, `REQ-BINDER-SPAN`, `REQ-AGREE`, `GAP-BINDER-CASE`;
decisions `D1` to `D5`.

Red. Add `src/parser/tests/relation_binder_case.rs` (declared in
`src/parser/tests/mod.rs`) with the `rstest` table and the inline snapshot from
`Methods per obligation`, the hand-built CST accessor test, and the three BDD
scenarios with the new step. Run them and confirm they fail because `(Foo)` and
`(a, Bad)` produce no diagnostic and the accessor returns `Ok`.

Green.

1. Create `src/parser/ident_case.rs` with a `//!` module comment stating its
   scope (identifier case classes from specification section 2.2; pure, no I/O;
   the single owner of case-class predicates in the parser) and:

   ```rust
   /// Return whether `text` is an `LcName`: `_` or an ASCII lowercase letter,
   /// followed by ASCII letters, digits, or `_`.
   pub(crate) fn is_lc_name(text: &str) -> bool
   ```

   with a rustdoc example and unit tests. Declare it with `mod ident_case;` in
   `src/parser/mod.rs`.
2. In `src/parser/span_scanners/relations/suffix.rs`, replace
   `is_binder_identifier_list`'s `bool` with a private verdict:

   ```rust
   enum BinderListVerdict {
       WellFormed,
       Malformed,
       NonLcBinder(Span),
   }
   ```

   Rename the function
   `classify_binder_list(tokens, src, list_start,
   list_end) -> BinderListVerdict`.
   It keeps the existing two-state loop, additionally records the span of the
   first `T_IDENT` whose text fails `is_lc_name`, and returns `Malformed` for
   any structural failure before considering case (`L-STRUCT-FIRST`).
   `parse_primary_key_clause` maps `Malformed` (or `!has_column`) to the
   existing error at `binder_list_span` and `NonLcBinder(span)` to
   `custom_error(&span, D_REL_007)`. Keep the function's rustdoc current.
3. In `src/parser/ast/parse_utils/relation.rs`, change the `binder` parser in
   `primary_key_clause` to `try_map` each `(name, span)`, returning
   `Simple::custom(span, D_REL_007)` when `!is_lc_name(&name)`, and update its
   rustdoc example set with a rejected uppercase case.
4. Add a postcondition sentence to the rustdoc of `PrimaryKeyBinder::name` and
   `PrimaryKey::binders`: every binder name is an `LcName`.
5. Update specification section 5.5 (remove the "does not enforce" sentence;
   state the anchor rule) and register item 13 (remove the known limitation;
   add the anchor rule and the new test files to the coverage list), per the
   register's maintenance rule that behaviour changes update the register in
   the same change.

Refactor. Tidy names and rustdoc in the touched functions; keep `suffix.rs`
under 400 lines. Run the focused tests, then the full gates.

### `EP-M2` bounded enumeration, property tests, and snapshot review

Assigned: `INV-VERDICT`, `INV-AGREE`, `INV-CASE`, `L-STRUCT-FIRST`, `L-FINITE`.

Add `src/parser/span_scanners/relations/binder_list_tests.rs` (declared as
`#[cfg(test)] mod binder_list_tests;` in `relations/mod.rs`) containing the
oracle `V`, the token-sequence builder, the enumeration
`binder_list_enumeration_matches_oracle`, the class-coverage assertions, and a
comment recording `L-FINITE`'s state set. The test calls `classify_binder_list`
(visible to the sibling test module through `pub(super)`) and
`primary_key_clause` (`pub(crate)`). Add
`src/parser/tests/relation_binder_case_props.rs` with the public-boundary
property and its fixed-seed coverage test, following the developers' guide
convention that property helpers return `Result<_, TestCaseError>`.

Because `EP-M1` is already green, the red evidence for this milestone is the
seeded fault: apply it, observe the enumeration and property fail with a
counterexample, revert, and record the transcript in `Artefacts and notes`.

### `EP-M3` import-alias decision pinned and specification corrected

Assigned: `REQ-ALIAS-DECIDED`, `GAP-ALIAS-SPEC`; decisions `D6` and `D8`.

Add the `INV-ALIAS` `rstest` table to `src/parser/tests/imports.rs`. Run the
seeded fault to show the table can fail, then revert. Update specification
section 5.2: change the production to
`Import ::= 'import' ScopedPath ('as' Ident)? ';'`, replace the "uses the
documented `LcName` category" paragraph with the upstream rule and the
decision, and note that upstream also accepts a `::`-separated alias that
ddlint does not. Add an import-alias entry to the conformance register (new
item 23, status `implemented` for the case decision, with a known limitation
for scoped aliases) and remove the import-alias sentence from item 13.

### `EP-M4` consolidate duplicated lowercase-identifier helpers

Assigned: `INV-REFAC`, `L-LEX`; decision `D5`. Separate refactor commit.

Replace `is_lowercase_ident` in `src/parser/span_scanners/transformers.rs`,
`is_lowercase_ident` in `src/parser/pattern/mod.rs`, and
`is_lowercase_identifier_segment` in `src/parser/expression/qualified.rs` with
`crate::parser::ident_case::is_lc_name`. Before replacing each pattern and
transformer site, confirm the argument is the text of a single `T_IDENT` token
(`L-LEX` applies); the qualified-call site already uses the full regex. If
`is_uppercase_ident` in `pattern/mod.rs` has a caller, move it to
`ident_case.rs` as `is_uc_name` with the section 2.2 `UcName` rule only if its
caller also passes a single token; otherwise leave it. All existing tests must
pass unchanged.

### `EP-M5` documentation, register, roadmap, and delivery closure

Assigned: `REQ-DOCS`; decision `D9`.

- `docs/users-guide.md`: in "Relation declarations", state that binders must
  start with a lowercase letter or `_`, with `primary key (Foo)` as a rejected
  example and the span rule; in "Relation diagnostics", widen the `D-REL-007`
  cause to "Stray or malformed `primary key` clause, including a binder that
  does not start with a lowercase letter or underscore". Near the `as` note,
  state that import aliases are not case-restricted.
- `docs/parser-implementation-notes.md`: describe the verdict type, the
  anchor precedence, and the shared predicate.
- `docs/developers-guide.md`: under "Parser module ownership" or "Boundary
  rules", record `src/parser/ident_case.rs` as the single owner of identifier
  case-class predicates and its reuse policy (crate-private, pure, call it
  instead of adding local first-character checks); under "Parser test helpers",
  record the bounded-enumeration convention (direct token construction,
  independent oracle, class-coverage assertions, seeded fault).
- `docs/repository-layout.md`: add `src/parser/ident_case.rs` and the new test
  files to the parser ownership map.
- `docs/adrs/adr-0002-relation-role-kind-modelling.md`: append a dated
  amendment stating that `INV-AGREE` now includes the binder case class and that
  `D-REL-007` anchors at the first non-`LcName` binder.
- `docs/ddlint-design.md`: one sentence in section 2 that binders are
  case-checked by scanner and accessor, referencing the ADR-0002 amendment.
- `docs/parser-conformance-register.md`: item 12 gains a known limitation
  recording the upstream transformer divergence (`D9`); item 13 notes the
  upstream bracket-body primary-key acceptance as an observation.
- `docs/roadmap.md`: check item 2.6.9 with a short "Done:" summary, and, if
  the user approved it with this plan, add an unchecked item deciding
  transformer alignment with upstream.
- `CHANGELOG.md`: under "Unreleased", "Changed": primary-key binders must be
  `LcName`; `primary key (Foo)` reports `D-REL-007` at the binder.
- `docs/contents.md`: the ExecPlan index entry was added with the draft plan;
  check that its title still matches.

Search for stale claims with
`grep -rn "does not enforce\|LcName\|2\.6\.9" docs --include=*.md | grep -v docs/execplans`
and fix every live hit. Mark this plan `COMPLETE` after the gates pass and the
outcomes section is written.

## Milestones and plateaus

- `EP-M1`: binders are case-checked in scanner and accessor.
  - Requirements and gaps: `REQ-BINDER-LC`, `REQ-BINDER-SPAN`, `REQ-AGREE`,
    `GAP-BINDER-CASE`.
  - Acceptance evidence: the `relation_binder_case` tests and BDD scenarios
    fail before and pass after; `make check-fmt`, `make lint`, `make test`
    pass.
  - Conformance check:
    - spec 5.5 satisfied for binders;
    - ADR-0002 invariants still hold (existing tests green);
    - no new public interface, code, message, or dependency;
    - no trust-boundary or persisted-format change;
    - register item 13 and spec 5.5 updated in the same commit.
  - Recovery: revert the milestone commit; no data or format migration.
  - Remaining gaps: exhaustive evidence (`EP-M2`), aliases (`EP-M3`).
  - Compatibility decision: none (pre-release, `AGENTS.md` and project
    memory).
- `EP-M2`: verification evidence complete for binders.
  - Requirements: `REQ-AGREE`, `INV-VERDICT`, `INV-CASE`.
  - Acceptance evidence: enumeration and property pass; seeded-fault
    transcripts recorded; gates pass.
  - Conformance check: tests exercise production code; no tooling added.
  - Recovery: test-only commit, safe to revert.
  - Remaining gaps: aliases, refactor, docs.
  - Compatibility decision: none.
- `EP-M3`: alias decision pinned.
  - Requirements and gaps: `REQ-ALIAS-DECIDED`, `GAP-ALIAS-SPEC`.
  - Acceptance evidence: alias table passes; seeded fault fails it; spec 5.2
    and register updated; gates pass.
  - Conformance check: behaviour unchanged; spec aligned with upstream.
  - Recovery: revert the commit.
  - Remaining gaps: refactor, docs.
  - Compatibility decision: none.
- `EP-M4`: one case predicate owner.
  - Requirements: `INV-REFAC`.
  - Acceptance evidence: no test changes needed; gates pass; `grep -rn
    "fn is_lowercase_ident\|fn is_lowercase_identifier_segment" src` returns
    nothing (or only sites recorded as unconsolidated).
  - Recovery: revert the refactor commit independently of `EP-M1` to `EP-M3`.
  - Compatibility decision: none.
- `EP-M5`: documentation and roadmap closed.
  - Requirements: `REQ-DOCS`.
  - Acceptance evidence: `make markdownlint` and `make nixie` pass; stale-claim
    search clean; roadmap 2.6.9 checked.
  - Compatibility decision: none.

## Concrete steps

Working directory: the repository root. Use `set -o pipefail` for every `tee`
pipeline. Logs go to
`/tmp/<action>-ddlint-2-6-9-enforce-lc-name-case-restriction-on-primary-key-binders.out`.

`EP-M1`:

```plaintext
Red:   cargo test --lib relation_binder_case
       -> FAILED: (Foo) expected one D-REL-007, got []
Red:   cargo test --test relation_primary_key_bdd
       -> scenario "Capitalized binder is rejected at the binder" fails
Green: the same two commands -> all pass
```

`EP-M2`:

```plaintext
Green:  cargo test --lib binder_list_enumeration
        -> ok, with per-class counts printed under --nocapture
Fault:  temporarily accept I("Cd") in classify_binder_list
        -> FAILED with a counterexample naming the (Cd) sequence; revert
Green:  cargo test --lib relation_binder_case_props -> ok
```

`EP-M3`:

```plaintext
Green: cargo test --lib imports -> ok
Fault: temporarily reject uppercase aliases -> Baz and BAZ rows fail; revert
```

Gate sequence after each milestone, sequentially:

```sh
set -o pipefail
make fmt
make check-fmt 2>&1 | tee /tmp/check-fmt-ddlint-$(git branch --show-current).out
make lint 2>&1 | tee /tmp/lint-ddlint-$(git branch --show-current).out
make test 2>&1 | tee /tmp/test-ddlint-$(git branch --show-current).out
make markdownlint 2>&1 | tee /tmp/markdownlint-ddlint-$(git branch --show-current).out
make nixie 2>&1 | tee /tmp/nixie-ddlint-$(git branch --show-current).out
```

## Validation and acceptance

- `ddlint::parse("input relation R(x: u32) primary key (Foo)")` returns one
  error whose message is
  `D-REL-007: unexpected or malformed primary key clause` and whose span slices
  the source to `Foo`, and no relations.
- `ddlint::parse("input relation R(x: u32, y: u32) primary key (_x, rowId)")`
  returns no errors and `binder_names()` is `["_x", "rowId"]`.
- `ddlint::parse("input relation R(x: u32) primary key (Foo,)")` reports one
  `D-REL-007` spanning the `(`.
- `ddlint::parse("import foo::bar as Baz;")` returns no errors and
  `alias()` is `Some("Baz")`.

Quality criteria:

- Tests: `make test` passes, including the new tests named above.
- Verification: `INV-VERDICT`, `INV-AGREE`, and `L-STRUCT-FIRST` discharged
  within the stated bound and extended by `L-FINITE`; `INV-CASE`, `INV-ALIAS`,
  and `INV-REFAC` discharged by named and property tests; each seeded fault
  observed failing and reverted.
- Lint and format: `make check-fmt`, `make lint`, `make markdownlint`, and
  `make nixie` pass.
- Performance: the enumeration completes in under 10 seconds within
  `make test`.

## Idempotence and recovery

All steps are re-runnable. Seeded faults are applied and reverted within one
working-tree session and never committed; confirm with `git diff` before each
commit. If proptest writes a file under `proptest-regressions/`, keep it only
if it records a genuine, fixed failure, and mention it in
`Surprises & discoveries`. Each milestone is one or more commits that can be
reverted independently; `EP-M4` in particular is isolated so it can be dropped
without affecting behaviour.

## Artefacts and notes

Reconnaissance evidence, revision `7adbd8bc`:

```plaintext
suffix.rs:104  if !has_column || !is_binder_identifier_list(tokens, list_start, *cursor) {
suffix.rs:215  (true, SyntaxKind::T_IDENT) => { ... }          // no text check
relation.rs:64 let binder = just(SyntaxKind::T_IDENT)        // no text check
imports.rs:36  just(SyntaxKind::K_AS).padded_by(ws).ignore_then(ident_parser)
```

Upstream DDlog `Parse.hs` (fetched 2026-11-23):

```plaintext
key_expr = withPos $ KeyExpr nopos <$> (parens varIdent) <*> expr
varIdent = lcIdentifier <?> "variable name"
imprt = Import nopos <$ reserved "import" <*> modname
          <*> (option (ModuleName []) $ reserved "as" *> modname)
modname = ModuleName <$> modIdent `sepBy1` reservedOp "::"
modIdent = identifier <?> "module name"
transIdent = ucScopedIdentifier <?> "transformer name"
```

Implementation transcripts are added here as milestones complete.

## Interfaces and dependencies

No new dependencies. At the end of `EP-M1`:

- `src/parser/ident_case.rs`: `pub(crate) fn is_lc_name(text: &str) -> bool`.
  Owner: the parser. Reuse policy: every parser site that classifies an
  identifier's case calls this module; local first-character checks are not
  added. Pure function, no allocation, no I/O.
- `src/parser/span_scanners/relations/suffix.rs`: private
  `enum BinderListVerdict { WellFormed, Malformed, NonLcBinder(Span) }` and
  `pub(super) fn classify_binder_list(tokens: &[(SyntaxKind, Span)], src: &str,
  list_start: usize, list_end: usize) -> BinderListVerdict`
  (`pub(super)` only so the sibling test module can call it).
- `src/parser/ast/parse_utils/relation.rs`: `primary_key_clause` keeps its
  signature; its binder parser rejects non-`LcName` names with
  `Simple::custom(span, D_REL_007)`.
- Public API: unchanged signatures; rustdoc postconditions added to
  `PrimaryKey::binders` and `PrimaryKeyBinder::name`.

## Revision note

- 2026-11-23: initial draft from Wyvern reconnaissance and upstream grammar
  lookup. Awaiting community-of-experts review.
