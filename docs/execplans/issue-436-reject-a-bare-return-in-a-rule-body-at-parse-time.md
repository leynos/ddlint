# Reject a bare `return` in a rule body at parse time

This ExecPlan (execution plan) is a living document. The sections `Constraints`,
`Tolerances`, `Risks`, `Progress`, `Surprises & discoveries`, `Decision log`,
`Outcomes & retrospective`, `Conformance basis`, and `Verification plan` must
be kept up to date as work proceeds.

Status: IN PROGRESS

Branch: `issue-436-reject-a-bare-return-in-a-rule-body-at-parse-time`

Roadmap item: `2.2.4` in `docs/roadmap.md`.

## Purpose / big picture

`return` supplies a value to a function or closure body. A Differential Datalog
(DDlog) rule body has no return value, so
`docs/differential-datalog-parser-syntax-spec-updated.md` section 5.10 permits
`return` only inside a function or closure body. Before this work, the parser
accepted `Output(head_x) :- Source(return head_x).` and analysed it as though
the keyword were legal.

After this work, the parser reports the keyword itself:

```text
example.dl:21..27: error: 'return' is not permitted in a rule body; use it only in a function or closure body
```

while both permitted positions still parse without diagnostics:

```ddlog
function f(): u32 { return 1; }
Output(head_x) :- Source(|ignored| return head_x).
```

The command-line entrypoint gains an interim parse-only path so the rejection
and the two permitted positions are observable end to end, rather than only
through the library API.

Behaviour deliberately preserved: the public `parse_expression` API, rule-head
parsing, and top-level `for` lowering all keep accepting `return`, because
those positions are not rule bodies. The rejection is a parse-time diagnostic,
not a semantic-analysis rule.

## Constraints

- Public `parse_expression` keeps its signature and permissive behaviour; rule
  heads and top-level `for` parsing stay unchanged (register item 7 records the
  remaining gaps).
- The diagnostic must name the `return` keyword's half-open byte span, not the
  whole rule literal.
- No backwards-compatibility diagnostics or migration guidance: `ddlint` is
  pre-release, as recorded in `AGENTS.md` and the project memory.
- New source files stay under 400 lines and every module, including test
  modules and fixture crate roots, starts with a `//!` comment.
- No new dependency is added. The command-line crate continues to depend only
  on `ddlint` for parser behaviour.
- Historical completed ExecPlans, including
  `docs/execplans/2-6-8-decide-brace-group-extension-policy.md`, are not
  retroactively edited.
- Gate sequence follows `AGENTS.md`: `make fmt`, `make check-fmt`, `make lint`,
  `make test`, `make markdownlint`, and `make nixie`, run sequentially for this
  candidate.

## Tolerances (exception triggers)

- Files: more than 25 non-ExecPlan files changed requires escalation.
- Code: more than 700 net lines of Rust requires escalation.
- Documentation: more than 300 net lines outside ExecPlan files requires
  escalation.
- Interface: any public API change beyond `parse_error_message`,
  `parse_error_span`, their re-export, and the crate-private rule-body entry
  point requires escalation.
- Dependencies: any new dependency requires escalation.
- Iterations: more than four repair attempts on one gate failure requires
  escalation.
- Ambiguity: if a permitted position and the rejection cannot both hold under
  one design, stop and present options.

Budget basis: the survey found the Pratt scoping hooks (`in_rule_body`,
`parse_rule_body_expression`) already present from the merged brace-block work,
so this change mostly touches classification, the diagnostics renderer, the
CLI, and tests. Comparable completed plans in `docs/execplans/` budget
500-1,500 net code lines for parser features; this one is smaller because the
Pratt parser already carries the scoping state.

## Risks

- Risk: the rule-body literal splitter does not track `|`, so a closure with a
  top-level comma could split into two literals and hide the permitted
  position. Severity: medium. Likelihood: medium. Mitigation: test only
  single-parameter closures and nested single-parameter closures, and record
  the splitter limitation in the tests.
- Risk: function bodies are opaque scanned regions, so "`return` in a function
  body still parses" is established only for whole-program parsing, not for
  expression-level validation. Severity: low. Likelihood: high. Mitigation:
  state the limitation in register item 7 and test the full-program form.
- Risk: moving the diagnostic from `diagnostic_message` to a public
  `parse_error_message` could change observer output. Severity: medium.
  Likelihood: low. Mitigation: keep the rendering code byte-identical and
  retain the two observer unit tests.
- Risk: compile admission is saturated by other agents, making gate runs slow.
  Severity: low. Likelihood: high. Mitigation: allow long timeouts and do not
  start parallel builds.

## Progress

- [x] (2026-11-23 16:00Z) Reconnaissance: located the existing scoping hooks,
  the span scanner, classification, CLI placeholder, and documentation.
- [x] (2026-11-23 17:10Z) `EP-M1`: classification uses the rule-body entry
  point; keyword-span rejection moved to `K_RETURN` dispatch; closure scope
  restores permission.
- [x] (2026-11-23 17:40Z) `EP-M2`: parser test module, CLI parse-only path,
  CLI process tests, and unit tests written.
- [x] (2026-11-23 20:20Z) `EP-M2` verification: 15 focused parser tests, 4 CLI
  process tests, `make typecheck`, and the full `make test` suite pass,
  including both new doctests.
- [x] (2026-11-23 21:10Z) Code committed as `6d366ea7`.
- [x] (2026-11-24 00:30Z) `make check-fmt`, `make typecheck`, `make lint`
  (Clippy, Rustdoc, Whitaker, spelling), `make markdownlint`, and `make nixie`
  pass; the CLI reads through a `cap_std` file capability after Whitaker's
  `no_std_fs_operations` finding (commit `5a9b4d12`).
- [x] (2026-11-24 04:40Z) CodeRabbit review completed with three findings; all
  three were verified valid and fixed: register wording excludes nested closure
  bodies, `File::open_ambient` restores symlink resolution with a new process
  test, and classification spans now use file coordinates.
- [ ] `EP-M3`: commit the review fixes, push, and open the draft pull request.

## Surprises & discoveries

- Observation: the workspace's Whitaker suite enforces a capability-based
  filesystem policy that denies `std::fs` outright, including in binary crates.
  Evidence: `make lint` reported `no_std_fs_operations` for `cli/src/main.rs`
  and `cli/tests/return_placement_cli.rs`. Impact: the plan's instruction to
  read with `std::fs` cannot stand; the CLI uses `cap_std`.
- Observation: a directory capability refuses symlinks that leave the
  directory. Evidence: CodeRabbit review finding on `Dir::open_ambient_dir` plus
  `read_to_string`. Impact: opening the named file with
  `cap_std::fs::File::open_ambient` preserves ordinary CLI symlink semantics.
- Observation: `Rule::body_terms()` reported standalone-literal parse errors in
  literal-relative coordinates while the assignment branch used file
  coordinates. Evidence: CodeRabbit review finding and the existing
  `body_terms_shift_assignment_*` tests. Impact: the standalone branch now
  shifts by the literal start plus the leading whitespace `trim()` removed.
- Observation: the merged brace-block work (PR 408) had already introduced
  `RETURN_IN_RULE_BODY_ERROR`, `parse_rule_body_expression`, and the
  `in_rule_body` Pratt flag, and applied them in `validate_expression`.
  Evidence: `git log -S'RETURN_IN_RULE_BODY_ERROR'`. Impact: the remaining work
  is classification consistency, tests, the CLI path, and documentation, not a
  from-scratch parser change.
- Observation: `Rule::body_terms()` still parsed literals with the permissive
  `parse_expression`, so classification could accept a construct the base parse
  rejected. Impact: the plan's classification task is real and required.

## Decision log

- D1. Keep the scoped-state name `in_rule_body` and invert the new field to
  `return_permitted`, rather than introducing a parallel flag. Rationale: the
  permission polarity matches the plan's wording while the existing closure
  helper can delegate to one `with_return_permission` routine. Date/Author:
  2026-11-23, implementer.
- D2. Keep the diagnostic at the `K_RETURN` dispatch in
  `src/parser/expression/prefix.rs` and remove the duplicate check from
  `parse_return_expression`. Rationale: the plan asks for the check at dispatch
  so the keyword span is available directly, and one check avoids duplicate
  diagnostics. Date/Author: 2026-11-23, implementer.
- D3. Add `parse_error_span` beside `parse_error_message` so the CLI crate needs
  no `chumsky` dependency to print the located diagnostic. Rationale: the plan
  only names `parse_error_message`, but the CLI must print the span and should
  not take a parser-internal dependency. Date/Author: 2026-11-23, implementer.
- D4. Adopt the plan's quoting style for the message
  (`'return' is not permitted ...`). Rationale: it names the keyword and
  matches the existing single-quote style for tokens such as `'->'`.
  Date/Author: 2026-11-23, implementer.
- D5. Use a local `ensure(holds, message)` helper in the CLI process tests
  instead of `assert!`. Rationale: the workspace denies `expect_used`,
  `unwrap_used`, and `panic_in_result_fn`, so a fallible test must report
  through its `Result`; the helper mirrors `tests/block_expressions.rs` and
  `tests/build_standard_support/injected.rs`. Date/Author: 2026-11-23,
  implementer.
- D6. Read the CLI's input files with `cap_std::fs::File::open_ambient`, not
  `Dir::open_ambient_dir` plus `read_to_string` and not `std::fs`. Rationale:
  the Whitaker `no_std_fs_operations` lint denies `std::fs`, while an
  operator-supplied symlink may point outside its own directory and a directory
  capability refuses that target. Opening the named file is the narrowest
  ambient operation that preserves ordinary CLI semantics. Date/Author:
  2026-11-24, implementer.
- D7. Fix the classification offset discrepancy that the plan originally said
  to preserve. Rationale: CodeRabbit showed the standalone branch reporting
  literal-relative spans while the assignment branch reports file offsets; the
  new `return` diagnostic makes that inconsistency user-visible, and one
  coordinate system is now asserted by `body_terms_reject_a_bare_return`.
  Date/Author: 2026-11-24, implementer.

## Outcomes & retrospective

To be completed after the gates pass and the pull request is opened.

## Context and orientation

The parser is split into span scanners and a handwritten Pratt expression
parser. `src/parser/span_scanners/rules.rs` splits each rule body into
comma-separated literals and validates each one through
`src/parser/expression_span.rs::validate_expression`, which now calls the
crate-private `parse_rule_body_expression` in
`src/parser/expression/pratt/mod.rs`. `Rule::body_terms()` in
`src/parser/ast/rule/mod.rs` reaches `parse_rule_body_term` in
`src/parser/ast/rule/classification.rs`, which previously parsed literals with
the public `parse_expression`.

The Pratt parser keeps scoped state: `struct_literals`, `semicolon_terminates`,
and `return_permitted`. `parse_closure_literal` in
`src/parser/expression/data_structures.rs` wraps its body in
`with_closure_body_scope`.

The command-line crate is `cli/`, whose binary target is `ddlint`. It currently
prints a placeholder and exits `1`. The library re-exports `parse`, `Span`,
`Parsed`, and the diagnostic types from `src/lib.rs`.

Frameworks and gates: `rstest` for parameterized tests, `tempfile` for CLI file
fixtures, `cargo test` (not nextest), and the Makefile gates listed in
`Constraints`.

## Conformance basis

- `docs/differential-datalog-parser-syntax-spec-updated.md` section 5.10
  (`REQ-RETURN-PLACEMENT`): `return` is valid only in function or closure
  bodies and misuse must be reported with a clear diagnostic.
- `docs/parser-conformance-register.md` item 7 (`GAP-RETURN-PLACEMENT`): the
  base parse previously accepted `return` directly in a rule body.
- `docs/roadmap.md` item 2.2.4 (`EP-ROADMAP`): imperative statements parse.

Trace links:
`REQ-RETURN-PLACEMENT -> GAP-RETURN-PLACEMENT -> EP-M1 ->
src/parser/tests/rules/return_placement.rs`,
and `REQ-RETURN-PLACEMENT -> EP-M2 -> cli/tests/return_placement_cli.rs`.

## Verification plan

### Abstract model

A parse input is a token sequence whose rule-body literals are validated by
`parse_rule_body_expression`. The parser carries a Boolean permission `p`,
initially `true` for `parse_expression` and `false` for
`parse_rule_body_expression`. Encountering `return` with `p = false` emits one
diagnostic at the keyword span and continues parsing the value. A closure body
parses with `p = true` and restores the previous value afterwards.

### Invariants and lemmas

- `INV-KEYWORD-SPAN`: every rejection diagnostic span equals the half-open byte
  range of the `return` keyword.
- `INV-SINGLE-DIAGNOSTIC`: one misplaced keyword yields exactly one rejection
  diagnostic.
- `INV-CLOSURE-RESTORE`: leaving a closure body restores the enclosing
  permission, so a sibling argument is checked.
- `L-ENTRY-POINT`: both the span scanner and `Rule::body_terms()` reach the
  rule-body entry point, so they agree on acceptance.

### Axioms (trusted, not verified here)

- `AXIOM-CHUMSKY`: `Simple::span()` returns the range supplied when the error
  was pushed.
- `AXIOM-SPLITTER`: `rule_body_literal_spans` splits at top-level commas and
  tracks only `()`, `{}`, and `[]`, not `|`.

### Methods per obligation

- `INV-KEYWORD-SPAN` and `INV-SINGLE-DIAGNOSTIC`: parameterized parser tests in
  `src/parser/tests/rules/return_placement.rs` using `assert_parse_error`,
  which requires exactly one error and an exact span; the multibyte-prefix case
  proves the offset is a byte offset. Non-vacuity: each case contains the
  keyword and would fail with a literal-span diagnostic.
- `INV-CLOSURE-RESTORE`: the sibling-argument case
  `Source(|ignored| ignored, return x)` plus the nested-closure acceptance
  case. A wrong restore would either accept the sibling keyword or reject the
  nested closure.
- `L-ENTRY-POINT`: `body_terms_reject_a_bare_return` and
  `body_terms_accept_a_return_in_a_closure_body`.
- End-to-end: `cli/tests/return_placement_cli.rs` asserts exit statuses and
  stderr text through the binary.

No non-trivial formal obligation is introduced; the change is a scoped parser
check and a renderer move, so parameterized tests and process tests are
proportionate.

## Plan of work

`EP-M1` makes the parser and classification agree: switch
`parse_rule_body_term` and its assignment path to `parse_rule_body_expression`,
move the `return` check to the `K_RETURN` dispatch with
`with_return_permission` scope restoration, and update the message constant.

`EP-M2` adds `src/parser/tests/rules/return_placement.rs`, the CLI parse-only
path in `cli/src/main.rs`, `cli/tests/return_placement_cli.rs`, and unit tests
for writer failures.

`EP-M3` updates `docs/users-guide.md`, `docs/parser-conformance-register.md`,
`docs/parser-implementation-notes.md`, the specification's section 5.10 and
revision list, and `docs/roadmap.md`, then runs the full gate sequence.

## Milestones and plateaus

### `EP-M1` Parser and classification agreement

- Requirements and gaps: `REQ-RETURN-PLACEMENT`, `GAP-RETURN-PLACEMENT`.
- Acceptance evidence: `cargo test --lib parser::tests::rules::return_placement`
  passes; the diagnostic span equals the keyword span.
- Conformance check: public `parse_expression` unchanged; no dependency change;
  register gaps recorded.
- Recovery: revert the commit; no persisted state.
- Remaining gaps: rule heads, top-level `for`, opaque regions, `break` and
  `continue` placement.
- Compatibility: none.

### `EP-M2` Tests and CLI path

- Acceptance evidence: parser tests, CLI process tests, and writer-failure unit
  tests pass.
- Compatibility: none.

### `EP-M3` Documentation and delivery

- Acceptance evidence: `make fmt`, `make check-fmt`, `make lint`, `make test`,
  `make markdownlint`, and `make nixie` pass; draft pull request opened.
- Compatibility: none.

## Concrete steps

```text
Working directory: repository root
Focused red/green: cargo test --lib parser::tests::rules::return_placement
CLI: cargo test --test return_placement_cli
Gates: make fmt && make check-fmt && make lint && make test && make markdownlint && make nixie
```

## Validation and acceptance

A rule body containing a bare `return` is rejected at the keyword span, while
`function f(): u32 { return 1; }` and
`Output(head_x) :- Source(|ignored| return head_x).` parse with no diagnostics.
`ddlint <file>` reports `<path>:<start>..<end>: error: <message>` and exits
zero only when every file parses cleanly.

## Idempotence and recovery

Every step is re-runnable. The parser change has no persisted state; reverting
the commit restores the previous permissive parse. CLI tests use temporary
directories created and removed by `tempfile`.

## Artefacts and notes

Evidence is recorded in the pull request and in the gate logs under `/tmp`.

## Interfaces and dependencies

The new public surface is
`ddlint::parse_error_message(&Simple<SyntaxKind>) -> String` and
`ddlint::parse_error_span(&Simple<SyntaxKind>) -> Span`, re-exported from
`src/lib.rs` beside `parse`. The crate-private surface is
`crate::parser::expression::parse_rule_body_expression`. No dependency is added.

## Revision note

- 2026-11-23: Initial version, recording the reconnaissance finding that part
  of the Pratt scoping work already exists on `main` and that the remaining
  work is classification consistency, tests, the CLI path, and documentation.
