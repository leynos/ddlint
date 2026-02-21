# Implement `RuleCtx` with source, config, and AST context

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`, and
`Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: COMPLETE

PLANS.md is not present in this repository.

## Purpose / big picture

Roadmap item `3.1.2` introduces a real rule execution context so CST rules can
read source text, inspect per-rule configuration, and access typed AST data
without global state. After this milestone, rule implementations can rely on a
stable `RuleCtx` API that is ready for upcoming `CstRuleStore` and runner work.

Observable success is:

- `crate::linter::RuleCtx` exposes source text, configuration, and AST context.
- unit tests verify `RuleCtx` constructors and accessors.
- behavioural tests verify rules can consume `RuleCtx` during CST dispatch.
- `docs/ddlint-design.md` records the chosen `RuleCtx` API and rationale.
- `docs/roadmap.md` marks item `3.1.2` done after all gates pass.

## Constraints

- Treat `docs/ddlint-design.md` sections `1.2` and `3.1` as the normative
  design source for this milestone.
- Keep scope limited to roadmap item `3.1.2`.
- Do not implement `CstRuleStore` (`3.1.3`) or the parallel runner (`3.1.4`).
- Do not add external dependencies.
- Keep existing parser behaviour unchanged.
- Preserve existing `Rule` and `CstRule` trait intent from `3.1.1`.
- Every new Rust module must start with a `//!` module comment.
- Keep files below 400 lines by splitting modules when needed.
- Validate with unit and behavioural tests.
- Record design decisions in `docs/ddlint-design.md`.
- Use Make targets and run gates with `set -o pipefail` and `tee`.

## Tolerances (exception triggers)

- Scope: if implementation exceeds 10 files changed or 360 net new lines, stop
  and escalate.
- Interface: if `Rule` or `CstRule` signatures must change in a non-additive
  way, stop and escalate.
- Dependencies: if a new crate is required, stop and escalate.
- Ambiguity: if config representation cannot be chosen without blocking
  roadmap item `3.1.3`, stop and record options in `Decision Log`.
- Iterations: if tests still fail after three focused fix cycles, stop and
  escalate with failing test names.

## Risks

- Risk: over-designing configuration types in `RuleCtx` can create migration
  pain once full `ddlint.toml` support lands. Severity: medium Likelihood:
  medium Mitigation: implement a small typed value enum and map, with additive
  extension points.

- Risk: context ownership choices can create awkward lifetimes for rule
  authors. Severity: high Likelihood: medium Mitigation: prefer owned or
  clone-cheap values in `RuleCtx` constructors, avoid exposing lifetime-heavy
  APIs.

- Risk: broad test breakage from replacing placeholder `RuleCtx::default()`.
  Severity: medium Likelihood: high Mitigation: add a compatibility constructor
  or update existing tests in one atomic pass with clear helper fixtures.

- Risk: documentation drift between roadmap wording and implementation detail.
  Severity: medium Likelihood: medium Mitigation: update design doc and roadmap
  in the same change set, then run full quality gates.

## Progress

- [x] (2026-02-20 00:20Z) Reviewed roadmap item `3.1.2`, current linter trait
  code, parser context surfaces, and referenced docs.
- [x] (2026-02-20 00:35Z) Drafted this ExecPlan.
- [x] (2026-02-21 00:30Z) Implemented concrete `RuleCtx` with typed
  `RuleConfigValue` and `RuleConfig`.
- [x] (2026-02-21 00:40Z) Updated unit tests for context constructors and
  accessors in `src/linter/rule.rs`.
- [x] (2026-02-21 00:50Z) Updated behavioural CST-dispatch tests to prove rule
  access to source/config/AST context.
- [x] (2026-02-21 01:00Z) Updated `docs/ddlint-design.md` with final `RuleCtx`
  contract.
- [x] (2026-02-21 01:20Z) Ran all required quality gates and captured logs in
  `/tmp/ddlint-*.log`.
- [x] (2026-02-21 01:05Z) Marked roadmap item `3.1.2` done in
  `docs/roadmap.md`.

## Surprises & Discoveries

- Observation: `docs/roadmap.md` references
  `docs/ddlint-design-and-road-map.md`, but this repository currently provides
  `docs/ddlint-design.md`. Evidence:
  `rg --files docs | rg 'ddlint-design-and-road-map'` returns no match. Impact:
  this plan treats `docs/ddlint-design.md` as the canonical source.

- Observation: `RuleCtx` is currently a placeholder empty struct.
  Evidence: `src/linter/rule.rs` defines `RuleCtx { _reserved: () }`. Impact:
  this milestone needs real fields, constructors, and tests.

- Observation: parser output type `Parsed` does not store source text.
  Evidence: `src/parser/cst_builder/mod.rs` stores only green tree, root, and
  errors. Impact: `RuleCtx` constructors must accept source text from callers.

- Observation: project-memory MCP resources are unavailable in this runtime.
  Evidence: `list_mcp_resources` and `list_mcp_resource_templates` returned
  empty lists. Impact: no Qdrant note retrieval/storage could be performed this
  session.

- Observation: `clippy::similar-names` and `clippy::needless-borrow` failed in
  `tests/linter_rule_traits.rs` on the first lint pass. Evidence: `make lint`
  reported failures for temporary variable names and `&ctx` double-borrows.
  Impact: renamed local variables for clarity and removed needless borrows
  before rerunning quality gates.

## Decision Log

- Decision: map roadmap references to `docs/ddlint-design.md` for this
  milestone. Rationale: it is the only in-repo design doc containing sections
  `1.2` and `3.1` for linter engine architecture. Date/Author: 2026-02-20 /
  assistant

- Decision: keep this milestone atomic by implementing only context data shape
  and access APIs, while deferring dispatch/store concerns to roadmap items
  `3.1.3` and `3.1.4`. Rationale: preserves roadmap sequencing and limits blast
  radius. Date/Author: 2026-02-20 / assistant

- Decision: model rule configuration as a small typed enum
  (`RuleConfigValue`) backed by `BTreeMap<String, RuleConfigValue>` rather than
  introducing an external deserialization layer at this stage. Rationale: this
  keeps `3.1.2` additive and dependency-free while providing enough type
  information for rule implementations. Date/Author: 2026-02-21 / assistant

## Outcomes & Retrospective

Completed roadmap item `3.1.2` by implementing a concrete `RuleCtx` with source
text, typed rule configuration, and AST/CST access. Added typed configuration
values (`RuleConfigValue`) and map alias (`RuleConfig`), updated unit and
behavioural tests to validate context access, and documented the final contract
in `docs/ddlint-design.md`.

Roadmap update is complete: `docs/roadmap.md` now marks `3.1.2` done.

Validation completed successfully:

- `make markdownlint`
- `make fmt`
- `make nixie`
- `make check-fmt`
- `make lint`
- `make test`

## Context and orientation

Current linter surfaces:

- `src/linter/rule.rs` now contains `Rule`, `CstRule`, `LintDiagnostic`,
  concrete `RuleCtx`, `RuleConfig`, and `RuleConfigValue`.
- `src/linter/mod.rs` re-exports linter contracts.
- `tests/linter_rule_traits.rs` provides behavioural CST dispatch tests and now
  constructs `RuleCtx` instances from source + parsed AST + configuration.

Relevant parser and AST surfaces used by context:

- `src/parser/cst_builder/mod.rs` exposes `Parsed` and typed root access.
- `src/parser/ast/root.rs` exposes typed AST navigation from `Root`.
- `src/lib.rs` exports `Parsed` and parser entrypoints.

Relevant docs to update:

- `docs/ddlint-design.md` section `3.1` for final `RuleCtx` API.
- `docs/roadmap.md` checklist item `3.1.2` after gates are green.

## Interfaces and dependencies

Target public API after this milestone:

- `crate::linter::RuleCtx`
- `crate::linter::RuleConfig`
- `crate::linter::RuleConfigValue`

Planned shape (final names may vary, behaviour must match):

    pub type RuleConfig = BTreeMap<String, RuleConfigValue>;

    pub enum RuleConfigValue {
        Bool(bool),
        Integer(i64),
        String(String),
    }

    pub struct RuleCtx {
        source_text: Arc<str>,
        ast_root: crate::parser::ast::Root,
        config: RuleConfig,
    }

    impl RuleCtx {
        pub fn new(
            source_text: impl Into<Arc<str>>,
            ast_root: crate::parser::ast::Root,
            config: RuleConfig,
        ) -> Self;

        pub fn source_text(&self) -> &str;
        pub fn ast_root(&self) -> &crate::parser::ast::Root;
        pub fn cst_root(&self) -> &rowan::SyntaxNode<crate::DdlogLanguage>;
        pub fn config(&self) -> &RuleConfig;
        pub fn config_value(&self, key: &str) -> Option<&RuleConfigValue>;
    }

Dependency policy:

- Use only `std` collections and smart pointers (`BTreeMap`, `Arc`).
- No new crates for configuration encoding in this milestone.

## Plan of work

Stage A: lock behaviour with tests first.

Add failing tests that define expected `RuleCtx` behaviour:

- source text accessor returns exact file text.
- config accessors return typed values for known keys.
- AST accessor exposes typed root and CST root for rule logic.
- existing behavioural dispatch tests continue to work with concrete context.

Go/no-go: proceed only when tests fail for the intended reasons against the
placeholder implementation.

Stage B: implement `RuleCtx` and related types.

Refactor linter internals to keep modules clear and under size limits. If
needed, split context types into a dedicated `src/linter/context.rs` module and
re-export through `src/linter/mod.rs`. Implement constructors and accessor
methods with Rustdoc examples.

Go/no-go: proceed only when context API compiles and tests can import the new
surface.

Stage C: adapt behavioural tests and helper wiring.

Update `tests/linter_rule_traits.rs` (or a new behavioural file) so dispatch
creates a concrete `RuleCtx` from fixture source plus parsed AST root and
configuration. Add at least one rule asserting all three context channels are
available during `check_node` or `check_token`.

Go/no-go: proceed only when unit and behavioural tests pass locally.

Stage D: document decisions, run gates, and close roadmap item.

Update `docs/ddlint-design.md` section `3.1` with final `RuleCtx` fields,
constructors, and typed config contract. After quality gates pass, mark `3.1.2`
as done in `docs/roadmap.md`.

## Concrete steps

1. Add/adjust tests to encode target `RuleCtx` behaviour.

   - Update unit tests in `src/linter/rule.rs` or new `src/linter/context.rs`
     tests for constructor/accessor semantics.
   - Update behavioural tests in `tests/linter_rule_traits.rs` to construct
     non-placeholder context and assert source/config/AST use inside a rule.

2. Implement context model in linter module.

   - Add `RuleConfigValue` and `RuleConfig`.
   - Replace placeholder `RuleCtx` with concrete fields and methods.
   - Keep `CstRule` signatures stable (`ctx: &RuleCtx`).

3. Re-export public types.

   - Update `src/linter/mod.rs` to re-export context types from stable paths.

4. Update design documentation.

   - Amend `docs/ddlint-design.md` section `3.1` to reflect the implemented
     `RuleCtx` API and rationale.

5. Run required validation commands from repository root.

   - Documentation gates:

       set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log
       set -o pipefail && make fmt 2>&1 | tee /tmp/ddlint-fmt.log
       set -o pipefail && make nixie 2>&1 | tee /tmp/ddlint-nixie.log

   - Required Rust gates:

       set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
       set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
       set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log

6. Mark roadmap item done only after all gates pass.

   - In `docs/roadmap.md`, switch `3.1.2` from `[ ]` to `[x]`.

## Validation and acceptance

Acceptance criteria:

- `RuleCtx` exposes source text, typed config, and AST/CST context via public
  accessors.
- Unit tests cover constructor and accessor behaviour, including config lookups.
- Behavioural tests prove rules can consume all `RuleCtx` channels during CST
  traversal.
- `docs/ddlint-design.md` documents the final context contract and design
  decision(s).
- `docs/roadmap.md` marks `3.1.2` done.
- `make check-fmt`, `make lint`, and `make test` succeed.
- `make markdownlint`, `make fmt`, and `make nixie` succeed.

## Idempotence and recovery

All steps are re-runnable.

If tests fail after context refactor, restore a compiling baseline by keeping
old exports and introducing the new context types behind additive constructors,
then migrate call sites incrementally. Re-run the failing gate first, then
re-run all gates.

## Artifacts and notes

Store gate logs in `/tmp/ddlint-*.log` and summarize pass/fail lines in the
implementation handoff. Keep new tests concise and fixture-based.

## Revision note

- 2026-02-20: Initial draft created for roadmap item `3.1.2`, including API
  proposal, staged implementation sequence, and validation gates.
- 2026-02-21: Implemented `RuleCtx`, updated tests/docs/roadmap, captured a
  Clippy-driven test cleanup, and completed all quality gates.
