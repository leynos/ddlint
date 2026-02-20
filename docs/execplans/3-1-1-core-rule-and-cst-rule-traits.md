# Define core `Rule` and `CstRule` traits

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`, and
`Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: COMPLETE

PLANS.md is not present in this repository.

## Purpose / big picture

This milestone introduces the first linter-engine contracts so rules can be
implemented behind stable trait boundaries. After this work, contributors can
create concrete syntax tree (CST)-based rules that expose consistent metadata
(`Rule`) and analysis hooks (`CstRule`) with compile-time guarantees for
thread-safety and dispatch.

Observable success is:

- a new linter module exports `Rule` and `CstRule` from stable paths;
- unit tests prove trait contracts and object-safety expectations;
- behavioural tests prove hook dispatch against real parsed CST input;
- design docs record the chosen trait signatures and rationale;
- `docs/roadmap.md` marks item `3.1.1` done after all gates pass.

## Constraints

- Follow `docs/ddlint-design.md` section `1.2` and section `3.1` as the
  normative design for metadata + CST rule behaviour.
- Keep this milestone scoped to `3.1.1`: trait contracts only. Full `RuleCtx`
  data plumbing, rule store registration, and parallel runner are separate
  roadmap items (`3.1.2` to `3.1.4`).
- Do not add external dependencies.
- Keep parser behaviour and existing abstract syntax tree (AST) / concrete
  syntax tree (CST) APIs unchanged.
- Every new Rust module begins with a `//!` module comment.
- Public APIs added in this milestone must include Rustdoc comments.
- Keep files below 400 lines; split modules if needed.
- Use `rstest` for shared fixture setup and parameterized cases where it
  improves clarity.
- Update design documentation for all signature or naming decisions.
- Run required quality gates with `set -o pipefail` and `tee`.

## Tolerances (exception triggers)

- Scope: if implementation needs more than 9 files or 320 net new lines, stop
  and escalate.
- Interface: if trait design requires changing existing parser public APIs,
  stop and escalate.
- Dependencies: if a new crate is required, stop and escalate.
- Ambiguity: if a decision materially affects future roadmap items (`3.1.2+`)
  and docs do not resolve it, stop and record options in `Decision Log`.
- Iterations: if tests still fail after three focused fix cycles, stop and
  escalate with failing test names.

## Risks

- Risk: naming collision between lint `Rule` trait and existing AST
  `parser::ast::Rule` type can reduce readability. Severity: medium Likelihood:
  high Mitigation: place lint traits under `crate::linter` namespace and avoid
  wildcard imports in tests/docs.

- Risk: choosing signatures that overfit current assumptions could force
  breaking changes in `3.1.2` (`RuleCtx`) or `3.1.3` (`CstRuleStore`).
  Severity: high Likelihood: medium Mitigation: keep signatures minimal,
  additive, and explicit about extension points (for example default hook
  methods).

- Risk: behavioural tests may accidentally re-implement a future runner instead
  of validating contracts. Severity: medium Likelihood: medium Mitigation: keep
  behavioural test harness tiny and local to integration tests, only validating
  dispatch and hook invocation guarantees.

## Progress

- [x] (2026-02-18 00:00Z) Reviewed roadmap item `3.1.1`, referenced design
  docs, and existing parser/test structure.
- [x] (2026-02-18 00:10Z) Authored initial ExecPlan draft.
- [x] (2026-02-19 00:15Z) Implemented linter trait module and crate exports.
- [x] (2026-02-19 00:20Z) Added unit tests for trait contracts and helper
  types.
- [x] (2026-02-19 00:25Z) Added behavioural CST dispatch tests using
  `examples/hello_join.dl` and `examples/reachability.dl`.
- [x] (2026-02-19 00:30Z) Updated design docs with concrete trait signatures.
- [x] (2026-02-19 00:50Z) Ran Markdown and Rust quality gates with logged
  output.
- [x] (2026-02-19 00:55Z) Marked roadmap item `3.1.1` done.

## Surprises & Discoveries

- Observation: `docs/roadmap.md` references
  `docs/ddlint-design-and-road-map.md`, but the repository currently provides
  `docs/ddlint-design.md` as the relevant design source. Evidence:
  `rg --files docs` contains `docs/ddlint-design.md` and no
  `docs/ddlint-design-and-road-map.md`. Impact: this plan treats
  `docs/ddlint-design.md` as canonical for section `3.1` references and records
  that decision below.

- Observation: there is no existing `src/linter/` module; current code is
  parser-first. Evidence: `src/` contains parser/tokenizer/language modules
  only. Impact: this milestone must introduce the initial linter module
  boundary.

- Observation: the environment does not expose qdrant Model Context Protocol
  (MCP) tools. Evidence: `list_mcp_resources` and `list_mcp_resource_templates`
  returned empty lists. Impact: project-memory retrieval/storage could not be
  completed during this implementation session.

## Decision Log

- Decision: use `docs/ddlint-design.md` section `3.1` as the normative source
  for this milestone, despite roadmap link text naming a non-existent file.
  Rationale: it is the only in-repo design doc with the required trait content.
  Date/Author: 2026-02-18 / assistant

- Decision: keep `3.1.1` strictly focused on trait contracts and contract
  tests, while deferring full execution infrastructure to later roadmap items.
  Rationale: preserves roadmap sequencing and keeps change scope atomic.
  Date/Author: 2026-02-18 / assistant

- Decision: include `RuleCtx` and `LintDiagnostic` as lightweight placeholders
  in `3.1.1` to stabilize trait signatures while deferring rich context and
  reporting behaviour to later milestones. Rationale: allows immediate rule
  authoring and behavioural testing without pre-empting `3.1.2` design.
  Date/Author: 2026-02-19 / assistant

## Outcomes & Retrospective

Implemented a new `crate::linter` module with core traits and lightweight
supporting types:

- `Rule` for rule metadata;
- `CstRule` for CST node/token hooks with default no-op methods;
- `RuleCtx` placeholder context type;
- `LintDiagnostic` minimal diagnostic container.

Validation added:

- Unit tests in `src/linter/rule.rs` verify metadata dispatch, trait-object
  usage, `Send + Sync` constraints, and diagnostic accessor behaviour.
- Behavioural tests in `tests/linter_rule_traits.rs` run a local dispatch loop
  against real examples and confirm node/token hook invocation counts and
  default no-op behaviour.

Documentation and roadmap:

- `docs/ddlint-design.md` section `3.1` now records concrete signatures and
  implementation location.
- `docs/roadmap.md` marks `3.1.1` as done.

All quality gates passed:

- `make fmt`
- `make markdownlint`
- `make nixie`
- `make check-fmt`
- `make lint`
- `make test`

## Context and orientation

Current repository state is parser-centric:

- `src/lib.rs` exports parser/tokenizer/language modules only.
- `src/parser/mod.rs` exposes parsing entrypoints and parse diagnostics.
- `src/parser/ast/` already defines an AST `Rule` wrapper type, so lint trait
  names must stay namespaced to avoid confusion.
- Integration tests under `tests/` currently validate parser behaviour.

Planned implementation area for this milestone:

- New module namespace under `src/linter/` for lint engine contracts.
- Export path updates in `src/lib.rs`.
- New unit tests under `src/linter/` (module-local).
- New behavioural integration tests under `tests/` using real `parse(...)`
  output and a tiny local dispatch helper.
- Documentation updates in `docs/ddlint-design.md` and `docs/roadmap.md`.

## Interfaces and dependencies

Target public surface after this milestone:

- `crate::linter::Rule`
- `crate::linter::CstRule`

Target signature shape (exact names may be adjusted during implementation, but
behaviour must match):

    pub trait Rule {
        fn name(&self) -> &'static str;
        fn group(&self) -> &'static str;
        fn docs(&self) -> &'static str;
    }

    pub trait CstRule: Rule + Send + Sync {
        fn target_kinds(&self) -> &'static [SyntaxKind];

        fn check_node(
            &self,
            node: &rowan::SyntaxNode<DdlogLanguage>,
            ctx: &RuleCtx,
            diagnostics: &mut Vec<LintDiagnostic>,
        ) {
        }

        fn check_token(
            &self,
            token: &rowan::SyntaxToken<DdlogLanguage>,
            ctx: &RuleCtx,
            diagnostics: &mut Vec<LintDiagnostic>,
        ) {
        }
    }

Notes for scope control:

- `RuleCtx` and `LintDiagnostic` may be introduced as minimal placeholders only
  if needed to keep trait signatures stable; substantive implementations belong
  to later roadmap items.
- Keep contracts object-safe and `Send + Sync` compatible.

## Plan of work

Stage A: scaffold traits and establish failing tests first.

Add unit and behavioural tests that express required contracts before
finalizing implementation details. Tests should fail until the trait module and
exports exist.

Go/no-go: proceed only when tests clearly encode metadata requirements,
`Send + Sync` constraints, and dispatch behaviour.

Stage B: implement trait module and crate exports.

Create the linter module and implement the `Rule` and `CstRule` traits with
clear Rustdoc. Wire exports from `src/lib.rs` so downstream code can use stable
paths.

Go/no-go: proceed only when trait module compiles and tests can import the
public API.

Stage C: complete contract tests.

Implement unit tests for:

- metadata methods (`name`, `group`, `docs`) returning stable values;
- trait object usability for `&dyn CstRule`;
- compile-time `Send + Sync` expectations for rule implementations.

Implement behavioural tests that parse a small DDlog program and use a local
mini-dispatch helper to call hooks for matching `SyntaxKind`s, verifying rule
invocation counts and deterministic ordering.

Go/no-go: proceed only when all new tests pass and no parser regressions occur.

Stage D: documentation and roadmap updates, then full validation.

Update `docs/ddlint-design.md` with the final trait signatures and rationale.
Mark `docs/roadmap.md` item `3.1.1` as done only after all quality gates pass.

## Concrete steps

All commands run from repository root (`/home/user/project`).

1. Create linter trait module and exports.

   - Add `src/linter/mod.rs`.
   - Add `src/linter/rule.rs` (or equivalent) with `Rule` and `CstRule`.
   - Update `src/lib.rs` to export the new module.

2. Add unit tests for trait contracts.

   - Add tests under `src/linter/tests/` or module-local `#[cfg(test)]` blocks.
   - Use `rstest` where parameterization improves readability.

3. Add behavioural integration tests.

   - Add `tests/linter_rule_traits.rs` with end-to-end parse input and local
     dispatch helper.
   - Ensure tests validate node/token hook invocation over real CST elements.

4. Update design docs and roadmap.

   - Update `docs/ddlint-design.md` section `3.1` with concrete signatures.
   - Mark roadmap checkbox `3.1.1` done in `docs/roadmap.md`.

5. Run Markdown validation for doc changes.

    set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log
    set -o pipefail && make fmt 2>&1 | tee /tmp/ddlint-fmt.log
    set -o pipefail && make nixie 2>&1 | tee /tmp/ddlint-nixie.log

6. Run required Rust quality gates.

    set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
    set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
    set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log

Expected success indicators:

- each command exits with status `0`;
- lint/test logs contain no failures;
- `make test` includes new unit and behavioural tests for this milestone.

## Validation and acceptance

Acceptance criteria for `3.1.1`:

- `crate::linter::Rule` and `crate::linter::CstRule` exist with documented,
  stable signatures aligned to `docs/ddlint-design.md` section `3.1`.
- Unit tests validate metadata and trait constraints.
- Behavioural tests validate CST hook dispatch against parsed DDlog input.
- `make check-fmt`, `make lint`, and `make test` pass.
- If docs are changed, `make markdownlint`, `make fmt`, and `make nixie` pass.
- `docs/roadmap.md` marks item `3.1.1` done.

## Idempotence and recovery

All steps are re-runnable.

If a stage fails:

- fix the immediate issue;
- re-run the failed command;
- then re-run the full gate set before marking completion.

If trait signatures must change after tests are written, update tests and
`docs/ddlint-design.md` in the same change so contracts and documentation stay
in sync.

## Artifacts and notes

Keep command logs under `/tmp/ddlint-*.log` and summarize pass/fail outcomes in
implementation notes or commit body.

Prefer concise fixtures in tests. If DDlog samples become long, move them into
named fixture constants to keep test intent readable.

## Revision note

Revised to COMPLETE after implementing `3.1.1`, adding tests and docs, and
passing all required quality gates.
