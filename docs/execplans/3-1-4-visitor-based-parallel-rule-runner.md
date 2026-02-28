# Build a visitor-based parallel rule runner (roadmap 3.1.4)

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`, and
`Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: COMPLETE

## Purpose / big picture

Roadmap item 3.1.4 introduces the **Rule Runner**: a visitor-based traversal
engine that walks the concrete syntax tree (CST) and invokes applicable lint
rules via `CstRuleStore`, dispatching rule evaluations across a `rayon` thread
pool for parallel execution. This completes the linter engine core (milestone
3.1), connecting rule traits (3.1.1), context (3.1.2), and store (3.1.3) into a
functioning execution pipeline.

Observable success is:

- `crate::linter::Runner` is a public struct that accepts a `CstRuleStore`,
  parsed input, source text, and per-rule configuration, then returns a sorted
  `Vec<LintDiagnostic>`.
- Parallel rule execution produces identical diagnostics to sequential dispatch
  (verified by behavioural tests against the existing `run_store_over_cst`
  pattern in `tests/linter_rule_store.rs`).
- Diagnostics are sorted deterministically by `(span.start, span.end,
  rule_name)` regardless of thread scheduling.
- Unit and behavioural tests cover empty stores, single rules, multiple rules,
  overlapping target kinds, empty target kinds, config-aware rules, ordering
  determinism, and `Send + Sync` conformance.
- `docs/ddlint-design.md` section 3.1 documents the `Runner` API contract and
  the per-rule parallelism strategy.
- `docs/roadmap.md` marks item 3.1.4 as done.
- `make check-fmt`, `make lint`, and `make test` all succeed.

## Constraints

- Treat `docs/ddlint-design.md` sections 1.2 and 3.1 as the normative design
  source for this milestone.
- Keep scope limited to roadmap item 3.1.4. Do not implement `declare_lint!`
  (3.2) or any concrete lint rules.
- Do not modify the `Rule` or `CstRule` trait definitions from 3.1.1.
- Do not modify `RuleCtx` from 3.1.2.
- Do not modify `CstRuleStore` from 3.1.3.
- Keep existing parser behaviour unchanged.
- Every new Rust module must start with a `//!` module comment.
- Keep files below 400 lines by splitting modules when needed.
- Validate with unit and behavioural tests.
- Record design decisions in `docs/ddlint-design.md`.
- Use Make targets and run gates with `set -o pipefail` and `tee`.
- Satisfy all strict Clippy lints in `Cargo.toml` lines 26-60 (no
  `indexing_slicing`, no `unwrap_used`/`expect_used`, `#[must_use]` on pure
  queries, `Default` when `new()` takes no args, `use_self`).
- Use en-GB-oxendict spelling in comments and documentation.
- Only `rayon` may be added as a new dependency.

## Tolerances (exception triggers)

- Scope: if implementation exceeds 8 files changed or 400 net new lines, stop
  and escalate.
- Interface: if `Rule`, `CstRule`, `RuleCtx`, or `CstRuleStore` signatures
  must change, stop and escalate.
- Dependencies: if a dependency other than `rayon` is required, stop and
  escalate.
- Thread safety: if `GreenNode::clone()` proves insufficient for thread-local
  tree reconstruction, stop and document alternatives in `Decision Log`.
- Iterations: if tests still fail after three focused fix cycles, stop and
  escalate with failing test names.

## Risks

- Risk: `Root::from_green(green.clone())` may not produce a fully functional
  tree for rule dispatch in a different thread. Severity: high. Likelihood:
  low. Mitigation: `Root::from_green` is already used in `parse()`
  (`src/parser/mod.rs`) and works by calling `SyntaxNode::new_root(green)`.
  `GreenNode` is `Send + Sync + Clone` by design. A unit test will verify tree
  equivalence after reconstruction.

- Risk: Per-rule parallelism introduces non-deterministic diagnostic ordering.
  Severity: medium. Likelihood: high. Mitigation: sort diagnostics by
  `(span.start, span.end, rule_name)` before returning. Behavioural tests run
  multiple iterations to confirm determinism.

- Risk: Strict Clippy lints (`indexing_slicing`, `unwrap_used`) flag patterns
  in test or implementation code. Severity: low. Likelihood: medium.
  Mitigation: use iterator-based patterns, `.get()`, `if let`, and `for` loops.
  Learned from 3.1.3 experience.

- Risk: `RuleConfig::clone()` per-rule cost in parallel dispatch.
  Severity: low. Likelihood: low. Mitigation: configs are small `BTreeMap`s.
  Accept clone cost for this milestone; wrap in `Arc<RuleConfig>` later if
  profiling shows a problem.

## Progress

- [x] (2026-02-26) Write ExecPlan to
  `docs/execplans/3-1-4-visitor-based-parallel-rule-runner.md`.
- [x] (2026-02-26) Add `rayon` dependency to `Cargo.toml`.
- [x] (2026-02-26) Create `src/linter/runner.rs` with `Runner` struct,
  parallel dispatch, and unit tests.
- [x] (2026-02-26) Update `src/linter/mod.rs` to wire runner module and
  re-export `Runner`.
- [x] (2026-02-26) Create `tests/linter_runner/` with behavioural tests
  (`main.rs` and `rules.rs`).
- [x] (2026-02-26) Update `docs/ddlint-design.md` section 3.1 with `Runner`
  contract.
- [x] (2026-02-26) Run quality gates (`make check-fmt`, `make lint`,
  `make test`, `make markdownlint`). All passed.
- [x] (2026-02-26) Mark `docs/roadmap.md` item 3.1.4 as done.

## Surprises & discoveries

- Observation: the project's `rustfmt` configuration enforces multi-line bodies
  for all trait method implementations in test code, even trivial single-line
  accessors like `fn name(&self) -> &'static str { "x" }`. Evidence:
  `make check-fmt` flagged all single-line `Rule` impls in both
  `src/linter/runner.rs` and `tests/linter_runner/main.rs`. Impact: future test
  rules must use multi-line bodies from the start.

- Observation: Clippy's `needless_borrow` lint catches `.cmp(&b.rule_name())`
  on `&'static str` comparisons. The fix is `.cmp(b.rule_name())`. Evidence:
  `make lint` failed with `needless_borrow` in both `runner.rs` and
  `linter_runner/main.rs` for the sort comparator. Impact: string comparisons
  via `Ord::cmp` should not take a reference when the type is already `&str`.

- Observation: Clippy's `redundant_closure_for_method_calls` lint flags
  `|d| d.rule_name()` closures in `.map()` calls, suggesting the method
  reference form `LintDiagnostic::rule_name` instead. Evidence: `make lint`
  failed in the `diagnostics_are_sorted_by_span` unit test. Impact: prefer
  method references over closures for single-method-call maps.

## Decision log

- Decision: use per-rule parallelism (each rule gets its own thread-local
  tree) rather than per-element or batch parallelism. Rationale:
  `rowan::SyntaxNode` and `rowan::SyntaxToken` are `!Send + !Sync` (they use
  `Rc` and thread-local allocators internally). CST elements cannot cross
  thread boundaries. Only `GreenNode` is `Send + Sync + Clone`. Each rayon task
  clones the `GreenNode`, constructs a thread-local red tree via
  `Root::from_green()`, and walks it independently. This is the simplest
  correct approach that satisfies the design document's requirement for
  thread-pool dispatch. Alternatives considered: per-element parallelism
  (impossible, elements are `!Send`), batch collection then `par_iter`
  (impossible for the same reason), switching to `cstree` (out of scope). Date:
  2026-02-25.

- Decision: `Runner` stores `GreenNode` + `Arc<str>` + `RuleConfig` (not
  `RuleCtx` or `Root`) to be `Send + Sync`. Rationale: `RuleCtx` contains
  `Root` which contains `SyntaxNode` (`!Send + !Sync`). Storing only
  `Send + Sync` types makes the `Runner` itself `Send + Sync`, enabling
  file-level parallelism where multiple files are linted concurrently. Date:
  2026-02-25.

- Decision: sort diagnostics by `(span.start, span.end, rule_name)`.
  Rationale: parallel execution produces non-deterministic ordering. Sorting by
  span position matches the natural source-order expectation. Rule name as
  tiebreaker ensures stability when multiple rules fire on the same element.
  Date: 2026-02-25.

- Decision: `Runner::new()` takes `&Parsed` (to extract `GreenNode`) rather
  than `GreenNode` directly. Rationale: ergonomic API — callers already have a
  `Parsed` value. Matches the pattern in `RuleCtx::from_parsed()`. Date:
  2026-02-25.

- Decision: use `rayon = "1.10"` with caret requirement.
  Rationale: follows project convention (caret requirements per AGENTS.md).
  Date: 2026-02-25.

## Outcomes & retrospective

All observable success criteria met:

- `Runner` implemented in `src/linter/runner.rs` (~270 lines including tests),
  well within the 400-line limit.
- 6 unit tests and 8 behavioural tests (including 2 parameterized fixture
  cases) pass, covering empty stores, single rules, multiple rules, overlapping
  kinds, empty target kinds, config-aware rules, deterministic ordering,
  sort-invariant verification, and `Send + Sync` conformance.
- `docs/ddlint-design.md` section 3.1 updated with the `Runner` API contract
  and per-rule parallelism rationale.
- `docs/roadmap.md` item 3.1.4 marked done.
- All quality gates pass: `make check-fmt`, `make lint`, `make test`,
  `make markdownlint`.

Files modified (8, within tolerance of 8):

- `Cargo.toml` (edit): added `rayon = "1.10"`.
- `src/linter/runner.rs` (new): `Runner` struct, parallel dispatch, unit
  tests.
- `src/linter/mod.rs` (edit): added `mod runner;` and `pub use
  runner::Runner;`.
- `tests/linter_runner/main.rs` (new): behavioural tests and helpers.
- `tests/linter_runner/rules.rs` (new): shared test rule stubs.
- `docs/ddlint-design.md` (edit): documented `Runner` contract in section 3.1.
- `docs/roadmap.md` (edit): marked 3.1.4 done.
- `docs/execplans/3-1-4-visitor-based-parallel-rule-runner.md` (new): this
  ExecPlan.

Lessons:

- The per-rule parallelism strategy (cloning `GreenNode` per rayon task)
  worked cleanly. `Root::from_green(green.clone())` produces a fully functional
  thread-local tree without complications.
- Strict Clippy lints in test code continue to require attention:
  `needless_borrow`, `redundant_closure_for_method_calls`, and
  `indexing_slicing` all fired. Iterator-based assertions and method references
  should be the default pattern.
- The formatter enforces multi-line bodies even for trivial trait methods in
  test code. Single-line accessors like `fn name() -> &str { "x" }` are
  reformatted to multi-line.

## Context and orientation

The `ddlint` project is a CST-based linter for Differential Datalog. The linter
module lives at `src/linter/` and currently contains three files:

- `src/linter/mod.rs` (12 lines): module declaration and re-exports.
- `src/linter/rule.rs` (327 lines): `Rule` trait, `CstRule` trait,
  `LintDiagnostic`, `RuleConfigValue`, `RuleConfig`, `RuleCtx`, and unit tests.
- `src/linter/store.rs` (305 lines): `CstRuleStore` struct, impl, and unit
  tests.

The CST is built on `rowan` 0.15. `SyntaxKind` is defined in `src/language.rs`
as a `#[repr(u16)]` enum with ~173 variants. The `DdlogLanguage` newtype
bridges `SyntaxKind` into rowan's type system.

Key types for the runner:

- `Parsed` (`src/parser/cst_builder/mod.rs`): holds `GreenNode`, `Root`, and
  parse errors. Exposes `green() -> &GreenNode` and `root() -> &Root`.
- `Root` (`src/parser/ast/root.rs`): typed abstract syntax tree (AST) root
  wrapping `SyntaxNode<DdlogLanguage>`. Has `from_green(GreenNode) -> Self` and
  `syntax() -> &SyntaxNode<DdlogLanguage>`.
- `GreenNode` (from `rowan`): immutable, `Send + Sync + Clone`. The shared
  backbone that enables per-rule parallelism.
- `SyntaxNode<DdlogLanguage>` (from `rowan`): `!Send + !Sync`. Thread-local
  typed view over a `GreenNode`.

The existing behavioural test `tests/linter_rule_store.rs` contains a
sequential dispatch helper `run_store_over_cst()` that walks
`root.syntax().descendants_with_tokens()` and dispatches rules via
`store.rules_for_kind(kind)`. The runner formalizes and parallelizes this
pattern.

Thread-safety summary for relevant types:

Table: Send/Sync properties of types used by the visitor-based parallel rule
runner.

| Type               | Send | Sync | Notes                           |
| ------------------ | ---- | ---- | ------------------------------- |
| `GreenNode`        | yes  | yes  | Arc-backed immutable data       |
| `SyntaxNode<L>`    | no   | no   | Uses Rc, thread-local           |
| `Root`             | no   | no   | Wraps SyntaxNode                |
| `RuleCtx`          | no   | no   | Contains Root                   |
| `Arc<dyn CstRule>` | yes  | yes  | Trait bound enforces it         |
| `CstRuleStore`     | yes  | yes  | Vec + HashMap of Arc            |
| `Arc<str>`         | yes  | yes  | Shared string                   |
| `RuleConfig`       | yes  | yes  | BTreeMap of owned values        |
| `LintDiagnostic`   | yes  | yes  | Static str + String + TextRange |

## Plan of work

### Stage A: add rayon dependency

Edit `Cargo.toml` to add `rayon = "1.10"` to the `[dependencies]` section,
following the project's caret-requirement convention.

### Stage B: create `src/linter/runner.rs`

Create the runner module containing the `Runner` struct, the parallel dispatch
implementation, and unit tests. Target: well under 400 lines.

The `Runner` struct stores only `Send + Sync` types:

```rust
pub struct Runner<'a> {
    store: &'a CstRuleStore,
    green: GreenNode,
    source_text: Arc<str>,
    config: RuleConfig,
}
```

The constructor `Runner::new()` takes `&CstRuleStore`, source text (as
`impl Into<Arc<str>>`), `&Parsed`, and `RuleConfig`. It clones the green node
from `parsed.green()`.

The `run()` method implements the parallel dispatch algorithm:

1. If the store is empty, return an empty `Vec`.
2. Use `rayon::prelude::par_iter()` over `store.all_rules()`.
3. For each rule, call a private helper `run_single_rule()` that:
   a. Clones the `GreenNode` and constructs a thread-local `Root` via
      `Root::from_green(green.clone())`.
   b. Constructs a thread-local `RuleCtx` from the cloned root, shared
      source text (`Arc::clone`), and cloned config.
   c. Iterates `root.syntax().descendants_with_tokens()`. d. For each element
   whose kind is in `rule.target_kinds()`, dispatches to
      `check_node` or `check_token` as appropriate.
   e. Returns the collected `Vec<LintDiagnostic>`.
4. Collects all per-rule diagnostic vectors via `flat_map_iter`.
5. Sorts diagnostics by `(span.start, span.end, rule_name)`.
6. Returns the sorted vector.

A private `sort_diagnostics()` helper encapsulates the sort logic.

Unit tests (in `#[cfg(test)] mod tests`):

- `runner_is_send_and_sync`: compile-time bound assertion.
- `empty_store_produces_no_diagnostics`: trivial base case.
- `single_rule_produces_diagnostics`: basic functionality.
- `diagnostics_are_sorted_by_span`: ordering verification.
- `multiple_rules_produce_merged_diagnostics`: parallel merge correctness.

These tests use lightweight `StubRule` structs following the pattern from
`src/linter/store.rs` tests.

### Stage C: update `src/linter/mod.rs`

Add `mod runner;` and `pub use runner::Runner;`. Update the module-level `//!`
comment to mention the parallel runner.

### Stage D: create `tests/linter_runner/`

Behavioural tests following the established pattern from
`tests/linter_rule_store.rs`, split into `main.rs` (tests, helpers, fixtures)
and `rules.rs` (shared test rule stubs). These re-use the same test rule
patterns (`CountingRule`, `EmptyTargetsRule`, `ConfigAwareRule`,
`IdentTokenRule`) from the existing store tests.

Key tests:

- `runner_matches_sequential_dispatch` (parameterized with `#[rstest]` over
  `hello_join.dl` and `reachability.dl`): runs both the existing sequential
  `run_store_over_cst` helper and `Runner::run()`, sorts both results, asserts
  they are identical. This is the critical correctness test.
- `runner_with_empty_store_produces_no_diagnostics`: empty store returns
  empty result.
- `runner_with_multiple_rules`: registers `CountingRule` and `IdentTokenRule`,
  verifies diagnostics from both are present and correctly merged.
- `runner_handles_empty_target_kinds`: registers `EmptyTargetsRule` alongside
  `CountingRule`, verifies `EmptyTargetsRule` is never dispatched (its
  `check_*` methods panic).
- `runner_respects_config`: registers `ConfigAwareRule` with enabled=true and
  enabled=false configs, verifies config is correctly threaded through parallel
  execution.
- `runner_produces_deterministic_ordering`: runs the same lint multiple times,
  asserts identical ordering each time.
- `runner_diagnostics_sorted_by_span_then_rule_name`: uses multiple rules
  targeting the same kind, verifies `(span.start, span.end, rule_name)` sort.

### Stage E: update documentation

Edit `docs/ddlint-design.md` section 3.1 (after the `CstRuleStore` paragraph,
around line 500) to add a new section documenting the `Runner` API contract,
the per-rule parallelism strategy, and the rowan thread-safety rationale.

### Stage F: quality gates and roadmap

Run all quality gates:

```bash
set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log
set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log
```

Fix any issues. Then mark `docs/roadmap.md` item 3.1.4 as done.

## Concrete steps

All commands run from the repository root (`/home/user/project`).

1. Write the ExecPlan to
   `docs/execplans/3-1-4-visitor-based-parallel-rule-runner.md`.

2. Edit `Cargo.toml`: add `rayon = "1.10"` to `[dependencies]` after the
   `smallvec` line.

3. Create `src/linter/runner.rs` with the full implementation and unit tests.

4. Edit `src/linter/mod.rs`: add `mod runner;`, update `pub use` to include
   `Runner`, and update the module-level doc comment.

5. Create `tests/linter_runner/main.rs` and `tests/linter_runner/rules.rs`
   with behavioural tests and shared rule stubs.

6. Run:

   ```bash
   set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log
   ```

   Expected: all tests pass, including the new tests in
   `linter::runner::tests::*` and `linter_runner::*`.

7. Edit `docs/ddlint-design.md` section 3.1 (after the `CstRuleStore`
   paragraph) to document the `Runner` contract and parallelism strategy.

8. Run:

   ```bash
   set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
   set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
   set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log
   ```

   Expected: all gates pass with zero warnings.

9. Edit `docs/roadmap.md` line 240: change `- [ ]` to `- [x]` for item 3.1.4.

## Validation and acceptance

Quality criteria (what "done" means):

- Tests: `make test` passes. New unit tests in `src/linter/runner.rs` and
  behavioural tests in `tests/linter_runner/main.rs` are green.
- Lint/typecheck: `make check-fmt` and `make lint` pass with zero warnings.
- Markdown: `make markdownlint` passes.
- Design doc: `docs/ddlint-design.md` section 3.1 includes the `Runner`
  contract and parallelism rationale.
- Roadmap: `docs/roadmap.md` item 3.1.4 is marked `[x]`.

Quality method (verification steps):

```bash
set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log
set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log
```

The new tests verify:

- Runner with an empty store produces no diagnostics.
- Runner with a single rule produces the same diagnostics as sequential
  dispatch (tested across `hello_join.dl` and `reachability.dl` fixtures).
- Runner with multiple rules produces merged diagnostics from all rules.
- Runner with overlapping target kinds dispatches correctly.
- Runner with empty `target_kinds` rule never dispatches that rule.
- Runner correctly threads `RuleConfig` through parallel rule invocations.
- Diagnostics are sorted by `(span.start, span.end, rule_name)`.
- Diagnostic ordering is deterministic across multiple runs.
- `Runner` is `Send + Sync` (compile-time assertion).

## Idempotence and recovery

All steps are idempotent. Creating or overwriting `runner.rs` and the
`linter_runner/` test module is safe. Edits to `mod.rs`, `Cargo.toml`,
`ddlint-design.md`, and `roadmap.md` are additive and can be re-applied.
Quality gate commands are read-only checks. If a step fails, fix the issue and
re-run from that step.

## Artefacts and notes

### Thread-safety rationale

`rowan` 0.15's `SyntaxNode<L>` and `SyntaxToken<L>` are `!Send + !Sync` because
they use `Rc` and thread-local allocators. Only `GreenNode` (the immutable
backbone) is `Send + Sync + Clone`. The per-rule parallelism strategy works
around this by having each rayon task clone the `GreenNode` and construct its
own thread-local red tree via `Root::from_green(green.clone())`. This is a
well-established rowan pattern: the green tree is the shared, immutable data;
red trees are cheap, ephemeral views.

### Sequential dispatch baseline (from `tests/linter_rule_store.rs`)

```rust
fn run_store_over_cst(
    parsed: &Parsed,
    ctx: &RuleCtx,
    store: &CstRuleStore,
) -> Vec<LintDiagnostic> {
    let mut diagnostics = Vec::new();
    for element in parsed.root().syntax().descendants_with_tokens() {
        let kind = element.kind();
        for rule in store.rules_for_kind(kind) {
            match &element {
                NodeOrToken::Node(node) => {
                    rule.check_node(node, ctx, &mut diagnostics);
                }
                NodeOrToken::Token(token) => {
                    rule.check_token(token, ctx, &mut diagnostics);
                }
            }
        }
    }
    diagnostics
}
```

The behavioural tests sort both the sequential and parallel results by the same
`(span.start, span.end, rule_name)` key and assert equality.

### Runner public interface

In `src/linter/runner.rs`, define:

```rust
/// Visitor-based parallel rule runner.
///
/// Walks the concrete syntax tree (CST) and invokes applicable lint rules
/// from a [`CstRuleStore`], dispatching rule evaluations across a `rayon`
/// thread pool for concurrent execution.
///
/// # Thread safety
///
/// `Runner` is `Send + Sync`.  Each rule evaluation constructs a
/// thread-local red tree from the shared [`GreenNode`], so `rowan`'s
/// `!Send` constraint on `SyntaxNode` is respected.
pub struct Runner<'a> {
    store: &'a CstRuleStore,
    green: GreenNode,
    source_text: Arc<str>,
    config: RuleConfig,
}

impl<'a> Runner<'a> {
    /// Create a runner from a rule store, source text, parsed result, and
    /// per-rule configuration.
    #[must_use]
    pub fn new(
        store: &'a CstRuleStore,
        source_text: impl Into<Arc<str>>,
        parsed: &Parsed,
        config: RuleConfig,
    ) -> Self;

    /// Execute all registered rules against the CST in parallel.
    ///
    /// Returns diagnostics sorted by span start, then span end, then rule
    /// name, ensuring deterministic output regardless of thread scheduling.
    #[must_use]
    pub fn run(&self) -> Vec<LintDiagnostic>;
}
```

## Interfaces and dependencies

New dependency: `rayon = "1.10"` (caret requirement, crates.io).

Files modified:

- `Cargo.toml` (edit): add rayon dependency.
- `src/linter/runner.rs` (new): `Runner` struct, impl, unit tests.
- `src/linter/mod.rs` (edit): add `mod runner;` and re-export.
- `tests/linter_runner/main.rs` (new): behavioural tests and helpers.
- `tests/linter_runner/rules.rs` (new): shared test rule stubs.
- `docs/ddlint-design.md` (edit): document `Runner` contract.
- `docs/roadmap.md` (edit): mark 3.1.4 done.
- `docs/execplans/3-1-4-visitor-based-parallel-rule-runner.md` (new): this
  ExecPlan.

Total: 8 files (within 8-file tolerance).

Existing functions and utilities to reuse:

- `Parsed::green() -> &GreenNode` (`src/parser/cst_builder/mod.rs:39`)
- `Root::from_green(GreenNode) -> Self` (`src/parser/ast/root.rs:38`)
- `Root::syntax() -> &SyntaxNode<DdlogLanguage>` (`src/parser/ast/root.rs:32`)
- `RuleCtx::new(source_text, ast_root, config)` (`src/linter/rule.rs:110`)
- `CstRuleStore::all_rules() -> &[Arc<dyn CstRule>]`
  (`src/linter/store.rs:101`)
- `CstRuleStore::is_empty() -> bool` (`src/linter/store.rs:113`)
- `CstRule::target_kinds() -> &'static [SyntaxKind]`
  (`src/linter/rule.rs:203`)
- `CstRule::check_node(...)` (`src/linter/rule.rs:208`)
- `CstRule::check_token(...)` (`src/linter/rule.rs:219`)
- `LintDiagnostic::span() -> TextRange` (`src/linter/rule.rs:47`)
- `LintDiagnostic::rule_name() -> &'static str` (`src/linter/rule.rs:35`)
- Test fixtures: `rstest` `#[fixture]` pattern from `src/linter/store.rs:167`
- Test rules: `StubRule` pattern from `src/linter/store.rs:133`
- Behavioural test rules: `CountingRule`, `IdentTokenRule`,
  `ConfigAwareRule`, `EmptyTargetsRule` from `tests/linter_rule_store.rs`
- Example DDlog files: `examples/hello_join.dl`, `examples/reachability.dl`
