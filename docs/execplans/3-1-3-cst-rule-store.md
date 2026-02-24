# Implement `CstRuleStore` to register and resolve rule handlers by syntax kind

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`, and
`Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: COMPLETE

PLANS.md is not present in this repository.

## Purpose / big picture

Roadmap item `3.1.3` introduces the central registry that maps `SyntaxKind`
values to `CstRule` implementations. The `CstRuleStore` sits between rule
definitions (completed in 3.1.1 and 3.1.2) and the visitor-based parallel rule
runner (3.1.4, next milestone). During concrete syntax tree (CST) traversal the
runner will query the store with each node or token's `SyntaxKind` and receive
back only the rules interested in that kind, avoiding the cost of checking
every rule against every element.

Observable success is:

- `crate::linter::CstRuleStore` is a public struct with `new`, `register`,
  `rules_for_kind`, `all_rules`, `len`, and `is_empty` methods.
- Unit tests verify registration, lookup, multi-rule dispatch, empty-kind
  lookup, insertion order preservation, and `Send + Sync` conformance.
- Behavioural tests verify that store-based dispatch produces the same
  diagnostics as the manual `target_kinds().contains()` loop from
  `tests/linter_rule_traits.rs`.
- `docs/ddlint-design.md` section 3.1 records the `CstRuleStore` API contract.
- `docs/roadmap.md` marks item `3.1.3` done after all gates pass.
- `make check-fmt`, `make lint`, `make test`, and `make markdownlint` all
  succeed.

## Constraints

- Treat `docs/ddlint-design.md` sections `1.2` and `3.1` as the normative
  design source for this milestone.
- Keep scope limited to roadmap item `3.1.3`. Do not implement the parallel
  runner (`3.1.4`) or the `declare_lint!` macro (`3.2`).
- Do not add external dependencies. Use only `std::collections::HashMap` and
  `std::sync::Arc`.
- Do not modify the `Rule` or `CstRule` trait definitions from `3.1.1`.
- Do not modify `RuleCtx` from `3.1.2`.
- Keep existing parser behaviour unchanged.
- Every new Rust module must start with a `//!` module comment.
- Keep files below 400 lines by splitting modules when needed.
- Validate with unit and behavioural tests.
- Record design decisions in `docs/ddlint-design.md`.
- Use Make targets and run gates with `set -o pipefail` and `tee`.
- Satisfy all strict Clippy lints in `Cargo.toml` lines 26-60 (no indexing, no
  unwrap/expect, `#[must_use]` on pure queries, `Default` when `new()` takes no
  args, `use_self`).

## Tolerances (exception triggers)

- Scope: if implementation exceeds 8 files changed or 300 net new lines, stop
  and escalate.
- Interface: if `Rule` or `CstRule` signatures must change, stop and escalate.
- Dependencies: if a new crate dependency is required, stop and escalate.
- Ambiguity: if the store's ownership model (`Arc` vs index) proves unworkable
  with the existing traits, stop and record options in `Decision Log`.
- Iterations: if tests still fail after three focused fix cycles, stop and
  escalate with failing test names.

## Risks

- Risk: Doctest for `CstRuleStore` may fail if it requires parser internals not
  available in doc context. Severity: low. Likelihood: medium. Mitigation: use
  `no_run` on the doctest code block and rely on integration tests for
  end-to-end validation. Outcome: mitigated. Used `rust,no_run` on the struct
  doctest.

- Risk: Strict Clippy lints (`indexing_slicing`, `unwrap_used`) may flag
  patterns in HashMap usage. Severity: low. Likelihood: low. Mitigation: use
  only `HashMap::get` (returns `Option`) and `Vec::as_slice` (safe). No
  indexing or unwrapping needed. Outcome: realized in test code. Initial unit
  test used direct indexing which tripped `indexing_slicing`. Replaced with
  iterator-based assertions.

- Risk: `Arc<dyn CstRule>` forces the runner (3.1.4) to work with `Arc`.
  Severity: low. Likelihood: high (this is deliberate). Mitigation: the runner
  will need shared references for parallel dispatch anyway. `Arc` is the
  idiomatic choice and `Arc::clone` is cheap (atomic increment). Outcome:
  accepted by design.

## Progress

- [x] (2026-02-23) Create `src/linter/store.rs` with struct, impl, and unit
  tests.
- [x] (2026-02-23) Wire module in `src/linter/mod.rs` and re-export
  `CstRuleStore`.
- [x] (2026-02-23) Create `tests/linter_rule_store.rs` with behavioural tests.
- [x] (2026-02-23) Update `docs/ddlint-design.md` section 3.1 with
  `CstRuleStore` contract.
- [x] (2026-02-23) Run quality gates (`make check-fmt`, `make lint`,
  `make test`, `make markdownlint`). All passed.
- [x] (2026-02-23) Mark `docs/roadmap.md` item 3.1.3 done.

## Surprises & discoveries

- Observation: the strict `clippy::indexing_slicing = "deny"` lint also applies
  inside `#[cfg(test)]` blocks, so even test code cannot use `slice[0]`.
  Evidence: `make lint` failed with `indexing may panic` on `rules[0]` in the
  `rule_metadata_accessible_through_store` test. Impact: tests must use
  iterator-based assertions or `.first()` with
  `#[expect(clippy::expect_used)]`. Chose iterator-based approach to avoid
  needing lint suppression.

## Decision log

- Decision: use `Arc<dyn CstRule>` for shared ownership across kind entries.
  Rationale: a single rule may target multiple `SyntaxKind` values (e.g.,
  `CountingRule` targets `N_RELATION_DECL` and `K_RELATION`).
  `Box<dyn CstRule>` is not `Clone`, so the options are index-based indirection
  or `Arc`. `Arc` is cleaner, safe, and `Send + Sync` when the inner type is
  (which `CstRule: Send + Sync` guarantees). The project already uses
  `Arc<str>` in `RuleCtx` (`src/linter/rule.rs:8`), so `Arc` is an established
  pattern. Date: 2026-02-22.

- Decision: `register` accepts `Box<dyn CstRule>` and wraps internally in
  `Arc`. Rationale: callers construct rules with `Box::new(MyRule)`, which is
  the standard Rust pattern. Requiring callers to produce `Arc` would add
  friction without benefit. The internal `Arc` wrapping is a one-time cost per
  registration. Date: 2026-02-22.

- Decision: `rules_for_kind` returns `&[Arc<dyn CstRule>]`, not `Option`.
  Rationale: returning an empty slice for unregistered kinds avoids `Option`
  wrapping and simplifies the runner loop. `HashMap::get` +
  `map_or(&[], Vec::as_slice)` produces this naturally. Date: 2026-02-22.

- Decision: no builder pattern; mutable-then-freeze via Rust's borrow system.
  Rationale: the store is populated mutably during setup, then shared immutably
  (`&CstRuleStore`) during traversal. Rust's ownership rules enforce this
  without a separate frozen type. A builder can be added additively later if
  needed. Date: 2026-02-22.

- Decision: no duplicate-name detection.
  Rationale: keeping the store simple. If the same rule is registered twice
  (different allocations), both registrations are honoured. Name-based
  deduplication can be added later with a `HashSet<&'static str>`. Date:
  2026-02-22.

- Decision: use iterator-based assertions in unit tests instead of direct
  indexing or `.first().expect(...)`. Rationale: the project denies
  `clippy::indexing_slicing` even in test code, and using
  `#[expect(clippy::expect_used)]` adds noise. Collecting names via
  `.iter().map(|r| r.name()).collect::<Vec<_>>()` is equally readable and
  avoids all lint issues. Date: 2026-02-23.

## Outcomes & retrospective

All observable success criteria met:

- `CstRuleStore` implemented in `src/linter/store.rs` (~285 lines including
  tests), well within the 400-line limit.
- 11 unit tests and 4 behavioural tests pass, covering registration, lookup,
  multi-rule dispatch, empty stores, insertion order, chaining, `Send + Sync`,
  and equivalence with the manual dispatch loop.
- `docs/ddlint-design.md` section 3.1 updated with the `CstRuleStore` API
  contract.
- `docs/roadmap.md` item 3.1.3 marked done.
- All quality gates pass: `make check-fmt`, `make lint`, `make test`,
  `make markdownlint`.

Files modified (5, within tolerance of 8):

- `src/linter/store.rs` (new)
- `src/linter/mod.rs` (edit)
- `tests/linter_rule_store.rs` (new)
- `docs/ddlint-design.md` (edit)
- `docs/roadmap.md` (edit)

Lesson: always account for strict Clippy lints in test code, not just
production code. The `indexing_slicing = "deny"` lint applies globally. Future
milestones should use iterator-based test assertions from the start.

## Context and orientation

The `ddlint` project is a concrete syntax tree (CST)-based linter for
Differential Datalog. Its source tree lives in `/home/user/project/`. The
linter module is at `src/linter/`, now containing:

- `src/linter/mod.rs` (12 lines): module declaration and re-exports.
- `src/linter/rule.rs` (327 lines): `Rule` trait, `CstRule` trait,
  `LintDiagnostic`, `RuleConfigValue`, `RuleConfig`, `RuleCtx`, and unit tests.
- `src/linter/store.rs` (~285 lines): `CstRuleStore` struct, impl, and unit
  tests.

The CST is built on `rowan` 0.15. `SyntaxKind` is defined in `src/language.rs`
as a `#[repr(u16)]` enum with ~173 variants (tokens `T_*`, keywords `K_*`,
nodes `N_*`). It derives `Clone`, `Copy`, `Hash`, `Eq`, `PartialEq`, `Ord`,
`PartialOrd`, and `Debug`.

The `CstRule` trait (from 3.1.1) declares
`target_kinds() -> &'static [SyntaxKind]` which tells the store which kinds
each rule is interested in. The existing behavioural test file
`tests/linter_rule_traits.rs` demonstrates the manual dispatch pattern using
`target_kinds().contains()` inside a `descendants_with_tokens()` loop. The
`CstRuleStore` replaces this linear scan with O(1) hash lookup.

## Plan of work

### Stage A: create store module with tests (test-first)

Create `src/linter/store.rs` containing:

1. Module-level `//!` comment describing the store's purpose.
2. Imports: `std::collections::HashMap`, `std::sync::Arc`, `crate::SyntaxKind`,
   `crate::linter::CstRule`.
3. `CstRuleStore` struct with two fields:
   - `rules: Vec<Arc<dyn CstRule>>` (canonical list, insertion order).
   - `by_kind: HashMap<SyntaxKind, Vec<Arc<dyn CstRule>>>` (dispatch index).
4. `impl CstRuleStore` with methods:
   - `pub fn new() -> Self` (`#[must_use]`)
   - `pub fn register(&mut self, rule: Box<dyn CstRule>) -> &mut Self`
   - `pub fn rules_for_kind(&self, kind: SyntaxKind) -> &[Arc<dyn CstRule>]`
     (`#[must_use]`)
   - `pub fn all_rules(&self) -> &[Arc<dyn CstRule>]` (`#[must_use]`)
   - `pub fn len(&self) -> usize` (`#[must_use]`)
   - `pub fn is_empty(&self) -> bool` (`#[must_use]`)
5. `impl Default for CstRuleStore` delegating to `Self::new()`.
6. Rustdoc on the struct with a complete example (using `no_run` since the
   example cannot be compiled in doc-test context).
7. All public methods documented with `///` doc comments.
8. `#[cfg(test)] mod tests` block with unit tests.

Wire the module in `src/linter/mod.rs`:

    mod store;
    pub use store::CstRuleStore;

Update the module-level comment in `mod.rs` to mention the rule-dispatch
registry.

Create `tests/linter_rule_store.rs` with behavioural tests.

### Stage B: implement core logic

Fill in the method bodies:

- `new()`: return `Self` with empty `Vec` and empty `HashMap`.
- `register(rule)`: wrap `Box` in `Arc`, iterate `rule.target_kinds()`, insert
  `Arc::clone(&rule)` into `by_kind.entry(kind).or_default()`, push the `Arc`
  into `rules`, return `&mut self`.
- `rules_for_kind(kind)`: `self.by_kind.get(&kind).map_or(&[],
  Vec::as_slice)`.
- `all_rules()`: `&self.rules`.
- `len()`: `self.rules.len()`.
- `is_empty()`: `self.rules.is_empty()`.

### Stage C: documentation

Update `docs/ddlint-design.md` section 3.1 (after the `RuleCtx` paragraph at
line 475) with a new paragraph and code block documenting the `CstRuleStore`
API contract.

### Stage D: quality gates and roadmap

Run all quality gates:

    set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
    set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
    set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log
    set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log

Fix any issues. Then mark `docs/roadmap.md` item 3.1.3 as done.

## Concrete steps

All commands run from the repository root `/home/user/project/`.

1. Create `src/linter/store.rs` with the full implementation, doc comments, and
   unit tests.

2. Edit `src/linter/mod.rs` to add `mod store;` and update the `pub use` line
   to include `CstRuleStore`.

3. Create `tests/linter_rule_store.rs` with behavioural tests.

4. Run:

       set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log

   Expected: all tests pass, including the new tests in
   `linter::store::tests::*` and `linter_rule_store::*`.

5. Edit `docs/ddlint-design.md` section 3.1 (after line 475) to document the
   `CstRuleStore` contract.

6. Run:

       set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
       set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
       set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log

   Expected: all gates pass with zero warnings.

7. Edit `docs/roadmap.md` line 238: change `- [ ]` to `- [x]` for item 3.1.3.

## Validation and acceptance

Quality criteria (what "done" means):

- Tests: `make test` passes. New unit tests in `src/linter/store.rs` and
  behavioural tests in `tests/linter_rule_store.rs` are green.
- Lint/typecheck: `make check-fmt` and `make lint` pass with zero warnings.
- Markdown: `make markdownlint` passes.
- Design doc: `docs/ddlint-design.md` section 3.1 includes the `CstRuleStore`
  contract.
- Roadmap: `docs/roadmap.md` item 3.1.3 is marked `[x]`.

Quality method (verification steps):

    set -o pipefail && make check-fmt 2>&1 | tee /tmp/ddlint-check-fmt.log
    set -o pipefail && make lint 2>&1 | tee /tmp/ddlint-lint.log
    set -o pipefail && make test 2>&1 | tee /tmp/ddlint-test.log
    set -o pipefail && make markdownlint 2>&1 | tee /tmp/ddlint-markdownlint.log

The new tests verify:

- An empty store reports `len() == 0`, `is_empty() == true`, and
  `rules_for_kind` returns `&[]` for any kind.
- Registering a single rule with one target kind makes it retrievable via
  `rules_for_kind`.
- A rule targeting multiple kinds appears under each kind but only once in
  `all_rules`.
- Multiple rules targeting the same kind are all returned by
  `rules_for_kind`.
- Insertion order is preserved in `rules_for_kind`.
- A rule with empty `target_kinds` appears in `all_rules` but not in any kind
  entry.
- Method chaining via `register` works.
- `CstRuleStore` is `Send + Sync` (compile-time assertion).
- Rule metadata (`name`, `group`, `docs`) is accessible through `Arc<dyn
  CstRule>`.
- (Behavioural) Store-based dispatch over a parsed DDlog file produces the same
  diagnostics as the manual `target_kinds().contains()` loop.
- (Behavioural) Multiple rules with overlapping kinds are all dispatched.
- (Behavioural) An empty store produces no matches for any element.
- (Behavioural) Store dispatch works with `RuleCtx` carrying configuration.

## Idempotence and recovery

All steps are idempotent. Creating or overwriting `store.rs` and
`linter_rule_store.rs` is safe. Edits to `mod.rs`, `ddlint-design.md`, and
`roadmap.md` are additive and can be re-applied. Quality gate commands are
read-only checks. If a step fails, fix the issue and re-run from that step.

## Artifacts and notes

### Stub rule for unit tests

A minimal test helper used across unit tests:

    struct StubRule {
        name: &'static str,
        kinds: &'static [SyntaxKind],
    }

    impl Rule for StubRule {
        fn name(&self) -> &'static str { self.name }
        fn group(&self) -> &'static str { "test" }
        fn docs(&self) -> &'static str { "Stub rule for testing." }
    }

    impl CstRule for StubRule {
        fn target_kinds(&self) -> &'static [SyntaxKind] { self.kinds }
    }

### Store-based dispatch helper for behavioural tests

A preview of the dispatch pattern that `3.1.4` will formalize:

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

## Interfaces and dependencies

No new external dependencies. Uses only:

- `std::collections::HashMap`
- `std::sync::Arc`
- `crate::SyntaxKind` (from `src/language.rs`)
- `crate::linter::CstRule` (from `src/linter/rule.rs`)

The public interface added by this milestone in `src/linter/store.rs`:

    pub struct CstRuleStore {
        rules: Vec<Arc<dyn CstRule>>,
        by_kind: HashMap<SyntaxKind, Vec<Arc<dyn CstRule>>>,
    }

    impl CstRuleStore {
        pub fn new() -> Self;
        pub fn register(&mut self, rule: Box<dyn CstRule>) -> &mut Self;
        pub fn rules_for_kind(&self, kind: SyntaxKind) -> &[Arc<dyn CstRule>];
        pub fn all_rules(&self) -> &[Arc<dyn CstRule>];
        pub fn len(&self) -> usize;
        pub fn is_empty(&self) -> bool;
    }

    impl Default for CstRuleStore { … }

Files modified:

- `src/linter/store.rs` (new): struct, impl, unit tests.
- `src/linter/mod.rs` (edit): add `mod store;` and re-export.
- `tests/linter_rule_store.rs` (new): behavioural tests.
- `docs/ddlint-design.md` (edit): add `CstRuleStore` contract to section 3.1.
- `docs/roadmap.md` (edit): mark 3.1.3 done.
- `docs/execplans/3-1-3-cst-rule-store.md` (new): this ExecPlan.
