# Implement `declare_lint!` for CST lint rules (roadmap 3.2.1)

This ExecPlan is a living document. The sections `Constraints`, `Tolerances`,
`Risks`, `Progress`, `Surprises & Discoveries`, `Decision Log`, and
`Outcomes & Retrospective` must be kept up to date as work proceeds.

Status: COMPLETE

## Purpose / big picture

Roadmap item `3.2.1` exists to make rule authoring materially cheaper than the
current handwritten pattern in `src/linter/rule.rs`,
`tests/linter_rule_store.rs`, and `tests/linter_runner/main.rs`, where every
rule repeats the same metadata methods and most of the same `CstRule`
scaffolding. After this change, a contributor should be able to declare a
zero-state lint rule with one `declare_lint!` invocation that generates the
rule struct, rule metadata, default level, target-kind registration, and
optional node/token handlers.

Observable success is:

- `ddlint::declare_lint!` is a public declarative macro available to internal
  modules and integration tests.
- A macro-generated rule implements `Rule` and `CstRule`, can be registered in
  `CstRuleStore`, and emits diagnostics through `Runner` without handwritten
  trait impls.
- Rule metadata includes `name`, `group`, `docs`, and a first-class default
  level aligned with the design document's severity model.
- Unit tests cover macro expansion behaviour for metadata, doc extraction,
  default level, target kinds, and generated node/token handlers.
- Behavioural tests prove that a macro-generated rule works end-to-end through
  `CstRuleStore` and `Runner` on parsed DDlog fixtures.
- `docs/ddlint-design.md` section `3.2` documents the final macro syntax and
  any design adjustments taken during implementation.
- `docs/roadmap.md` replaces stale `docs/ddlint-design-and-road-map.md`
  references with `docs/ddlint-design.md` where applicable and marks `3.2.1`
  done once all quality gates pass.
- `make check-fmt`, `make lint`, and `make test` all succeed at the end of the
  feature.

## Constraints

- Treat [docs/ddlint-design.md](docs/ddlint-design.md) section `3.2` as the
  normative design source for this milestone. The roadmap still references
  `docs/ddlint-design-and-road-map.md`, but that file does not exist in the
  current repository.
- Keep scope limited to roadmap item `3.2.1`. Do not implement CLI commands,
  configuration-file parsing, semantic-analysis passes, or concrete production
  lint rules from phase `4`.
- Keep `CstRuleStore` and `Runner` semantics unchanged. This milestone is about
  rule authoring ergonomics, not a new dispatch engine.
- Use a declarative `macro_rules!` macro, not a new procedural-macro crate.
- Do not add new dependencies.
- Keep any metadata API additions additive. Existing handwritten rules and
  tests must continue to compile without semantic changes unless explicitly
  updated to exercise the new API.
- Every new Rust module must start with a `//!` module comment.
- Keep files below 400 lines by splitting unit tests or helpers when needed.
- Validate with both unit tests and behavioural tests.
- Record final design decisions in
  [docs/ddlint-design.md](docs/ddlint-design.md).
- When editing [docs/roadmap.md](docs/roadmap.md) for this milestone, also fix
  stale references to the missing `docs/ddlint-design-and-road-map.md` file.
- Use Make targets for final quality gates and run every gate with
  `set -o pipefail` and `tee`.
- Use en-GB-oxendict spelling in comments and documentation.

## Tolerances (exception triggers)

- Scope: if implementation requires changes to more than 8 files or more than
  500 net new lines, stop and escalate.
- Interface: if the macro cannot be delivered without a non-additive break to
  `Rule`, `CstRule`, `CstRuleStore`, or `Runner`, stop and escalate.
- Macro model: if `macro_rules!` cannot express the agreed syntax cleanly and a
  procedural macro appears necessary, stop and escalate.
- Ambiguity: if the repository already depends on a different severity model
  than the one described here, stop and reconcile the conflict in
  `Decision Log` before coding further.
- Iterations: if tests or Clippy still fail after three focused fix cycles,
  stop and escalate with the failing test names or lint IDs.

## Risks

- Risk: declarative macros cannot transform `PascalCase` type names into the
  required kebab-case rule identifier. Severity: medium. Likelihood: high.
  Mitigation: require an explicit `name: "kebab-case"` field in the macro input
  and document why this is required.

- Risk: doc-comment capture may preserve unexpected whitespace or omit blank
  lines, making `docs()` output awkward or unstable. Severity: medium.
  Likelihood: medium. Mitigation: capture `#[doc = "..."]` attributes directly,
  assemble the output with `concat!`, and assert on representative
  multi-paragraph docs in unit tests.

- Risk: exporting the macro from the wrong module will make external use
  awkward or impossible. Severity: medium. Likelihood: medium. Mitigation:
  define the macro in a dedicated [src/linter/macros.rs](src/linter/macros.rs)
  module and export it with `#[macro_export]`, making `ddlint::declare_lint!`
  the stable public path.

- Risk: adding first-class default-level metadata could ripple through existing
  tests and future API expectations. Severity: medium. Likelihood: medium.
  Mitigation: add the new metadata as an additive API with a sensible default
  on `Rule`, then update only the tests that should assert on specific levels.

- Risk: macro-generated handler bodies may trip strict Clippy rules in tests
  and examples. Severity: low. Likelihood: medium. Mitigation: keep generated
  examples iterator-based, avoid indexing, and use the same lint-safe assertion
  patterns already established in the `3.1.*` milestones.

## Progress

- [x] (2026-03-07 00:00Z) Review roadmap item `3.2.1`, the `execplans` skill,
  the existing linter engine implementation, and prior linter execplans.
- [x] (2026-03-07 00:00Z) Write this ExecPlan to
  `docs/execplans/3-2-1-declare-lint-macro.md`.
- [x] (2026-03-07T10:44Z) Add first-class rule-level metadata in
  `src/linter/rule.rs`.
- [x] (2026-03-07T10:44Z) Add the public `declare_lint!` macro module and
  export path.
- [x] (2026-03-07T10:44Z) Add unit tests for macro metadata, doc extraction,
  target kinds, and generated handlers.
- [x] (2026-03-07T10:44Z) Add behavioural tests proving store and runner
  compatibility for a macro-generated rule.
- [x] (2026-03-07T10:44Z) Update `docs/ddlint-design.md` section `3.2` with
  the final macro contract and implemented syntax.
- [x] (2026-03-07T10:44Z) Run `make fmt`, `make markdownlint`, `make nixie`,
  `make check-fmt`, `make lint`, and `make test`.
- [x] (2026-03-07T10:44Z) Update stale
  `docs/ddlint-design-and-road-map.md` references in `docs/roadmap.md` and mark
  item `3.2.1` as done.

## Surprises & Discoveries

- Observation: the roadmap points to `docs/ddlint-design-and-road-map.md`, but
  the actual design document present in the repository is
  [docs/ddlint-design.md](docs/ddlint-design.md). Evidence:
  `rg --files docs | rg 'ddlint-design|road-map|roadmap'` returned only
  `docs/ddlint-design.md` and `docs/roadmap.md`. Impact: this ExecPlan, and the
  implementation that follows it, must treat `docs/ddlint-design.md` as the
  authoritative design source.

- Observation: the current `Rule` trait exposes `name`, `group`, and `docs`,
  but no severity or level metadata. Evidence:
  [src/linter/rule.rs](src/linter/rule.rs) defines only those three metadata
  methods. Impact: implementing the macro exactly as described in the design
  document requires an additive metadata extension instead of a pure macro-only
  change.

- Observation: standard Rust doc comments become `#[doc = " ..."]` attributes
  with a leading space on non-empty lines, and stable `macro_rules!` cannot
  trim that whitespace while still returning `&'static str`. Evidence: the
  initial exact-string unit test for `docs()` failed with leading spaces in
  headings and paragraphs captured from the doc comments. Impact: the design
  doc now states that `docs()` is sourced directly from doc comments, and tests
  assert on meaningful content rather than byte-for-byte whitespace.

## Decision Log

- Decision: treat [docs/ddlint-design.md](docs/ddlint-design.md) as the
  normative design document for this milestone and plan to repair stale roadmap
  references during implementation. Rationale: the roadmap path is stale, while
  `docs/ddlint-design.md` contains the actual `declare_lint!` design text in
  section `3.2`. Fixing the roadmap in the same change removes an already-known
  source of confusion for the next milestone. Date/Author: 2026-03-07 / Codex.

- Decision: extend `Rule` with a first-class default-level API for this
  milestone. Rationale: the design text for `declare_lint!`, the initial rule
  catalogue, and the future CLI/configuration sections all depend on rule-level
  metadata. Deferring level support would force the macro to invent private
  metadata that later phases could not read through the `Rule` trait.
  Date/Author: 2026-03-07 / Codex.

- Decision: require the macro caller to provide an explicit kebab-case `name`
  literal instead of deriving it from the rule struct name. Rationale: stable
  declarative macros cannot reliably convert `PascalCase` identifiers into
  kebab-case strings. An explicit name keeps the macro simple and avoids hidden
  string-munging rules. Date/Author: 2026-03-07 / Codex.

- Decision: have `declare_lint!` generate both the `Rule` impl and the
  `CstRule` impl, with optional `check_node` and `check_token` bodies.
  Rationale: metadata-only expansion would leave too much of the repetitive
  `CstRule` boilerplate in place and would not satisfy the roadmap's stated
  goal of reducing rule-definition ceremony. Date/Author: 2026-03-07 / Codex.

- Decision: export the macro as `ddlint::declare_lint!`.
  Rationale: `#[macro_export]` gives the crate a stable, external-facing macro
  path that works in integration tests and future downstream consumers without
  additional helper crates. Date/Author: 2026-03-07 / Codex.

- Decision: keep the macro grammar explicit, with four supported shapes: no
  handlers, node-only, token-only, or both handlers. Rationale: attempting a
  more permissive optional-handler grammar caused local ambiguity in
  `macro_rules!`. Explicit arms keep parse errors predictable and the public
  syntax easy to document. Date/Author: 2026-03-07 / Codex.

- Decision: capture `&self` as an identifier in handler signatures.
  Rationale: macro hygiene prevents a handler body from referring to a literal
  `self` unless that identifier is captured from the macro invocation and
  reused in the generated method signature. Date/Author: 2026-03-07 / Codex.

- Decision: keep `docs/roadmap.md` item `3.2.2` unchecked in this milestone.
  Rationale: the behavioural tests added here will prove compatibility with the
  existing store and runner, but the requested roadmap task is specifically
  `3.2.1`. Marking only `3.2.1` done keeps the roadmap history faithful to the
  user request. Date/Author: 2026-03-07 / Codex.

## Outcomes & Retrospective

The milestone shipped as planned:

- `RuleLevel` and `Rule::default_level()` now provide first-class rule-level
  metadata without breaking existing handwritten rules.
- `ddlint::declare_lint!` is implemented in `src/linter/macros.rs` and
  generates a zero-state rule struct plus `Rule` and `CstRule` impls.
- Unit coverage lives in `src/linter/macros.rs` and
  `src/linter/rule/tests.rs`.
- Behavioural coverage lives in `tests/linter_declare_lint_macro.rs` and
  proves store and runner compatibility end-to-end.
- `docs/ddlint-design.md` now documents the implemented macro syntax, and
  `docs/roadmap.md` now points to `docs/ddlint-design.md` consistently while
  marking `3.2.1` done.
- All required gates passed:
  `make fmt`, `make markdownlint`, `make nixie`, `make check-fmt`, `make lint`,
  and `make test`.

The most useful lesson was that severity metadata needed to be solved at the
trait level, not hidden inside the macro. The other significant lesson was
macro hygiene: capturing `&self` from the call site is necessary when the
handler body wants to call `self.name()` naturally.

## Context and orientation

The current linter engine lives in [src/linter](src/linter):

- [src/linter/rule.rs](src/linter/rule.rs) defines `LintDiagnostic`,
  `RuleConfigValue`, `RuleConfig`, `RuleCtx`, the `Rule` trait, the `CstRule`
  trait, and unit tests for those primitives.
- [src/linter/store.rs](src/linter/store.rs) defines `CstRuleStore`, which
  indexes `Arc<dyn CstRule>` by `SyntaxKind`.
- [src/linter/runner.rs](src/linter/runner.rs) defines `Runner`, which walks
  the parsed CST and invokes matching rules in parallel.
- [src/linter/mod.rs](src/linter/mod.rs) wires those modules together and
  re-exports the current public linter API.

The crate root is [src/lib.rs](src/lib.rs). No public linter macro is exported
today. Existing macro usage in the repository is limited to private helper
macros in parser and test utilities.

Rule definitions in tests currently show the boilerplate this milestone is
meant to remove:

- [tests/linter_rule_store.rs](tests/linter_rule_store.rs) hand-writes several
  `Rule` and `CstRule` impls just to test dispatch.
- [tests/linter_runner/main.rs](tests/linter_runner/main.rs) depends on
  similar handwritten rules to exercise end-to-end runner behaviour.

For this plan, "default level" means the rule's built-in severity before a
future configuration file overrides it. "Zero-state rule" means a rule with no
fields; it is just a type that carries metadata and behaviour.

## Plan of work

### Stage A: add the missing metadata contract first

Edit [src/linter/rule.rs](src/linter/rule.rs) to introduce a small `RuleLevel`
enum for the severity values already named in
[docs/ddlint-design.md](docs/ddlint-design.md): `allow`, `hint`, `warn`, and
`error`. Re-export this type from [src/linter/mod.rs](src/linter/mod.rs).

Add a new `fn default_level(&self) -> RuleLevel` method to `Rule` with a
default implementation so existing handwritten rules continue to compile
without immediate edits. Keep the method purely additive and document it with
Rustdoc. Add unit tests in `src/linter/rule.rs` confirming the default method
returns the chosen default for a handwritten rule and that explicit overrides
work when a rule opts in.

Go/no-go check: if adding `RuleLevel` forces non-additive changes to the
runner, store, or parser, stop and escalate before implementing the macro.

### Stage B: add the macro module and write the red tests

Create [src/linter/macros.rs](src/linter/macros.rs) with a module-level `//!`
comment and a `#[macro_export] macro_rules! declare_lint` definition. Wire the
module through [src/linter/mod.rs](src/linter/mod.rs) so the file is compiled
as part of the crate.

The macro syntax should be explicit and stable. Implement this shape:

```rust
ddlint::declare_lint! {
    /// Multi-line rule documentation.
    pub ExampleRule {
        name: "example-rule",
        group: "correctness",
        level: warn,
        target_kinds: [SyntaxKind::N_RULE, SyntaxKind::T_IDENT],
        fn check_node(
            &self,
            node: &rowan::SyntaxNode<ddlint::DdlogLanguage>,
            ctx: &ddlint::linter::RuleCtx,
            diagnostics: &mut Vec<ddlint::linter::LintDiagnostic>,
        ) {
            let _ = ctx;
            diagnostics.push(ddlint::linter::LintDiagnostic::new(
                self.name(),
                "node hit",
                node.text_range(),
            ));
        }
        fn check_token(
            &self,
            token: &rowan::SyntaxToken<ddlint::DdlogLanguage>,
            ctx: &ddlint::linter::RuleCtx,
            diagnostics: &mut Vec<ddlint::linter::LintDiagnostic>,
        ) {
            let _ = ctx;
            diagnostics.push(ddlint::linter::LintDiagnostic::new(
                self.name(),
                "token hit",
                token.text_range(),
            ));
        }
    }
}
```

The macro should expand to:

- a zero-sized public struct;
- an `impl Rule` returning the provided metadata and captured doc text;
- an `impl CstRule` returning the provided target kinds;
- optional generated `check_node` and `check_token` methods only when the
  caller supplies them, otherwise relying on the trait's default no-op methods.

Before filling in the macro, add unit tests that describe this contract and
initially fail. Keep them close to the macro implementation, either in
`src/linter/macros.rs` under `#[cfg(test)]` or in a dedicated
`src/linter/macros/tests.rs` child module if the file approaches 400 lines.

These red tests should cover:

- metadata methods on a macro-generated rule;
- doc extraction preserving headings, blank lines, and examples;
- target-kind registration for single-kind and multi-kind rules;
- omission of `check_token` or `check_node` still compiling and behaving as a
  no-op;
- explicit `level: allow | hint | warn | error` mapping to `RuleLevel`.

### Stage C: prove end-to-end compatibility with the existing engine

Add a behavioural test file such as
[tests/linter_declare_lint_macro.rs](tests/linter_declare_lint_macro.rs). Use
`ddlint::declare_lint!` to define one or two test-only rules in the test
module, parse an existing fixture such as
[examples/hello_join.dl](examples/hello_join.dl), and run the rules through
both `CstRuleStore` and `Runner`.

The behavioural suite should prove these outcomes:

- a macro-generated node rule can be registered in `CstRuleStore` and emits a
  diagnostic at runtime;
- a macro-generated token rule also works end-to-end;
- a rule with both handlers still produces deterministic ordering through
  `Runner`;
- `docs()` and `default_level()` remain available through a `dyn Rule` or
  `dyn CstRule` trait object.

Use the existing behavioural suites in
[tests/linter_rule_store.rs](tests/linter_rule_store.rs) and
[tests/linter_runner/main.rs](tests/linter_runner/main.rs) as reference
patterns. Do not rewrite those files unless a small shared helper extraction is
needed to stay within file-size limits.

### Stage D: document the final contract and close the roadmap item

Update [docs/ddlint-design.md](docs/ddlint-design.md) section `3.2` to reflect
the actual implemented macro shape. The current example names a struct, group,
and default severity conceptually, but the final document must also explain:

- that the caller provides an explicit kebab-case `name`;
- that the macro expands into both `Rule` and `CstRule`;
- that the exported path is `ddlint::declare_lint!`;
- that default level is now a first-class metadata concept available on
  `Rule`.

Then update stale `docs/ddlint-design-and-road-map.md` references in
[docs/roadmap.md](docs/roadmap.md) so they point at `docs/ddlint-design.md`.
Mark only roadmap item `3.2.1` as done and leave `3.2.2` unchanged.

Finish by running formatting, Markdown validation, Rust formatting checks,
Clippy, and the full test suite. Do not consider the milestone complete until
all gates pass.

## Concrete steps

Work from the repository root: `/home/user/project`.

1. Add the new unit tests before the macro implementation and run a focused red
   loop:

   ```bash
   set -o pipefail; \
   RUSTFLAGS="-D warnings" cargo test --all-targets --all-features declare_lint \
     2>&1 | tee /tmp/3-2-1-declare-lint-red.log
   ```

   Expect the run to fail before implementation, likely with errors mentioning
   missing `declare_lint!`, missing `RuleLevel`, or assertions against
   incomplete macro output.

2. Implement `RuleLevel`, `Rule::default_level`, and the macro, then rerun the
   focused loop:

   ```bash
   set -o pipefail; \
   RUSTFLAGS="-D warnings" cargo test --all-targets --all-features declare_lint \
     2>&1 | tee /tmp/3-2-1-declare-lint-green.log
   ```

   Expect the targeted macro tests to pass. A concise success transcript should
   look similar to:

   ```plaintext
   running <N> tests
   test ...declare_lint... ok
   test result: ok. <N> passed; 0 failed
   ```

3. Run the documentation gates required by this repository:

   ```bash
   set -o pipefail; make fmt 2>&1 | tee /tmp/3-2-1-declare-lint-fmt.log
   set -o pipefail; make markdownlint 2>&1 | tee /tmp/3-2-1-declare-lint-markdownlint.log
   set -o pipefail; make nixie 2>&1 | tee /tmp/3-2-1-declare-lint-nixie.log
   ```

4. Run the final Rust quality gates:

   ```bash
   set -o pipefail; make check-fmt 2>&1 | tee /tmp/3-2-1-declare-lint-check-fmt.log
   set -o pipefail; make lint 2>&1 | tee /tmp/3-2-1-declare-lint-lint.log
   set -o pipefail; make test 2>&1 | tee /tmp/3-2-1-declare-lint-test.log
   ```

5. Review the logs for the specific evidence this milestone needs:

   ```bash
   rg -n "declare_lint|RuleLevel|test result: ok|0 failed" \
     /tmp/3-2-1-declare-lint-*.log
   ```

## Validation and acceptance

The feature is done when all of the following are true:

- A developer can write a rule with one `ddlint::declare_lint!` invocation and
  no handwritten `impl Rule` or `impl CstRule`.
- The generated rule exposes `name()`, `group()`, `docs()`, and
  `default_level()` through the `Rule` trait.
- The generated rule exposes `target_kinds()` and any provided handler bodies
  through the `CstRule` trait.
- A macro-generated rule can be registered in `CstRuleStore` and run through
  `Runner`, producing deterministic diagnostics in behavioural tests.
- `docs/ddlint-design.md` matches the implemented syntax and
  `docs/roadmap.md` marks `3.2.1` done.

Quality criteria:

- Tests: the new unit and behavioural tests pass, and the full `make test`
  suite passes.
- Lint/typecheck: `make check-fmt` and `make lint` pass with no warnings.
- Documentation: `make fmt`, `make markdownlint`, and `make nixie` pass after
  the plan and design-doc updates.

## Idempotence and recovery

All steps in this plan are additive and safe to rerun. If the focused red or
green test loop fails, inspect the corresponding log under `/tmp/` and rerun
the same command after the next edit. If the macro syntax proves too awkward,
keep the tests and documentation changes, record the blocking detail in
`Decision Log`, and stop at the tolerance gate instead of inventing a more
powerful macro system.

If formatting or Markdown validation changes files unexpectedly, rerun
`make fmt`, inspect the diff, and then repeat the validation commands. Do not
edit generated formatting by hand unless the formatter cannot express the
required layout.

## Artifacts and notes

The most important implementation artifact should be a compact macro example in
`docs/ddlint-design.md` that mirrors the final public API. Keep it small enough
to read in a terminal. A representative final example should resemble:

```rust
use ddlint::{SyntaxKind, declare_lint};
use ddlint::linter::{LintDiagnostic, RuleCtx};

declare_lint! {
    /// Flags every relation declaration.
    pub RelationExample {
        name: "relation-example",
        group: "style",
        level: warn,
        target_kinds: [SyntaxKind::N_RELATION_DECL],
        fn check_node(
            &self,
            node: &rowan::SyntaxNode<ddlint::DdlogLanguage>,
            _ctx: &RuleCtx,
            diagnostics: &mut Vec<LintDiagnostic>,
        ) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "relation declaration seen",
                node.text_range(),
            ));
        }
    }
}
```

## Interfaces and dependencies

No new dependencies are allowed.

The implementation should leave these public interfaces in place at the end of
the milestone:

```rust
pub enum RuleLevel {
    Allow,
    Hint,
    Warn,
    Error,
}

pub trait Rule {
    fn name(&self) -> &'static str;
    fn group(&self) -> &'static str;
    fn docs(&self) -> &'static str;
    fn default_level(&self) -> RuleLevel { RuleLevel::Warn }
}
```

The macro should be exported at crate root:

```rust
ddlint::declare_lint! { /* ... */ }
```

The generated rule type must satisfy:

- `Rule`
- `CstRule`
- `Send + Sync`
- zero runtime state unless the caller later extends the pattern manually

Revision note: Completed implementation on 2026-03-07. The live plan now
records the shipped `RuleLevel` API, the explicit-arm macro grammar, the
captured-`&self` handler design, the direct `docs/roadmap.md` reference fix,
and the fact that all required gates passed.
