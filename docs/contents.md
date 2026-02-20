# Documentation contents

This page lists the current documents in the `docs/` directory.

## Core documents

- [Building an error-recovering parser with Chumsky](./building-an-error-recovering-parser-with-chumsky.md):
  Practical guidance for implementing resilient Chumsky parsers, including
  recovery patterns and common pitfalls.
- [Complexity antipatterns and refactoring strategies](./complexity-antipatterns-and-refactoring-strategies.md):
  Reference on identifying complexity smells and applying maintainable
  refactoring approaches.
- [ddlint design](./ddlint-design.md): Core architecture and implementation
  roadmap for the `ddlint` linter.
- [Documentation style guide](./documentation-style-guide.md): Authoring
  conventions for Markdown structure, spelling, formatting, and ADR content.
- [Haskell parser analysis](./haskell-parser-analysis.md): Technical summary of
  the legacy Haskell parser to inform Rust porting work.
- [Reliable testing in Rust via dependency injection](./reliable-testing-in-rust-via-dependency-injection.md):
  Testing patterns that use dependency injection to improve determinism and
  parallel test safety.
- [Roadmap](./roadmap.md): Phase-based delivery plan for `ddlint` from parser
  foundations to lints and editor integration.
- [Rust doctest dry guide](./rust-doctest-dry-guide.md): Guidance on writing
  reusable, maintainable Rust doctests and understanding `rustdoc` execution.
- [Rust parser testing comprehensive guide](./rust-parser-testing-comprehensive-guide.md):
  End-to-end testing strategy for Rust parser stacks built with `logos`,
  `chumsky`, and `rowan`.
- [Rust testing with rstest fixtures](./rust-testing-with-rstest-fixtures.md):
  Practical guide to structuring Rust tests with `rstest` fixtures and
  parameterized cases.
- [Scripting standards](./scripting-standards.md): Standards for project
  scripts covering tooling defaults, structure, and reproducibility.

## Active parser documentation

- [Differential Datalog parser syntax specification updated](./differential-datalog-parser-syntax-spec-updated.md):
  Normative reference for DDlog syntax, lexer rules, operator precedence, and
  parser desugarings.
- [Parser implementation notes](./parser-implementation-notes.md):
  Non-normative implementation invariants, parser architecture details, and
  cross-module parser utility notes.
- [Parser conformance register](./parser-conformance-register.md):
  Source-of-truth tracker for spec/code deltas, resolved contradictions, and
  open parser design decisions.

## Archived parser history

- [Parser gap analysis (archived)](./archive/parser-gap-analysis.md)
- [Parser syntax-spec migration plan (archived)](./archive/differential-datalog-parser-syntax-spec-migration-plan.md)
- [Pratt parser design (archived)](./archive/pratt-parser-for-ddlog-expressions.md)
- [Function parsing design (archived)](./archive/function-parsing-design.md)
- [Parser porting plan (archived)](./archive/parser-plan.md)
- [Parser documentation analysis (archived)](./archive/documentation-analysis-parser-design.md)

## Execution plans

- [Phase 2: attribute placement validators](./execplans/phase-2-attribute-placement-validators.md)
- [Phase 2: enforce qualified call rule](./execplans/phase-2-enforce-qualified-call-rule.md)
- [Phase 2: parse apply items](./execplans/phase-2-parse-apply-items.md)
