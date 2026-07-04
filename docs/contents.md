# Documentation contents

- [Documentation contents](./contents.md): Index for the current documentation
  set and the recommended starting point for repository orientation.

## Core project documents

- [Changelog](../CHANGELOG.md): User-facing release notes and migration
  highlights.
- [Repository layout](./repository-layout.md): Map of the repository tree,
  path responsibilities, and placement rules for contributors.
- [Users' guide](./users-guide.md): User-facing guide for applying `ddlint`
  today.
- [Documentation style guide](./documentation-style-guide.md): Authoring
  conventions for Markdown structure, spelling, formatting, diagrams, roadmaps,
  Request for Comments (RFCs), and Architectural Decision Records (ADRs).
- [Developer's guide](./developers-guide.md): Maintainer-facing workflow and
  implementation guidance for the current codebase.
- [ddlint design](./ddlint-design.md): Primary design document for the `ddlint`
  parser, semantic analysis, and linter architecture.
- [Roadmap](./roadmap.md): Phase-based delivery plan and task sequencing for
  parser foundations, linting, and editor integration.

## Parser and linter references

- [Differential Datalog parser syntax specification](./differential-datalog-parser-syntax-spec-updated.md):
  Normative reference for Differential Datalog (DDlog) syntax, lexer rules,
  operator precedence, and parser desugarings.
- [Parser implementation notes](./parser-implementation-notes.md):
  Non-normative parser implementation invariants, architecture details, and
  cross-module utility notes.
- [Parser conformance register](./parser-conformance-register.md): Tracker for
  specification and implementation deltas, resolved contradictions, and open
  parser design decisions.
- [Haskell parser analysis](./haskell-parser-analysis.md): Reference analysis
  of the upstream DDlog parser behaviour used when validating parser
  compatibility.

## Testing and implementation guides

- [Building an error-recovering parser with Chumsky](./building-an-error-recovering-parser-with-chumsky.md):
  Practical guidance for resilient Chumsky parsers, recovery patterns, and
  common pitfalls.
- [Complexity antipatterns and refactoring strategies](./complexity-antipatterns-and-refactoring-strategies.md):
  Reference on identifying complexity smells and applying maintainable
  refactoring approaches.
- [Reliable testing in Rust via dependency injection](./reliable-testing-in-rust-via-dependency-injection.md):
  Testing patterns that use dependency injection for deterministic and
  parallel-safe tests.
- [Rust doctest DRY guide](./rust-doctest-dry-guide.md): Guidance on reusable
  Rust doctests and `rustdoc` execution behaviour.
- [Rust parser testing comprehensive guide](./rust-parser-testing-comprehensive-guide.md):
  End-to-end strategy for Rust parser testing with `logos`, `chumsky`, and
  `rowan`.
- [Rust testing with rstest fixtures](./rust-testing-with-rstest-fixtures.md):
  Practical guide to structuring Rust tests with `rstest` fixtures and
  parameterized cases.
- [Scripting standards](./scripting-standards.md): Standards for project
  scripts covering tooling defaults, structure, and reproducibility.

## Decision records

- [Architectural decision record 001: parser crate split for multi-consumer use](./adr-001-parser-crate-split.md):
  Proposed decision record for parser crate boundaries and implementation
  structure.

## Execution plans

- [Execution plans](./execplans/): Directory of living plans for non-trivial
  implementation work.
- [2.5.4 top-level `for` desugaring](./execplans/2-5-4-top-level-for-desugaring.md):
  Plan for closing top-level `for` desugaring behaviour.
- [2.6.1 close the top-level `for` decision and align scanner](./execplans/2-6-1-close-the-top-level-for-decision-and-align-scanner.md):
  Plan for scanner alignment after the top-level `for` decision.
- [2.6.2 decide the aggregation extraction boundary](./execplans/2-6-2-decide-the-aggregation-extraction-boundary.md):
  Plan for aggregation extraction ownership.
- [2.6.3 decide collection literal lowering stage ownership](./execplans/2-6-3-decide-collection-literal-lowering-stage-ownership.md):
  Plan for collection literal lowering ownership.
- [2.6.4 align index declaration grammar](./execplans/2-6-4-align-index-declaration-grammar.md):
  Plan for index declaration grammar alignment.
- [2.6.5 align transformer declaration grammar](./execplans/2-6-5-align-transformer-declaration-grammar.md):
  Plan for transformer declaration grammar alignment.
- [2.6.7 finalize legacy token compatibility policy](./execplans/2-6-7-finalize-legacy-token-compatibility-policy.md):
  Plan for closing the legacy-token compatibility policy.
- [3.1.1 core rule and CST rule traits](./execplans/3-1-1-core-rule-and-cst-rule-traits.md):
  Plan for lint rule trait foundations.
- [3.1.2 rule context struct](./execplans/3-1-2-rule-context-struct.md):
  Plan for lint rule context modelling.
- [3.1.3 CST rule store](./execplans/3-1-3-cst-rule-store.md): Plan for the
  concrete syntax tree (CST) rule registry.
- [3.1.4 visitor-based parallel rule runner](./execplans/3-1-4-visitor-based-parallel-rule-runner.md):
  Plan for parallel lint rule execution.
- [3.2.1 declare lint macro](./execplans/3-2-1-declare-lint-macro.md): Plan for
  declarative lint rule registration.
- [3.3.1 symbol table and scope resolution](./execplans/3-3-1-symbol-table-and-scope-resolution.md):
  Plan for semantic scope and symbol table support.
- [4.1.1 implement unused relation diagnostics](./execplans/4-1-1-implement-unused-relation-diagnostics.md):
  Plan for unused relation lint diagnostics.
- [4.1.2 implement unused variable diagnostics](./execplans/4-1-2-implement-unused-variable-diagnostics.md):
  Plan for unused variable lint diagnostics.
- [Phase 2: attribute placement validators](./execplans/phase-2-attribute-placement-validators.md):
  Plan for validating attribute placement.
- [Phase 2: enforce qualified call rule](./execplans/phase-2-enforce-qualified-call-rule.md):
  Plan for enforcing qualified call syntax rules.
- [Phase 2: parse apply items](./execplans/phase-2-parse-apply-items.md): Plan
  for apply item parsing.

## Archived parser history

- [Parser gap analysis](./archive/parser-gap-analysis.md): Historical gap
  analysis retained for traceability.
- [Parser syntax specification migration plan](./archive/differential-datalog-parser-syntax-spec-migration-plan.md):
  Historical migration plan for the parser syntax specification.
- [Pratt parser design](./archive/pratt-parser-for-ddlog-expressions.md):
  Historical Pratt parser design note.
- [Function parsing design](./archive/function-parsing-design.md): Historical
  function parsing design note.
- [Parser porting plan](./archive/parser-plan.md): Historical parser porting
  plan.
- [Parser documentation analysis](./archive/documentation-analysis-parser-design.md):
  Historical analysis of parser documentation structure.
