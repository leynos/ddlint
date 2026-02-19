# Architectural decision record (ADR) 001: parser crate split for multi-consumer use

## Status

Proposed.

## Date

2026-02-18.

## Context and problem statement

The current parser implementation is embedded in the `ddlint` crate and exports
APIs that primarily suit a linter-first, Concrete Syntax Tree (CST)-centric
workflow. The project now has two consumers with materially different needs:

- `ddlint`, which needs lossless syntax, trivia preservation, and precise spans
  for diagnostics and future autofix.
- `telephone`, which needs a stable, compiler-facing semantic model that can
  drive dependency extraction, lowering, canonical planning, and compilation
  cache keys.

Keeping both concerns behind one implicit surface has already introduced
friction:

- public APIs are not clearly partitioned by consumer role,
- compiler-facing access still relies on string extraction and re-parsing in
  places, and
- diagnostics are transportable, but not yet stable as a formal contract.

This ADR proposes a crate split and migration plan that makes the parser a
first-class library with explicit layers and versioned contracts.

## Decision drivers

- Support two first-class consumers with different correctness and ergonomics
  requirements.
- Preserve lossless CST behaviour and spans for `ddlint`.
- Provide a typed semantic model for compiler consumers without CST reparsing.
- Establish stable API and diagnostics contracts suitable for reuse.
- Reduce coupling between lint-only utilities and compiler-only concerns.
- Enable incremental adoption with low migration risk.

## Requirements

### Functional requirements

- Expose a lossless parse surface that preserves trivia and source provenance.
- Expose a semantic program model suitable for compiler lowering and dependency
  extraction.
- Provide parse and semantic validation entrypoints as separate stages.
- Preserve existing `ddlint` parser behaviours during migration.

### Technical requirements

- Keep crate boundaries explicit and acyclic.
- Define stable diagnostic types (code, message, span, severity, stage).
- Maintain deterministic source mapping for rule heads, body terms, and
  adornments.
- Keep compatibility with workspace quality gates and existing tests.

## Options considered

### Option A: keep parser in `ddlint` and add a larger facade

Retain the current structure and extend `ddlint` exports for `telephone`.

Pros:

- minimal immediate file movement,
- low short-term disruption.

Cons:

- linter and compiler concerns remain coupled,
- API contracts remain implicit,
- higher long-term maintenance and versioning risk.

### Option B: split into dedicated parser crates with explicit layers

Create dedicated crates for syntax/lossless parsing and semantic modelling,
then make `ddlint` and `telephone` depend on those crates.

Pros:

- clear contracts per consumer class,
- better test and release boundaries,
- easier future evolution of compiler interfaces.

Cons:

- migration effort across modules and tests,
- temporary dual-path compatibility work.

### Option C: build a new parser library externally and migrate both consumers

Start a greenfield parser library outside the current workspace and migrate
both projects after parity.

Pros:

- cleanest separation from legacy choices.

Cons:

- highest delivery risk and duplicated effort,
- delayed value for current consumers.

| Topic                     | Option A | Option B | Option C |
| ------------------------- | -------- | -------- | -------- |
| Short-term effort         | Low      | Medium   | High     |
| Contract clarity          | Low      | High     | High     |
| Migration risk            | Medium   | Medium   | High     |
| Long-term maintainability | Medium   | High     | Medium   |
| Time to first value       | High     | High     | Low      |

_Table 1: Trade-offs between parser crate split options._

## Decision outcome / proposed direction

Adopt **Option B**.

Implement a staged crate split inside the workspace and promote the parser into
a first-class library with layered surfaces:

- `ddlog-syntax` (lossless layer):
  tokenization, `SyntaxKind`, CST construction, span/scanner utilities, and
  parse diagnostics tied to source fidelity.
- `ddlog-sema` (semantic layer):
  owned typed program model, semantic validators, and dependency extraction
  inputs for compiler pipelines.
- `ddlog-parser` (compatibility/facade layer):
  high-level entrypoints that coordinate syntax + semantic phases, preserving
  migration compatibility for existing call sites.

`ddlint` will consume `ddlog-syntax` (and selected semantic helpers where
useful), while `telephone` will consume `ddlog-sema` and parser facade
entrypoints intended for compiler workflows.

## Ownership and cross-crate coordination

This split only succeeds if ownership boundaries are explicit and enforced.

- `ddlog-syntax` ownership:
  maintained by the `ddlint` maintainers, with responsibility for CST fidelity,
  token model stability, and source mapping contracts.
- `ddlog-sema` ownership:
  maintained by the `telephone` maintainers, with responsibility for typed
  semantic model evolution, semantic validation, and dependency extraction
  contracts.
- `ddlog-parser` ownership:
  jointly maintained by `ddlint` and `telephone` maintainers as the public
  integration surface and compatibility facade.

Cross-crate change coordination policy:

- Breaking API changes in any crate require an ADR update and migration notes.
- Changes that modify shared contracts (diagnostics, spans, semantic node
  shapes, facade entrypoints) require approval from both consumer maintainer
  groups.
- `ddlog-parser` compatibility shims must include a deprecation target release
  and tracked removal issue.
- `CODEOWNERS` must mirror these crate boundaries so review routing is
  automatic.

## Goals and non-goals

### Goals

- Define a stable parser library contract for both consumers.
- Remove compiler dependence on CST-string reparsing.
- Preserve lossless syntax guarantees for lint use-cases.
- Provide explicit staging between parsing and semantic validation.

### Non-goals

- Rewriting DDlog grammar semantics as part of this split.
- Introducing pliron/egg/egglog planning in the same migration.
- Implementing runtime result caching behaviour changes.

## Migration plan

### Phase 1: establish crate skeletons and compatibility facade

- Create new crates and workspace wiring.
- Move shared lexical and syntax primitives into `ddlog-syntax`.
- Add a facade API in `ddlog-parser` that preserves existing entrypoint shape.

Deliverables:

- compiling workspace with no behavioural regression in parser tests.

Exit criteria:

- `ddlog-syntax`, `ddlog-sema`, and `ddlog-parser` compile in CI.
- Existing top-level parser entrypoints are available via `ddlog-parser`.
- Parser regression suite passes with no net increase in failures.
- `CODEOWNERS` entries exist for all three crates.

### Phase 2: formalize diagnostics contract

- Introduce stable diagnostic types with explicit parse and semantic categories.
- Add conversion layers from internal parser errors to public diagnostics.
- Freeze diagnostic code naming and document compatibility policy.

Deliverables:

- parser diagnostics documentation and contract tests.

Exit criteria:

- 100% of public parser diagnostics are emitted through the stable diagnostic
  types.
- Diagnostic code list is documented and treated as compatibility surface.
- Contract tests cover parse-stage and semantic-stage diagnostics.

### Phase 3: semantic model extraction

- Introduce an owned semantic model in `ddlog-sema`.
- Replace compiler-facing string reparsing paths with typed fields and nodes.
- Add semantic validation and dependency extraction entrypoints.

Deliverables:

- semantic model tests and dependency extraction fixtures.

Exit criteria:

- 100% of compiler-facing semantic reads use `ddlog-sema` typed nodes.
- Zero compiler paths depend on CST-string reparsing for semantic extraction.
- Dependency extraction fixtures are green for representative recursive and
  non-recursive programs.

### Phase 4: consumer migration

- Migrate `ddlint` imports to the syntax-layer APIs.
- Migrate `telephone` integration points to semantic/facade APIs.
- Keep temporary shims only where needed to reduce disruption.

Deliverables:

- both consumers building against new crates without legacy direct imports.

Exit criteria:

- 100% of `ddlint` parser imports resolve through `ddlog-syntax` or
  `ddlog-parser`.
- 100% of `telephone` parser/semantic imports resolve through `ddlog-sema` or
  `ddlog-parser`.
- Temporary shims are limited to `ddlog-parser` and tracked with removal issues.

### Phase 5: deprecation and cleanup

- Remove deprecated compatibility shims.
- Finalize crate-level versioning and change management notes.
- Update roadmap and parser architecture documentation.

Deliverables:

- a fully split parser library surface with documented maintenance ownership.

Exit criteria:

- All deprecated shim APIs are removed or have approved exception ADRs.
- Workspace builds and tests pass with the compatibility layer minimized to
  intended long-term surface area.
- Ownership and support expectations are documented in crate READMEs and
  `docs/roadmap.md`.

## Known risks and limitations

- Migration churn may temporarily increase maintenance overhead.
- Diagnostics compatibility constraints may slow internal refactors.
- Semantic model extraction may uncover latent grammar ambiguities that require
  separate ADRs.
- Cross-crate ownership boundaries can add complexity if crate scopes are not
  enforced consistently.

## Outstanding decisions

- Final crate names (`ddlog-syntax`, `ddlog-sema`, `ddlog-parser`) and public
  module paths.
- Versioning policy: independent crate versions or lockstep workspace versions.
- Whether dependency extraction belongs in `ddlog-sema` or a separate
  `ddlog-plan` crate.
- Compatibility window for deprecated facade APIs after consumer migration.

## Architectural rationale

This direction aligns with the project's existing CST-first design for linting
while introducing a dedicated semantic contract for compiler workflows. It
separates concerns at the crate boundary, improves API truthfulness, and
creates a stable foundation for future canonical planning and cache-key
generation in downstream compiler layers.
