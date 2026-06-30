# Repository layout

This document describes the main repository paths for `ddlint` contributors. It
is the canonical guide to where source code, documentation, examples, and
validation assets belong.

## Tree overview

The following tree is intentionally compact. It shows the stable top-level
shape and the subdirectories that contributors most often need when navigating
the project.

```plaintext
.
├── .github/
│   └── workflows/
├── docs/
│   ├── archive/
│   └── execplans/
├── examples/
├── src/
│   ├── linter/
│   ├── parser/
│   ├── sema/
│   └── test_util/
└── tests/
    ├── linter_runner/
    └── support/
```

_Figure 1: Compact repository tree for contributor orientation._

## Top-level paths

- `.github/`: GitHub automation, dependency updates, and continuous
  integration workflows. Keep repository automation here rather than in ad hoc
  scripts.
- `.vale/` and `.vale.ini`: Prose linting configuration and vocabulary. Update
  these when documentation terminology changes deliberately.
- `AGENTS.md`: Repository-specific agent and contributor operating
  instructions. This file supersedes inherited template guidance for this
  repository.
- `Cargo.toml` and `Cargo.lock`: Rust workspace package metadata and dependency
  lock state. Keep dependency requirements compatible with the policy in
  `AGENTS.md`.
- `Makefile`: Canonical command entrypoints for build, format, lint, test, and
  documentation validation. Prefer these targets over direct tool invocations.
- `README.md`: Public project overview and entrypoint. Keep it high-level and
  link to deeper documentation rather than duplicating it.
- `docs/`: Long-lived project documentation, design records, guides, and
  execution plans. Use [documentation contents](contents.md) as the index.
- `examples/`: Differential Datalog examples used for manual inspection and
  parser behaviour checks. Keep examples small and representative.
- `src/`: Rust library and command-line implementation. Modules must start with
  module-level documentation comments.
- `tests/`: Integration and behavioural tests. Shared integration helpers
  belong under `tests/support/`.

## Source layout

The `src/` tree is organized by domain responsibility rather than by generic
layer names.

- `src/main.rs`: Command-line entrypoint for the `ddlint` binary.
- `src/lib.rs`: Library crate root and public module surface.
- `src/language.rs`: Language-facing definitions shared across parser,
  semantic analysis, and linting.
- `src/tokenizer.rs`: Tokenization support.
- `src/syntax_utils.rs`: Shared syntax helpers.
- `src/parser/`: Parser, concrete syntax tree construction, span scanning,
  abstract syntax tree projections, and parser tests.
- `src/sema/`: Semantic analysis support such as symbol and scope handling.
- `src/linter/`: Lint rule abstractions, rule storage, runner integration, and
  concrete lint rules.
- `src/test_util/`: Shared test utilities that are part of the crate source
  tree.

## Documentation layout

The `docs/` directory is the source of truth for project context that should
survive beyond one implementation branch.

- `docs/contents.md`: Index for the documentation set.
- `docs/documentation-style-guide.md`: Documentation authoring and formatting
  rules.
- `docs/repository-layout.md`: Reference for major documentation paths,
  including document roles and ownership responsibilities.
- `docs/ddlint-design.md`: Primary design document for `ddlint`.
- `docs/developers-guide.md`: Maintainer workflow and implementation guidance.
- `docs/roadmap.md`: Delivery roadmap and task sequencing.
- `docs/adr-*.md`: Architectural decision records.
- `docs/execplans/`: Execution plans for non-trivial implementation work.
- `docs/archive/`: Historical documents kept for traceability but no longer
  treated as current guidance.

## Test layout

Integration tests live under `tests/`, with feature-oriented files for parser,
semantic, and linter behaviours. Shared fixtures and helper code belong under
`tests/support/`, while command-line runner scenarios belong under
`tests/linter_runner/`.

Keep test data close to the tests that own it unless it becomes large enough to
justify a dedicated fixture directory. When a helper mutates process-global
state, wrap that state behind a shared guard rather than duplicating local
locking in individual tests.
