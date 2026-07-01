# User guide

`ddlint` provides a parser, semantic model, and linting primitives for
Differential Datalog (`DDlog`) sources. This guide documents the current
user-facing behaviour.

## What is available now

- `parse` converts source text into a lossless CST-backed parse result and
  returns parser diagnostics.
- The linter runner executes registered CST rules and produces
  `LintDiagnostic` values.
- The built-in command-line entrypoint currently only prints a placeholder
  message and exits with a non-zero status.

## Parse a program

`parse` performs tokenisation, CST construction, top-level `for` rule
extraction, and parser-level validation before returning `Parsed`.

```rust
use ddlint::parse;

let source = "input relation R(x: u32);";
let parsed = parse(source);

assert!(parsed.errors().is_empty());
assert_eq!(parsed.root().relations().len(), 1);
```

`Parsed` also exposes parse-time semantic rules and the recovered parse-tree
root so downstream stages can work with structured syntax data.

## Run lint rules

The linter flow is:

1. Register rules in `CstRuleStore`.
2. Build a `Runner` with the source, parsed result, and rule config.
3. Call `run` to obtain diagnostics.

```rust
use ddlint::{linter::{CstRuleStore, RuleConfig, Runner}, parse};

let source = "input relation R(x: u32);";
let parsed = parse(source);
let store = CstRuleStore::new();
let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
let diagnostics = runner.run();

assert!(diagnostics.is_empty());
```

## Rule authorship (high level)

- Implement `Rule` for metadata (`name`, `group`, `docs`) and `CstRule` for CST
  targeting.
- Register one rule per implementation with `CstRuleStore`.
- Rule execution is deterministic regardless of parallel scheduling.

## Logging and diagnostics output

`ddlint` uses the `log` API for parser warnings. Initialize a logger in your
binary and use `RUST_LOG` to control verbosity (for example `RUST_LOG=warn`).
