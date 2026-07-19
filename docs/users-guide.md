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

`parse` performs tokenization, CST construction, top-level `for` rule
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

## Relation declarations

Relation declarations may combine an optional role, optional kind, optional
reference marker, and one body form:

```ddlog
R(id: u32)
input relation User(id: u32) primary key (id)
output stream Events[Event]
multiset & Bag(item: Item)
```

The accepted shape is:

```text
Role? Kind? '&'? Name Body PrimaryKey?
```

`Role` may be `input` or `output`. When no role is present, the relation is
internal. `internal` is not a keyword.

`Kind` may be `relation`, `stream`, or `multiset`. When no kind is present,
`relation` is assumed.

`Body` is either a record field list, such as `(id: u32, name: string)`, or a
bracketed element type, such as `[Event]`.

Primary-key clauses are accepted only on `input` relations with record bodies.
The parser preserves spec-form clauses such as:

```ddlog
input relation Book(row: BookRow) primary key (row) (row.author, row.title)
```

The current typed AST exposes the binder/list names from `primary key (...)`.
Typed access to the trailing expression is deferred to roadmap item `2.6.6.1`.

## Relation diagnostics

The parser emits deterministic diagnostics for invalid relation forms:

<!-- markdownlint-disable MD013 --><!-- Diagnostic table messages stay intact
for reviewability. -->

| Code      | Cause                                        | Message                                                                                |
| --------- | -------------------------------------------- | -------------------------------------------------------------------------------------- |
| D-REL-001 | Kind keyword before role keyword             | `relation role keyword (input/output) must precede the kind keyword`                   |
| D-REL-002 | More than one role keyword                   | `at most one role keyword (input, output) is permitted`                                |
| D-REL-003 | More than one kind keyword                   | `at most one kind keyword (relation, stream, multiset) is permitted`                   |
| D-REL-004 | Bracket body with a primary-key clause       | `bracket-form relations cannot declare a primary key clause`                           |
| D-REL-005 | Empty bracket body                           | `bracket-form relations require a single element type between '[' and ']'`             |
| D-REL-006 | Primary-key clause on a non-`input` relation | `primary key clauses are only valid on input relations`                                |
| D-REL-007 | Stray or malformed `primary key` clause      | `unexpected or malformed primary key clause`                                           |
| D-REL-008 | Bracket-wrapped primary-key clause           | `bracket-wrapped primary key clauses are not supported; remove the surrounding '['/']` |

<!-- markdownlint-enable MD013 -->
