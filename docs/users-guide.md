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
  message and exits with a non-zero status. When the message cannot be written
  to standard output, the command reports the failure on standard error and
  still exits non-zero.

### Const-capable APIs

The following public methods can be called in a `const` item or other
compile-time context:

- `RuleConfigValue::as_bool`
- `UseOrigin::is_relation_read`
- `SymbolOrigin::is_warning_eligible_rule_binding`
- `StringLiteral::is_interpolated`

The compile fixture `tests/ui/const_api_pass.rs` guards these qualifications
against regression.

### Unreleased behaviour changes

Three changes are not yet in a release. None removes an API, so no code
migration is required, but results observed through the previous release can
change:

- Public methods listed under [Const-capable APIs](#const-capable-apis) can now
  be evaluated in `const` items. Existing calls are unaffected.
- The `unused-variable` rule treats grouping parentheses and unary operators as
  transparent when it resolves uses. A binding used only under a wrapper is now
  counted as used, so a source that previously reported such a binding no
  longer does. See [Unused-variable rule](#unused-variable-rule).
- `Function::return_type` now returns the declared return type for parameter
  lists containing nested parenthesized types. It previously stopped at the
  first closing parenthesis and could return a truncated type. See
  [Function signatures](#function-signatures).

## Planned CLI configuration

The CLI remains a placeholder. Roadmap item 5.1.5 defines the configuration
contract for the planned lint command; it becomes active when the command
surface is implemented.

The planned file is `ddlint.toml`, with TOML as the only enabled format. Lint
settings live under `[cmds.lint]`, the configuration namespace for the default
`ddlint <FILES...>` invocation:

```toml
[cmds.lint]
format = "json"
ignore_patterns = [".git/", "target/", "generated/"]

[cmds.lint.rules]
"unused-relation" = "warn"
"unused-variable" = "error"
```

The default format is `rich`; the accepted formats are `rich`, `json`, and
`compact`. The default ignore patterns are `.git/`, `build/`, and `target/`.
Rule severities are `allow`, `hint`, `warn`, and `error`, and only the
`unused-relation` and `unused-variable` rules accept overrides. `format`,
ignore patterns, and rule severities can be supplied by the file and matching
`DDLINT_*` environment variables; `--format` is the explicit command-line
override. The precedence order is:

```text
defaults < files < environment < command line
```

Nested environment names use double underscores between configuration keys. For
example, `DDLINT_CMDS__LINT__FORMAT=json` sets the lint format, while
`DDLINT_CONFIG_PATH=/path/to/ddlint.toml` selects one required configuration
file.

The ignore-pattern setting is a list in TOML. Its environment value must be a
JSON array of strings, for example
`DDLINT_CMDS__LINT__IGNORE_PATTERNS='["target/", "build/"]'`.

Automatic discovery checks the user and system directories from the XDG Base
Directory Specification (XDG) and the platform's native configuration
directories for `ddlint/ddlint.toml` and `.ddlint.toml`, then the home
configuration locations, including `~/.ddlint.toml`, and finally `ddlint.toml`
in the current working directory. Project discovery does not check
`.ddlint.toml`, and discovery does not search parent directories. The
`DDLINT_CONFIG_PATH` variable, `--config <PATH>`, or `-c <PATH>` selects one
required file and suppresses automatic discovery. An absent automatic file
leaves defaults in place; a missing selected file is an error.

### Extend a configuration file

A configuration file may name one parent file with a top-level `extends` key:

```toml
# ddlint.toml
extends = "shared/base.toml"

[cmds.lint]
ignore_patterns = ["generated/"]
```

Relative paths resolve from the file that declares them. The parent is merged
beneath the child, so the child overrides only the keys it restates. A
`[cmds.lint.rules]` entry that only the parent declares survives, while a child
`ignore_patterns` array replaces the parent array instead of extending it. A
missing parent is an error that names the parent's absolute path and the file
that referenced it.

`--no-ignore` and file operands remain command-line-only. Unknown configuration
fields and unknown rule names are rejected. The `rules` and `explain` commands
have no settings and do not load configuration. Named profiles are not offered.
These are planned interfaces, not current binary behaviour.

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

### Function signatures

`Function::return_type` returns the declared return type, and
`Function::parameters` returns the declared parameter names and types. Both
handle parameter lists that contain nested parenthesized types: the scan tracks
parenthesis depth, so the closing parenthesis of a nested type does not end the
parameter list early.

```rust
use ddlint::parse;

let source = "function f(pair: ((u32,bool),string)): u32 { }";
let parsed = parse(source);
let function = parsed.root().functions().into_iter().next().expect("one function");

assert_eq!(function.return_type().as_deref(), Some("u32"));
assert_eq!(function.parameters().len(), 1);
```

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

When parsing produces errors, `Runner::run` returns no lint diagnostics and
does not run rules against the recovered parse tree. Inspect and report the
parser errors separately.

### Unused-variable rule

The `unused-variable` rule treats wrapper expressions as transparent when it
resolves uses. A binding used only inside parentheses or under a unary operator
still counts as used, so neither rule below reports `head_x`:

```ddlog
Output(head_x) :- Source((head_x)).
Output(head_x) :- Source(-head_x).
```

Only a binding with no resolved use is reported.

## Rule authorship (high level)

- Implement `Rule` for metadata (`name`, `group`, `docs`) and `CstRule` for CST
  targeting.
- Register one rule per implementation with `CstRuleStore`.
- Rule execution is deterministic regardless of parallel scheduling.

## Logging and diagnostics output

Legacy parser warnings use the `log` API. Initialize a logger in the host
binary and use `RUST_LOG` to control verbosity (for example `RUST_LOG=warn`).
For structured parser-attempt and diagnostic events, pass a `ParseObserver` to
`parse_with_observer`; this is opt-in and does not require a telemetry runtime.
Applications can implement and pass their own `ParseObserver` without enabling
`observability`; that feature is needed only for the supplied
`TelemetryObserver` adapter. The optional `observability` Cargo feature
supplies a `tracing` and `metrics` adapter. See the
[parser observability guide](parser-observability.md) for export and alerting
guidance. The library never initializes host subscribers or recorders.

Enable the supplied adapter in the application dependency:

```toml
[dependencies]
ddlint = { version = "0.1.0", features = ["observability"] }
```

Initialize the application's tracing subscriber and metrics recorder, then
construct one `TelemetryObserver` and reuse it for parser calls:

```rust,no_run
use ddlint::{TelemetryObserver, parse_with_observer};

fn parse_sources(sources: &[&str]) {
    // Configure application telemetry before constructing the observer.
    let observer = TelemetryObserver::new();
    for source in sources {
        let _parsed = parse_with_observer(source, &observer);
    }
}
```

## Legacy DDlog tokens

### Unreleased migration

Sources using the rejected legacy syntax below now produce parser errors and no
lint diagnostics from `Runner::run`. Migrate the syntax before relying on lint
results.

The parser keeps legacy token kinds in the lexer so diagnostics can point to
the exact source span, but unsupported legacy syntax is rejected during parsing.

- Use `type Foo = ...` instead of `typedef Foo = ...`.
- Use sized integer types such as `i64`, `u64`, and `u32` instead of `bigint`
  or `bit<N>`.
- Use `f64` instead of `double`.
- Use `f32` instead of `float`.
- Use signed sized integer types such as `i32` instead of `signed<N>`.
- Remove `<=>`; it was reserved upstream but has no DDlog semantics in
  `ddlint`.
- Use `#[...]` for attributes. A bare `#` is rejected.

`as` remains valid in supported grammar positions, including import aliases
such as `import foo::bar as baz`.

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

The typed AST exposes the clause through `primary_key()`. `binder_names()`
lists the identifiers, each binder and the whole clause carry spans, and
`expression()` returns the optional spec-form expression. Parsing the
expression is lazy: `PrimaryKeyExpression::parse()` reports errors at
source-absolute spans, and a failed parse never hides the binders.

Two scanner corrections accompany this. A `;` now ends a primary-key clause
instead of being absorbed into it, so a following declaration on the same line
is still scanned. A binder list that is not a comma-separated list of
identifiers, such as `(x.y)` or `(x,)`, reports `D-REL-007` instead of being
accepted.

The [v0.2.0 migration guide](./v0-2-0-migration-guide.md) shows the
before-and-after call shapes for code that used the retired `Vec<String>`
binder list.

## Relation diagnostics

The parser emits deterministic diagnostics for invalid relation forms:

<!-- markdownlint-disable MD013 --><!-- Diagnostic table messages stay intact
for reviewability. -->

Codes beginning with `D-REL` identify relation diagnostics.

Table 1. Relation diagnostics.

| Code        | Cause                                        | Message                                                                                            |
| ----------- | -------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `D-REL-001` | Kind keyword before role keyword             | `D-REL-001: relation role keyword (input/output) must precede the kind keyword`                    |
| `D-REL-002` | More than one role keyword                   | `D-REL-002: at most one role keyword (input, output) is permitted`                                 |
| `D-REL-003` | More than one kind keyword                   | `D-REL-003: at most one kind keyword (relation, stream, multiset) is permitted`                    |
| `D-REL-004` | Bracket body with a primary-key clause       | `D-REL-004: bracket-form relations cannot declare a primary key clause`                            |
| `D-REL-005` | Empty bracket body                           | `D-REL-005: bracket-form relations require a single element type between '[' and ']'`              |
| `D-REL-006` | Primary-key clause on a non-`input` relation | `D-REL-006: primary key clauses are only valid on input relations`                                 |
| `D-REL-007` | Stray or malformed `primary key` clause      | `D-REL-007: unexpected or malformed primary key clause`                                            |
| `D-REL-008` | Bracket-wrapped primary-key clause           | `D-REL-008: bracket-wrapped primary key clauses are not supported; remove the surrounding '['/']'` |

<!-- markdownlint-enable MD013 -->

### Build standard

Use `make` or `make build` for a debug build and `make release` for a release
build. Each Make build entrypoint invokes Cargo, which decides whether work is
already up to date. `build`, `test`, `typecheck` and `test-clippy-policy`
preflight the selected Rust toolchain and required Linux linker. `lint` checks
tools after `interrogate`. Formatting via `check-fmt` does not require linker
preflight. If setup is missing, run `make install-build-tools` to install the
pinned `mold` 2.41.0 release for supported Linux x86-64 and 64-bit ARM hosts.
The installer does not support other Linux architectures. macOS and Windows
keep their platform linker.

Development and test builds use the parallel `rustc` frontend (`-Zthreads=8`)
and, on Linux, the `mold` linker. Make appends these standard flags after
inherited `RUSTFLAGS`; any caller-supplied flags remain the caller's
responsibility. Release and coverage builds assign flags explicitly and do not
add these development defaults. Direct Cargo commands use the configuration in
`.cargo/config.toml` unless an assigned `RUSTFLAGS` replaces it. Maintainers
can find the full command and prerequisite contract in the
[developer's guide](developers-guide.md#the-build-standard).
