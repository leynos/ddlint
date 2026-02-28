# Project roadmap: `ddlint`

This roadmap groups work into phases, steps, and measurable tasks, from parser
foundation through lint engine delivery and editor integration.

Terminology: references to the Abstract Syntax Tree (AST) describe the typed
tree produced after parsing, while the Concrete Syntax Tree (CST) captures the
lossless token-level structure built during parsing.

## 1. Foundational components (largely complete)

This phase covers the core infrastructure required to turn source text into a
navigable CST and typed AST.

### 1.1. Project scaffolding and continuous integration

- [x] 1.1.1. Set up the Cargo project with library and binary targets.
  See docs/parser-implementation-notes.md.
- [x] 1.1.2. Configure strict Clippy lints in `clippy.toml` and
  `Cargo.toml`. See docs/ddlint-design-and-road-map.md §6.
- [x] 1.1.3. Establish a Continuous Integration (CI) pipeline for formatting,
  linting, and tests (`.github/workflows/ci.yml`). See
  docs/ddlint-design-and-road-map.md §6.
- [x] 1.1.4. Define project documentation standards and agent guidance
  (`AGENTS.md`, `docs/documentation-style-guide.md`). See
  docs/documentation-style-guide.md §Documentation style guide.

### 1.2. Lexical analysis (tokenizer)

- [x] 1.2.1. Define DDlog tokens for keywords, operators, literals, and trivia
  with `logos` (`src/tokenizer.rs`). See docs/parser-implementation-notes.md
  and docs/differential-datalog-parser-syntax-spec-updated.md §2.
- [x] 1.2.2. Implement keyword mapping with `phf` for efficient
  identifier-to-keyword conversion. See docs/parser-implementation-notes.md.
- [x] 1.2.3. Add tokenizer tests that validate token kinds, spans, and values,
  including trivia and error cases (`tests/tokenizer.rs`). See
  docs/parser-implementation-notes.md.

### 1.3. Core CST representation

- [x] 1.3.1. Define the complete `SyntaxKind` enum for tokens and grammar
  nodes (`src/language.rs`). See docs/ddlint-design-and-road-map.md §2.1 and
  docs/parser-implementation-notes.md.
- [x] 1.3.2. Implement `rowan::Language` for `DdlogLanguage` to bridge custom
  syntax kinds into the CST. See docs/ddlint-design-and-road-map.md §1.3.

### 1.4. Parsing pipeline and CST construction

- [x] 1.4.1. Implement token-stream cursor utilities for safe parser
  navigation (`src/parser/token_stream.rs`). See
  docs/ddlint-design-and-road-map.md §2.2.
- [x] 1.4.2. Implement span scanning for top-level DDlog statements
  (`src/parser/span_scanner.rs`). See docs/parser-implementation-notes.md.
- [x] 1.4.3. Implement a `GreenNode` builder that consumes tokens and statement
  spans to produce a full-fidelity CST (`src/parser/cst_builder/tree.rs`). See
  docs/ddlint-design-and-road-map.md §2.
- [x] 1.4.4. Expose a unified `parse()` entrypoint that orchestrates tokenizing
  and CST construction (`src/parser/mod.rs`). See
  docs/ddlint-design-and-road-map.md §2.

### 1.5. Typed AST layer

- [x] 1.5.1. Create a `Root` AST node for semantic analysis entry
  (`src/parser/ast/root.rs`). See docs/parser-implementation-notes.md.
- [x] 1.5.2. Implement typed AST wrappers for parsed top-level statements.
  See docs/parser-implementation-notes.md.
  - [x] 1.5.2.1. Implement `Import` (`src/parser/ast/import.rs`).
    See docs/differential-datalog-parser-syntax-spec-updated.md §5.2.
  - [x] 1.5.2.2. Implement `TypeDef` (`src/parser/ast/type_def.rs`).
    See docs/differential-datalog-parser-syntax-spec-updated.md §5.2.
  - [x] 1.5.2.3. Implement `Relation` (`src/parser/ast/relation.rs`).
    See docs/differential-datalog-parser-syntax-spec-updated.md §5.5.
  - [x] 1.5.2.4. Implement `Index` (`src/parser/ast/index.rs`).
    See docs/differential-datalog-parser-syntax-spec-updated.md §5.6.
  - [x] 1.5.2.5. Implement `Function` (`src/parser/ast/function.rs`).
    See docs/differential-datalog-parser-syntax-spec-updated.md §5.3.
  - [x] 1.5.2.6. Implement `Transformer` (`src/parser/ast/transformer.rs`).
    See docs/differential-datalog-parser-syntax-spec-updated.md §5.4.
  - [x] 1.5.2.7. Implement `Rule` (`src/parser/ast/rule.rs`).
    See docs/differential-datalog-parser-syntax-spec-updated.md §5.8.
- [x] 1.5.3. Add and test parsing utilities for nested structures such as
  parameter and type lists (`src/parser/ast/parse_utils/`). See
  docs/parser-implementation-notes.md.

## 2. Parser grammar expansion

The parser now identifies top-level statements, but this phase completes
expression parsing, control flow, and syntax-spec alignment.

### 2.1. Detailed expression parsing

- [x] 2.1.1. Implement a Pratt parser with correct operator precedence and
  associativity. See docs/parser-implementation-notes.md and
  docs/differential-datalog-parser-syntax-spec-updated.md §4.
- [x] 2.1.2. Parse all literal forms in expressions (strings, numbers,
  booleans). See docs/parser-implementation-notes.md and
  docs/differential-datalog-parser-syntax-spec-updated.md §3.
- [x] 2.1.3. Parse variable references (`e_var`) and function calls (`e_func`).
  See docs/parser-implementation-notes.md.
- [x] 2.1.4. Parse struct literals, tuple literals, and closures (`e_struct`,
  `e_tuple`, `e_closure`). See docs/parser-implementation-notes.md.

### 2.2. Control-flow parsing

- [x] 2.2.1. Parse `if` and `else` expressions (`e_ite`).
  See docs/parser-implementation-notes.md and
  docs/differential-datalog-parser-syntax-spec-updated.md §5.13.
- [x] 2.2.2. Parse `for` loops inside rules, including optional `if` guards.
  See docs/parser-implementation-notes.md and
  docs/differential-datalog-parser-syntax-spec-updated.md §5.10.
- [x] 2.2.3. Parse `match` expressions (`e_match`).
  See docs/parser-implementation-notes.md and
  docs/differential-datalog-parser-syntax-spec-updated.md §5.13.
- [x] 2.2.4. Parse imperative statements (`break`, `continue`, `return`).
  See docs/differential-datalog-parser-syntax-spec-updated.md §5.10.

### 2.3. Rule body parsing integration

- [x] 2.3.1. Refactor `rule.rs` and `span_scanner.rs` to use expression and
  control-flow parsers instead of the simple atom parser. See
  docs/parser-implementation-notes.md.
- [x] 2.3.2. Parse aggregation and FlatMap constructs in rule bodies.
  See docs/differential-datalog-parser-syntax-spec-updated.md §5.12 and
  docs/differential-datalog-parser-syntax-spec-updated.md §6.1.

### 2.4. Syntax-spec lexical and expression conformance

- [x] 2.4.1. Support raw and interpolated string forms, including interned
  variants, and reject interpolated strings in patterns. See
  docs/differential-datalog-parser-syntax-spec-updated.md §3.2 and
  docs/differential-datalog-parser-syntax-spec-updated.md §5.12.
- [x] 2.4.2. Parse width-qualified numeric literals (signed and unsigned
  integers, floats) with range validation and shaped AST literals. See
  docs/differential-datalog-parser-syntax-spec-updated.md §3.1.
- [x] 2.4.3. Complete the operator table for `++`, `^`, and `=>`, including
  precedence tests against existing operators. See
  docs/differential-datalog-parser-syntax-spec-updated.md §4 and
  docs/parser-implementation-notes.md.
- [x] 2.4.4. Parse vector and map literals as raw AST (`Expr::VecLit`,
  `Expr::MapLit`) while deferring builder-call desugaring. See
  docs/differential-datalog-parser-syntax-spec-updated.md §3.3 and
  docs/differential-datalog-parser-syntax-spec-updated.md §6.4.
- [x] 2.4.5. Enforce the qualified-call rule so only fully scoped identifiers
  parse as function calls. See docs/parser-implementation-notes.md and
  docs/differential-datalog-parser-syntax-spec-updated.md §2.2.
  - [x] 2.4.5.1. Parser accepts only fully qualified call syntax (for example,
    `foo.bar()` and `pkg::foo()`), and parses bare `bar(...)` as a deferred
    name-application form.
    See docs/parser-implementation-notes.md.
  - [x] 2.4.5.2. Unit tests include at least two explicit cases: one qualified
    call accepted, and one bare call not parsed as a function call.
    See docs/ddlint-design-and-road-map.md §6.1.
  - [x] 2.4.5.3. Integration tests include at least two end-to-end cases proving
    the same distinction through parser entrypoints.
    See docs/ddlint-design-and-road-map.md §6.3.
  - [x] 2.4.5.4. Add at least one dedicated regression case and keep CI green.
    See docs/ddlint-design-and-road-map.md §6.
  - [x] 2.4.5.5. Update documentation and changelog entries for qualified-call
    parsing and deferred name resolution.
    See docs/parser-implementation-notes.md.

### 2.5. Syntax-spec structural and semantic conformance

- [x] 2.5.1. Introduce a dedicated pattern parser for `match` arms, `for`
  bindings, and FlatMap constructs. See
  docs/differential-datalog-parser-syntax-spec-updated.md §5.12.
- [x] 2.5.2. Parse rule-head extensions: multi-head rules, head locations,
  delay markers (`-<N>`), diff adornments (`'`), and by-reference head lowering
  to `ref_new`. See docs/differential-datalog-parser-syntax-spec-updated.md §7
  and docs/differential-datalog-parser-syntax-spec-updated.md §6.3.
- [x] 2.5.3. Extract `group_by` and legacy `Aggregate` constructs during
  parsing into normalized representation. See
  docs/differential-datalog-parser-syntax-spec-updated.md §6.1 and
  docs/differential-datalog-parser-syntax-spec-updated.md §6.2.
- [ ] 2.5.4. Resolve top-level `for` contract and implementation status.
  Decide whether to implement top-level `for` desugaring into rules or to mark
  top-level `for` as unsupported in the normative grammar. See
  docs/differential-datalog-parser-syntax-spec-updated.md §6.5 and
  docs/parser-conformance-register.md item 8.
- [x] 2.5.5. Parse `apply` items and enforce that non-`extern` transformers
  produce diagnostics. See
  docs/differential-datalog-parser-syntax-spec-updated.md §5.7 and
  docs/differential-datalog-parser-syntax-spec-updated.md §5.4.
- [x] 2.5.6. Validate attribute placement and top-level name uniqueness.
  See docs/differential-datalog-parser-syntax-spec-updated.md §8 and
  docs/differential-datalog-parser-syntax-spec-updated.md §9.
  - [x] 2.5.6.1. `#[cold]\ntypedef T = u32` parses without errors, while
    `#[cold]\nindex I on R(x)` yields exactly one attribute-placement error.
    See docs/differential-datalog-parser-syntax-spec-updated.md §9.
  - [x] 2.5.6.2. Duplicate top-level typedef names yield exactly one duplicate
    error that includes the duplicated identifier.
    See docs/differential-datalog-parser-syntax-spec-updated.md §8.
  - [x] 2.5.6.3. Function overloading by arity is accepted without duplicate
    errors.
    See docs/differential-datalog-parser-syntax-spec-updated.md §8.
  - [x] 2.5.6.4. Unit, parser-level, and integration tests cover permitted and
    forbidden attribute targets, plus all duplicate-name categories.
    See docs/ddlint-design-and-road-map.md §6.
  - [x] 2.5.6.5. `make test`, `make check-fmt`, and `make lint` pass.
    See docs/ddlint-design-and-road-map.md §6.

### 2.6. Parser conformance decisions before ADR-001 planning

- [ ] 2.6.1. Close the top-level `for` decision and align scanner behaviour,
  tests, and spec language. See docs/parser-conformance-register.md item 8.
- [ ] 2.6.2. Decide the aggregation extraction boundary (parse stage versus
  semantic stage), then align parser pipeline guarantees and docs. See
  docs/parser-conformance-register.md item 9.
- [ ] 2.6.3. Decide collection literal lowering stage ownership (parser,
  semantic pass, or later lowering), then align docs and tests. See
  docs/parser-conformance-register.md item 10.
- [ ] 2.6.4. Align index declaration grammar between scanner implementation and
  syntax specification. See docs/parser-conformance-register.md item 11.
- [ ] 2.6.5. Align transformer declaration grammar, including output signature
  requirements. See docs/parser-conformance-register.md item 12.
- [ ] 2.6.6. Resolve relation form coverage (role/kind/bracket variants) and
  document supported forms explicitly. See docs/parser-conformance-register.md
  item 13.
- [ ] 2.6.7. Finalize legacy token compatibility policy (`typedef`, `as`,
  legacy type names, `#`, `<=>`) with deterministic diagnostics. See
  docs/parser-conformance-register.md item 14.
- [ ] 2.6.8. Decide brace-group extension policy (`{ expr }`): codify or
  remove with migration diagnostics. See docs/parser-conformance-register.md
  item 15.

## 3. Linter engine and semantic analysis

This phase builds the execution engine and semantic context used by rule
implementations.

### 3.1. Linter engine core

- [x] 3.1.1. Define the core `Rule` and `CstRule` traits.
  See docs/ddlint-design-and-road-map.md §3.1.
- [x] 3.1.2. Implement `RuleCtx` to provide source text, configuration, and AST
  context to rules. See docs/ddlint-design-and-road-map.md §1.2 and
  docs/ddlint-design-and-road-map.md §3.1.
- [x] 3.1.3. Create `CstRuleStore` to register and resolve rule handlers by
  syntax kind. See docs/ddlint-design-and-road-map.md §1.2.
- [x] 3.1.4. Build a visitor-based parallel rule runner (using `rayon`) to walk
  the CST and invoke applicable rules. See docs/ddlint-design-and-road-map.md
  §1.2.

### 3.2. Rule-authoring ergonomics

- [ ] 3.2.1. Implement the `declare_lint!` macro to reduce boilerplate when
  defining rule metadata and behaviour. See docs/ddlint-design-and-road-map.md
  §3.2.
- [ ] 3.2.2. Ensure macro output integrates with rule registration and runtime
  dispatch in `CstRuleStore`. Requires 3.1.3. See
  docs/ddlint-design-and-road-map.md §3.2.

### 3.3. Semantic analysis infrastructure

- [ ] 3.3.1. Implement symbol-table and scope-resolution passes.
  See docs/ddlint-design-and-road-map.md §2.3.
- [ ] 3.3.2. Record declarations for relations, functions, and types.
  Requires 3.3.1. See docs/ddlint-design-and-road-map.md §2.3.
- [ ] 3.3.3. Record per-rule scope bindings for head variables and
  literal-derived variables. Requires 3.3.1. See
  docs/ddlint-design-and-road-map.md §2.3.
- [ ] 3.3.4. Record usage sites for variables and relations.
  Requires 3.3.1. See docs/ddlint-design-and-road-map.md §2.3.

## 4. Lint rule implementation

With the rule runner and semantic analysis in place, implement the initial lint
catalog.

### 4.1. Correctness rules

- [ ] 4.1.1. Implement `unused-relation` diagnostics for declared relations with
  no usage sites. Requires 3.3.2 and 3.3.4. See
  docs/ddlint-design-and-road-map.md §3.3.
- [ ] 4.1.2. Implement `unused-variable` diagnostics for variables defined but
  not used within a rule, treating `_` as explicit ignore. Requires 3.3.3 and
  3.3.4. See docs/ddlint-design-and-road-map.md §3.3.
- [ ] 4.1.3. Implement `shadowed-variable` diagnostics for same-scope rebinding
  in later literals. Requires 3.3.3. See docs/ddlint-design-and-road-map.md
  §3.3.
- [ ] 4.1.4. Implement `recursive-negation` detection via relation dependency
  graphs that track negated edges and reject cycles containing negation. See
  docs/ddlint-design-and-road-map.md §3.3.

### 4.2. Performance rules

- [ ] 4.2.1. Implement `inefficient-join-order` hints using literal selectivity
  heuristics or a simple cost model. See docs/ddlint-design-and-road-map.md
  §3.3.
- [ ] 4.2.2. Implement `superfluous-group-by` detection when grouped variables
  equal the atom variable set. Requires parser support from 2.5.3. See
  docs/ddlint-design-and-road-map.md §3.3.

### 4.3. Style rules

- [ ] 4.3.1. Implement `consistent-casing` checks for identifier nodes using
  rule configuration (`relation_style`, and related settings). See
  docs/ddlint-design-and-road-map.md §3.3 and
  docs/ddlint-design-and-road-map.md §4.3.
- [ ] 4.3.2. Make `consistent-casing` autofixable.
  Requires 6.1.1. See docs/ddlint-design-and-road-map.md §3.3 and
  docs/ddlint-design-and-road-map.md §5.2.
- [ ] 4.3.3. Implement `no-magic-numbers` checks for numeric literals in rule
  bodies outside `const` definitions, with configurable exceptions. See
  docs/ddlint-design-and-road-map.md §3.3.

## 5. User interface and experience

This phase delivers the end-user command-line interface and diagnostic output.

### 5.1. Command-line interface

- [ ] 5.1.1. Replace placeholder `main.rs` with a `clap`-based CLI.
  See docs/ddlint-design-and-road-map.md §4.1.
- [ ] 5.1.2. Implement default lint command: `ddlint <FILES...>`.
  See docs/ddlint-design-and-road-map.md §4.1.
- [ ] 5.1.3. Implement `ddlint rules` to list available rules.
  See docs/ddlint-design-and-road-map.md §4.1.
- [ ] 5.1.4. Implement `ddlint explain <RULE_NAME>`.
  See docs/ddlint-design-and-road-map.md §4.1.
- [ ] 5.1.5. Implement `ddlint.toml` loading via `config-rs`.
  See docs/ddlint-design-and-road-map.md §4.2 and
  docs/ddlint-design-and-road-map.md §4.3.

### 5.2. Rich diagnostics

- [ ] 5.2.1. Integrate `miette` for diagnostic rendering.
  See docs/ddlint-design-and-road-map.md §5.1.
- [ ] 5.2.2. Refactor rule outputs to emit `miette`-compatible diagnostic
  structures. See docs/ddlint-design-and-road-map.md §5.1.
- [ ] 5.2.3. Ensure diagnostics include stable error codes and documentation
  links. See docs/ddlint-design-and-road-map.md §5.1.

## 6. Advanced features and future work

### 6.1. Autofixing

- [ ] 6.1.1. Implement autofix collection and conflict resolution in the rule
  runner, applying non-overlapping edits in reverse order. See
  docs/ddlint-design-and-road-map.md §5.2.
- [ ] 6.1.2. Implement the `--fix` CLI flag.
  Requires 6.1.1 and 5.1.1. See docs/ddlint-design-and-road-map.md §4.1 and
  docs/ddlint-design-and-road-map.md §5.2.
- [ ] 6.1.3. Add dual-snapshot tests with `insta` for each autofixable rule,
  covering both diagnostics and transformed output. Requires 6.1.1. See
  docs/ddlint-design-and-road-map.md §5.3 and
  docs/ddlint-design-and-road-map.md §6.2.

### 6.2. Integrated development environment (IDE) integration

- [ ] 6.2.1. Create a dedicated Language Server Protocol (LSP) binary crate
  that reuses the `ddlint` core library. See docs/ddlint-design-and-road-map.md
  §7.
- [ ] 6.2.2. Integrate an LSP framework (`tower-lsp`) and protocol types
  (`lsp-types`) for transport and message contracts. Requires 6.2.1. See
  docs/ddlint-design-and-road-map.md §7.
- [ ] 6.2.3. Implement on-the-fly diagnostics publication (`didOpen`,
  `didChange`) backed by the existing lint engine. Requires 6.2.1 and 5.2.1.
  See docs/ddlint-design-and-road-map.md §7.
