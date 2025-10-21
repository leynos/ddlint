# Project roadmap: `ddlint`

This roadmap breaks down the work into logical phases, from the foundational
parsing layer to the implementation of the linter engine and user-facing
features.

______________________________________________________________________

## **Phase 1: Foundational Components (Largely Complete)**

This phase covers the core infrastructure for turning source text into a
navigable syntax tree.

- [x] **Project Scaffolding & CI**

  - [x] Set up the Cargo project with a library and binary structure.

  - [x] Configure strict Clippy lints in `clippy.toml` and `Cargo.toml`.

  - [x] Establish a CI pipeline using GitHub Actions for formatting, linting,
    and testing (`.github/workflows/ci.yml`).

  - [x] Define project documentation standards and agent instructions
    (`AGENTS.md`, `docs/documentation-style-guide.md`).

- [x] **Lexical Analysis (Tokenizer)**

  - [x] Define all DDlog tokens, including keywords, operators, literals, and
    trivia (whitespace, comments), using `logos` (`src/tokenizer.rs`).

  - [x] Implement a keyword map using `phf` for efficient identifier-to-keyword
    conversion.

  - [x] Create a comprehensive test suite for the tokenizer, verifying token
    kinds, spans, and values for all cases, including trivia and errors
    (`tests/tokenizer.rs`).

- [x] **Core Syntax Tree (CST) Representation**

  - [x] Define the complete `SyntaxKind` enum for all tokens and grammar nodes
    (`src/language.rs`), as required by `rowan`.

  - [x] Implement the `rowan::Language` trait for `DdlogLanguage` to bridge the
    custom `SyntaxKind` with the `rowan` CST.

- [x] **Parsing Pipeline and CST Construction**

  - [x] Implement a token stream wrapper for safe cursor-based navigation
    (`src/parser/token_stream.rs`).

  - [x] Implement a span-scanning parser (`src/parser/span_scanner.rs`) to
    identify the byte ranges of top-level DDlog statements (imports, relations,
    functions, etc.).

  - [x] Implement the `GreenNode` builder (`src/parser/cst_builder/tree.rs`)
    that consumes tokens and statement spans to construct a full-fidelity
    `rowan` CST.

  - [x] Expose a unified `parse()` function that orchestrates the entire
    tokenizing and CST-building process (`src/parser/mod.rs`).

- [x] **Typed AST Layer**

  - [x] Create a `Root` AST node to serve as the entry point for semantic
    analysis (`src/parser/ast/root.rs`).

  - [x] Implement typed AST wrappers for all parsed top-level statements,
    providing convenient, high-level accessors for their properties:

    - [x] `Import` (`src/parser/ast/import.rs`)

    - [x] `TypeDef` (`src/parser/ast/type_def.rs`)

    - [x] `Relation` (`src/parser/ast/relation.rs`)

    - [x] `Index` (`src/parser/ast/index.rs`)

    - [x] `Function` (`src/parser/ast/function.rs`)

    - [x] `Transformer` (`src/parser/ast/transformer.rs`)

    - [x] `Rule` (`src/parser/ast/rule.rs`)

  - [x] Develop and test robust parsing utilities for complex nested structures
    like parameter and type lists, with error recovery
    (`src/parser/ast/parse_utils/`).

______________________________________________________________________

## **Phase 2: Parser Grammar Expansion**

The current parser excels at identifying top-level statements but has a
simplified understanding of statement bodies, particularly expressions and
control flow. This phase aims to build a complete grammar.

- [ ] **Implement Detailed Expression Parsing**

  - [x] Design and implement a Pratt parser for DDlog expressions to correctly
    handle operator precedence and associativity, as outlined in the Haskell
    parser analysis (`docs/haskell-parser-analysis.md`).

  - [x] Add support for parsing all literal types within expressions (e.g.,
    strings, numbers, booleans). The `SyntaxKind` enum already defines these.

  - [x] Implement parsers for variable references (`e_var`) and function calls
    (`e_func`).

  - [x] Implement parsers for compound data structures: struct literals
    (`e_struct`), tuple literals (`e_tuple`), and closures (`e_closure`).

- [ ] **Implement Control-Flow Parsing**

  - [x] Implement a parser for `if`/`else` expressions (`e_ite`).

  - [x] Implement a parser for `for` loops within rules, including optional
    `if` guards (`parseForStatement` from the Haskell analysis).

  - [x] Implement a parser for `match` expressions (`e_match`).

  - [ ] Implement parsers for imperative statements like `break`, `continue`,
    and `return`.

- [ ] **Enhance Rule Body Parsing**

  - [ ] Refactor the current `rule.rs` and `span_scanner.rs` to use the new
    expression and control-flow parsers, replacing the simple `atom` parser.
    This allows for detailed analysis of rule bodies.

  - [ ] Add support for parsing aggregations and `FlatMap` constructs within
    rules.

______________________________________________________________________

## **Phase 3: Linter Engine and Semantic Analysis**

This phase involves building the core engine that will execute lint rules, as
envisioned in `ddlint-design-and-road-map.md`.

- [ ] **Implement the Linter Engine Core**

  - [ ] Define the core `Rule` and `CstRule` traits.

  - [ ] Implement the `RuleCtx` struct to provide context (source text,
    configuration, AST root) to rules.

  - [ ] Create the `CstRuleStore` to register and manage all available lint
    rules.

  - [ ] Build the visitor-based, parallelised Rule Runner (using `rayon`) that
    traverses the CST and invokes the appropriate rules for each node.

  - [ ] Implement the `declare_lint!` macro to simplify rule creation.

- [ ] **Build Semantic Analysis Infrastructure**

  - [ ] Design and implement a `Symbol` table and `Scope` resolution pass. This
    is a critical prerequisite for many correctness lints.

  - [ ] The pass should traverse the CST and populate the symbol table with
    information about:

    - [ ] All declared relations, functions, and types.

    - [ ] The scope of each rule, including variables bound in the head and in
      each literal.

    - [ ] The usage sites of each variable and relation.

______________________________________________________________________

## **Phase 4: Lint Rule Implementation**

With the engine and semantic analysis in place, the initial set of lint rules
from the design document can be implemented.

- [ ] **Correctness Rules**

  - [ ] `unused-relation`:

    - Requires the symbol table.

    - Logic: Iterate through all declared relations in the symbol table. For
      each, check if it has any usage sites (i.e., appears in a rule body). If
      not, emit a diagnostic.

  - [ ] `unused-variable`:

    - Requires the symbol table and scope analysis.

    - Logic: For each rule's scope, iterate through all variable definitions.
      If a variable has a definition site but no usage sites within that rule,
      emit a diagnostic. Handle `_` as an explicit ignore.

  - [ ] `shadowed-variable`:

    - Requires the symbol table and scope analysis.

    - Logic: During scope analysis, when binding a new variable, check if a
      variable with the same name is already in scope from a preceding literal
      in the same rule. If so, emit a diagnostic.

  - [ ] `recursive-negation`:

    - Requires building a dependency graph of relations.

    - Logic: Build a directed graph where an edge `R -> S` exists if `R`
      appears in a rule defining `S`. Mark edges as "negated" if `R` appears
      inside a `not` clause. Detect cycles in the graph that contain at least
      one negated edge.

- [ ] **Performance Rules**

  - [ ] `inefficient-join-order`:

    - Requires heuristics or a simple cost model for relation literals.

    - Logic: For each rule body, analyse the sequence of literals. Emit a hint
      if a literal that is likely to be highly selective (e.g., an equality
      check on a primary key) appears after a less selective one.

  - [ ] `superfluous-group-by`:

    - Requires parsing aggregation expressions.

    - Logic: For each aggregation, compare the set of grouped variables with
      the set of all variables in the atom. If they are the same, the
      `group_by` is redundant.

- [ ] **Style Rules**

  - [ ] `consistent-casing`:

    - Logic: Implement checks for identifier nodes (`T_IDENT`). Based on
      configuration (e.g., `relation_style = "PascalCase"`), verify the casing
      of relation, type, and variable names.

    - This rule needs to be `Autofixable`.

  - [ ] `no-magic-numbers`:

    - Logic: Identify numeric literals (`T_NUMBER`) that appear in rule bodies
      outside a `const` definition. Allow a configurable list of exceptions
      (e.g., 0, 1).

______________________________________________________________________

## **Phase 5: User Interface and Experience**

This phase focuses on creating a polished and usable tool for the end-user.

- [ ] **Command-Line Interface (CLI)**

  - [ ] Replace the placeholder `main.rs` with a full CLI using the `clap`
    crate.

  - [ ] Implement the default linting command: `ddlint <FILES...>`.

  - [ ] Implement the `ddlint rules` subcommand to list all available rules.

  - [ ] Implement the `ddlint explain <RULE_NAME>` subcommand.

  - [ ] Implement configuration loading via `ddlint.toml` using the `config-rs`
    crate.

- [ ] **Rich Diagnostics**

  - [ ] Integrate the `miette` crate for diagnostic reporting.

  - [ ] Refactor the linter engine and rules to emit `miette`-compatible
    diagnostic structs instead of simple errors.

  - [ ] Ensure all diagnostics include error codes and links to documentation.

______________________________________________________________________

## **Phase 6: Advanced Features and Future Work**

- [ ] **Autofixing**

  - [ ] Implement the autofixing mechanism in the Rule Runner, as described in
    the design document. This includes collecting suggestions, checking for
    conflicts, and applying non-overlapping changes in reverse order.

  - [ ] Implement the `--fix` CLI flag.

  - [ ] Add "dual snapshot" tests with `insta` for all `Autofixable` rules to
    verify both the diagnostic message and the resulting code transformation.

- [ ] **IDE Integration**

  - [ ] Plan and implement a Language Server Protocol (LSP) server in a new
    binary crate, reusing the `ddlint` core library to provide on-the-fly
    diagnostics in editors.
