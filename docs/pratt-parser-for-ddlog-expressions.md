# Pratt parser design for DDlog expressions

This design leverages the `chumsky` crate's built-in Pratt parsing
capabilities, which is the ideal approach given its existing use in the project
and its powerful, expressive features for handling expression grammars. This
avoids the need to write a Pratt parser from scratch and ensures cohesion with
the current parsing pipeline.

______________________________________________________________________

## 1. Core concepts and integration strategy

The parser will be implemented as a `chumsky` parser combinator that consumes
the token stream produced by the existing `logos`-based tokenizer. It will
produce a structured expression Abstract Syntax Tree (AST), which will then be
integrated as a node (`N_EXPR_NODE`) within the larger `rowan` Concrete Syntax
Tree (CST).

The key steps are:

1. **Define the Expression AST**: Create Rust `enum`s and `struct`s to
   represent the structure of all possible DDlog expressions.

2. **Define Operator Precedence and Associativity**: Translate the operator
   table from the Haskell parser analysis into a `chumsky` Pratt parser
   definition.

3. **Implement the Parser**: Build the `chumsky` parser using `chumsky::pratt`.

4. **Integrate with the CST**: Ensure that when the expression parser is
   invoked, the resulting AST is correctly represented within a `rowan`
   `GreenNode`.

______________________________________________________________________

## 2. Expression AST definition

First, we need a data structure to represent the parsed expressions. This will
live alongside the other AST definitions in `src/parser/ast/`.

```rust
// In a new file, e.g., src/parser/ast/expr.rs

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
    Tuple(Vec<Expr>),
    // ... other expression types like Struct, Match, If-Else, etc.
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(String), // Keep as string to preserve original format
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, Mod,
    // Comparison
    Eq, Neq, Lt, Lte, Gt, Gte,
    // Logical
    And, Or,
    // Bitwise
    BitAnd, BitOr, Shl, Shr,
    // Assignment
    Assign,
    // Other
    Concat, // '++' in DDlog
}
```

______________________________________________________________________

## 3. Pratt parser implementation with `chumsky`

The heart of the implementation uses `chumsky::pratt`. This requires defining
the atoms (the simplest parts of an expression, like literals or variables) and
then defining the operators and their binding power (precedence).

This logic would be added to the `span_scanner.rs` or a new module it delegates
to.

```rust
// In a parser module, e.g., src/parser/expression_parser.rs

use chumsky::prelude::*;
use chumsky::pratt::*;
use crate::{SyntaxKind, ast}; // Assuming ast::Expr is defined

pub fn expression_parser() -> impl Parser<SyntaxKind, ast::Expr, Error = Simple<SyntaxKind>> {
    // 1. Define the parser for atomic expressions (the base cases)
    let atom = select! {
        SyntaxKind::T_NUMBER => ast::Expr::Literal(ast::Literal::Number),
        SyntaxKind::T_STRING => ast::Expr::Literal(ast::Literal::String),
        SyntaxKind::K_TRUE => ast::Expr::Literal(ast::Literal::Bool(true)),
        SyntaxKind::K_FALSE => ast::Expr::Literal(ast::Literal::Bool(false)),
        SyntaxKind::T_IDENT => ast::Expr::Variable,
    }.or(recursive(|expr| { // For parenthesised expressions
        expr.delimited_by(just(SyntaxKind::T_LPAREN), just(SyntaxKind::T_RPAREN))
    }));

    // 2. Define the operator table using chumsky::pratt
    pratt((
        // == Prefix operators (e.g., -, !) ==
        Operator::new(
            just(SyntaxKind::T_MINUS),
            7,
            |rhs| ast::Expr::UnaryOp {
                op: ast::UnaryOp::Neg,
                expr: Box::new(rhs),
            },
        ),
        Operator::new(
            just(SyntaxKind::K_NOT),
            7,
            |rhs| ast::Expr::UnaryOp {
                op: ast::UnaryOp::Not,
                expr: Box::new(rhs),
            },
        ),

        // == Infix operators by precedence (binding power) ==
        // Precedence level 6: *, /, % (Left-associative)
        Operator::new(
            just(SyntaxKind::T_STAR),
            6,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Mul,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),
        Operator::new(
            just(SyntaxKind::T_SLASH),
            6,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Div,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),
        Operator::new(
            just(SyntaxKind::T_PERCENT),
            6,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Mod,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),

        // Precedence level 5: +, - (Left-associative)
        Operator::new(
            just(SyntaxKind::T_PLUS),
            5,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),
        Operator::new(
            just(SyntaxKind::T_MINUS),
            5,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Sub,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),

        // Precedence level 4: ==, !=, <, <=, >, >= (Comparison)
        Operator::new(
            just(SyntaxKind::T_EQEQ),
            4,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Eq,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),
        Operator::new(
            just(SyntaxKind::T_NEQ),
            4,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Neq,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),
        // ... other comparison operators ...

        // Precedence level 3: && (Logical AND)
        Operator::new(
            just(SyntaxKind::K_AND),
            3,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::And,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),

        // Precedence level 2: || (Logical OR)
        Operator::new(
            just(SyntaxKind::K_OR),
            2,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Or,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .left(),

        // Precedence level 1: = (Assignment, Right-associative)
        Operator::new(
            just(SyntaxKind::T_EQ),
            1,
            |lhs, rhs| ast::Expr::BinaryOp {
                op: ast::BinaryOp::Assign,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        )
        .right(),

        // Function calls would be handled as a postfix operator or within the 'atom' parser
    ))
}
```

**Note**: The binding power values (1 through 7) are illustrative. They must be
carefully chosen to exactly match the precedence rules specified in the Haskell
parser analysis (`docs/haskell-parser-analysis.md`).

______________________________________________________________________

## 4. CST integration

The `chumsky` expression parser produces a structured `ast::Expr`. However, the
core of `ddlint` is the `rowan` CST, which must remain lossless. The expression
parser will not run in isolation; it will be a component of the larger
statement-parsing logic.

The strategy is as follows:

1. When the main parser (in `span_scanner.rs`) encounters a context where an
   expression is expected (e.g., in a rule body or a `return` statement), it
   will invoke `expression_parser()`.

2. The `expression_parser()` will consume tokens from the stream.

3. Crucially, the main parser will continue to feed every single token
   (including whitespace and comments) into the `rowan::GreenNodeBuilder`.

4. The main parser will wrap the sequence of tokens that were successfully
   parsed by `expression_parser()` in an `N_EXPR_NODE` `SyntaxKind`.

This way, we get the best of both worlds:

- A structured `ast::Expr` for immediate semantic validation or interpretation.

- A full-fidelity `N_EXPR_NODE` in the CST, containing all original tokens,
  which can be used for linting rules that care about formatting, or for
  reliable autofixing.

The typed AST wrapper for this new node would look something like this:

```rust
// In src/parser/ast/mod.rs or a new file

pub struct Expression(rowan::SyntaxNode<DdlogLanguage>);

impl AstNode for Expression {
    fn syntax(&self) -> &rowan::SyntaxNode<DdlogLanguage> {
        &self.0
    }
}

impl Expression {
    /// This method would re-run the chumsky parser on the tokens
    /// within this CST node to produce the structured AST on-demand.
    /// This is a common and efficient pattern in CST-based architectures.
    pub fn to_structured_ast(&self) -> Result<ast::Expr, Vec<Simple<SyntaxKind>>> {
        let tokens = self.syntax().children_with_tokens().filter_map(|elem| {
            if let rowan::NodeOrToken::Token(token) = elem {
                // Here you would reconstruct the (SyntaxKind, Span) stream
                // This part needs careful implementation.
            }
            // ...
        });

        // expression_parser().parse(tokens)
        todo!()
    }
}
```

This design provides a clear and robust path forward for implementing a
complete and correct expression parser that integrates perfectly with the
established `ddlint` architecture, fulfilling the next major task in the
project roadmap.

______________________________________________________________________

## 5. Implementation notes

The first working parser lives in `src/parser/expression.rs` and is invoked by
the unit tests. Although the design assumed the availability of
`chumsky::pratt`, the version pinned in the manifest does not yet include that
module. To keep progress unblocked, the implementation uses a small bespoke
Pratt parser that walks the token stream directly. Binding powers match the
operator table analysed from the Haskell parser. Expression spans are now
recorded by `span_scanner` and emitted as `N_EXPR_NODE` entries when building
the CST.

Literal tokens are normalised in a dedicated helper so prefix parsing remains
readable. The parser maps `T_NUMBER`, `T_STRING`, `K_TRUE` and `K_FALSE` to
`ast::Literal` variants, ensuring numbers, strings and booleans appear directly
in the resulting AST.

Operator precedence is centralised in `src/parser/ast/precedence.rs`. Both the
Pratt parser and any future grammar extensions reference this table, ensuring
consistent binding power definitions across the codebase.

Variable references are parsed by interpreting identifier tokens as
`Expr::Variable`. When an identifier is immediately followed by a left
parenthesis, the parser treats it as a function call, parsing a comma-separated
list of argument expressions and producing `Expr::Call { name, args }`. This
postfix form naturally binds tighter than infix operators, so no additional
precedence entries are required.

Struct literals, tuple literals, and closures extend the prefix grammar. Struct
construction recognizes `Ident { field: expr, ... }` and records field order in
the AST. Tuple literals are distinguished from grouped expressions by the
presence of a comma or an empty pair of parentheses. Both structs and tuples
accept trailing commas. Closure literals parse a pipe-delimited parameter list
(allowing a trailing comma) followed by the body expression. Each feature is
implemented with small helper routines to keep the main parser readable.
