# Function Parsing Design

This document outlines the strategy for parsing `function` definitions and
declarations within `ddlint`. The parser relies on small helpers to interpret
parameter lists and optional return types. These helpers now live in the
`parser::ast::parse_utils` module so that both `Function` and `Relation` AST
nodes can reuse them.

```mermaid
classDiagram
    class parse_name_type_pairs {
        <<function>>
    }
    class parse_type_expr {
        <<function>>
    }
    class parse_type_after_colon {
        <<function>>
    }
    Function ..> parse_name_type_pairs : uses
    parse_name_type_pairs ..> parse_type_expr : uses
    Function ..> parse_type_after_colon : uses
    Relation ..> parse_name_type_pairs : uses
```

## Parameter list parsing

`parse_name_type_pairs` walks the token stream produced for the parameter list.
Each time a colon is encountered, the function delegates to `parse_type_expr` to
capture the following type expression. That helper recursively parses nested
delimiters, so constructs like `Vec<Map<string, Vec<u8>>>` are handled without
maintaining a delimiter stack in `parse_name_type_pairs` itself. Parameters end
when a comma or the closing `)` of the list is reached.

Missing colons between a parameter name and type trigger a
`ParseError::MissingColon`. The parser attaches the span of the comma or
parenthesis that ended the parameter, so diagnostics can highlight the exact
location of the issue. Helper functions `collect_parameter_name` and
`finalise_parameter` keep the main loop small by handling name collection and
type parsing respectively.
