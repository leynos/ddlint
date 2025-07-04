# Function Parsing Design

This document outlines the strategy for parsing `function` definitions and
declarations within `ddlint`. The parser relies on small helpers to interpret
parameter lists and optional return types. These helpers are shared with
relation parsing to avoid duplication.

```mermaid
classDiagram
    class parse_name_type_pairs {
        <<function>>
    }
    class parse_type_after_colon {
        <<function>>
    }
    Function ..> parse_name_type_pairs : uses
    Function ..> parse_type_after_colon : uses
    Relation ..> parse_name_type_pairs : uses
```
