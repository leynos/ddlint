# Haskell Parser Analysis

This document summarizes the design of the parser implemented in
`Language.DifferentialDatalog.Parse`. The original code is written in Haskell
using Parsec. This overview highlights token definitions, entry points, major
grammar rules, and the AST structures they construct. The notes provide a
reference for porting the parser to Rust using `chumsky` and `rowan` as
described in the other design documents.

## Token Definitions

Tokens are defined using Parsec's `TokenParser` facilities. The parser
recognises two sets of keywords:

- **Rust keywords** – reserved to avoid conflicts with future Rust code.
- **DDlog keywords** – language syntax. These include type names and control
  constructs.

The lists of keywords appear in the Haskell source:

```haskell
rustKeywords =
    [ "abstract", "async", "await", "become", "box"
    , "const", "crate", "do", "dyn", "final", "fn"
    , "impl", "let", "loop", "macro", "match", "mod"
    , "move", "override", "priv", "pub", "ref", "self"
    , "Self", "static", "struct", "super", "trait"
    , "try", "type", "typeof", "unsafe", "unsized"
    , "use", "virtual", "where", "while", "yield"
    ]

-- Datalog keywords
ddlogKeywords =
    [ "_", "Aggregate", "and", "apply", "as", "bigint"
    , "bit", "bool", "break", "continue", "double"
    , "else", "extern", "false", "FlatMap", "float"
    , "for", "function", "if", "import", "in", "input"
    , "Inspect", "multiset", "mut", "not", "or", "output"
    , "relation", "return", "signed", "skip", "stream"
    , "string", "transformer", "true", "typedef", "var"
    ]
```

【F:Parse.hs†L70-L96】

Operators and punctuation recognised as single tokens are listed under
`reservedOpNames`:

```haskell
reservedOpNames =
    [ ":", "::", "|", "&", "==", "=", ":-", "%", "*"
    , "/", "+", "-", ".", "->", "=>", "<=", "<=>"
    , ">=", "<", ">", "!=", ">>", "<<", "~", "@", "#"
    , "'"
    ]
```

【F:Parse.hs†L97-L109】

The token parser configuration defines how comments, identifiers, and other
lexical items are recognised:

```haskell
ccnDef = emptyDef { T.commentStart      = "/*"
                  , T.commentEnd        = "*/"
                  , T.commentLine       = "//"
                  , T.nestedComments    = True
                  , T.identStart        = letter <|> char '_'
                  , T.identLetter       = alphaNum <|> char '_'
                  , T.reservedOpNames   = reservedOpNames
                  , T.reservedNames     = reservedNames
                  , T.opLetter          = oneOf "!:%*-+./=|<>"
                  , T.caseSensitive     = True }
```

【F:Parse.hs†L110-L120】

Helper parsers such as `parens`, `braces`, and `identifier` are generated from
this configuration.

`reservedNames` is defined simply as `ddlogKeywords ++ rustKeywords`, so
downstream code must not reuse any of those identifiers.

## Parser Entry Points

The module exposes `parseDatalogString` as the main entry point. This wraps
Parsec and returns either a `DatalogProgram` or an error:

```haskell
parseDatalogString :: String -> String -> ExceptT String IO DatalogProgram
parseDatalogString program file = do
  case parse datalogGrammar file program of
       Left  e    -> throwE $ "failed to parse input file: " ++ show e
       Right prog -> return prog { progSources = M.singleton file program }
```

【F:Parse.hs†L57-L66】

Two grammar roots are provided:

```haskell
datalogGrammar = removeTabs *> ((optional whiteSpace) *> spec <* eof)
exprGrammar    = removeTabs *> ((optional whiteSpace) *> expr <* eof)
```

【F:Parse.hs†L214-L215】

`datalogGrammar` parses an entire source file while `exprGrammar` parses an
isolated expression. Both delegate to individual rules described below.

`parseDatalogString` wraps the Parsec `parse` function in `ExceptT`. This
ensures the caller receives a clear error message when parsing fails and that IO
exceptions remain separated from parse errors.

## Grammar Productions and AST Mapping

Each parser rule uses `withPos` to attach source locations. The rules construct
values from `Language.DifferentialDatalog.Syntax`, providing an explicit AST.
The top-level `spec` rule gathers a list of `SpecItem` values, then builds a
`DatalogProgram`:

```haskell
spec = do
    items <- concat <$> many decl
    let imports = mapMaybe (\case SpImport i -> Just i; _ -> Nothing) items
    let relations = mapMaybe (\case SpRelation r -> Just (name r, r); _ -> Nothing) items
    let indexes = mapMaybe (\case SpIndex i -> Just (name i, i); _ -> Nothing) items
    let types = mapMaybe (\case SpType t -> Just (name t, t); _ -> Nothing) items
    let funcs = mapMaybe (\case SpFunc f -> Just (name f, f); _ -> Nothing) items
    let transformers = mapMaybe (\case SpTransformer t -> Just (name t, t); _ -> Nothing) items
    let rules = mapMaybe (\case SpRule r -> Just r; _ -> Nothing) items
    let applys = mapMaybe (\case SpApply a -> Just a; _ -> Nothing) items
    let program = DatalogProgram { progImports      = imports
                                 , progTypedefs     = M.fromList types
                                 , progFunctions    = M.fromList $ groupSort funcs
                                 , progTransformers = M.fromList transformers
                                 , progRelations    = M.fromList relations
                                 , progIndexes      = M.fromList indexes
                                 , progRules        = rules
                                 , progApplys       = applys
                                 , progSources      = M.empty }
```

【F:Parse.hs†L222-L254】

### Declarations

`decl` recognises one of several declaration forms, each constructing a specific
AST node (`Import`, `TypeDef`, `Relation`, `Index`, `Function`, `Transformer`,
`Rule` or `Apply`). Attributes encountered before the item are attached to the
resulting node when applicable.

```haskell
decl =  do attrs <- attributes
           items <- (withPosMany $
                         (return . SpImport)         <$> imprt
                     <|> (return . SpType)           <$> typeDef
                     <|> relation
                     <|> (return . SpIndex)          <$> index
                     <|> (return . SpFunc)           <$> func
                     <|> (return . SpTransformer)    <$> transformer
                     <|> (return . SpRule)           <$> rule
                     <|> (return . SpApply)          <$> apply)
                   <|> (map SpRule . convertStatement) <$> parseForStatement
```

【F:Parse.hs†L262-L276】

Other notable grammar rules include:

- `imprt` – parses an import statement and yields an `Import` AST node.
- `typeDef` – handles regular and `extern` type definitions, producing `TypeDef`
  values.
- `func` – parses function definitions, optionally with a body.
- `transformer` – restricted to `extern` forms, returning a `Transformer` node.
- `index` – defines an index on a relation.
- `relation` – parses a relation declaration and its optional primary key.
- `rule` – parses a rule head followed by an optional list of body clauses.
- `statement` and its helpers – parse imperative statements used within rules.
- `expr` – an expression parser built via `buildExpressionParser`; it handles
  literals, operators and function calls.

### Control-Flow Statements

The parser contains dedicated rules for imperative constructs used in rules:

```haskell
parseForStatement = withPos $
                    ForStatement nopos <$ reserved "for"
                                       <*> (symbol "(" *> atom False)
                                       <*> (optionMaybe (reserved "if" *> expr))
                                       <*> (symbol ")" *> statement)

parseIfStatement = IfStatement nopos <$ reserved "if"
                                    <*> (parens expr) <*> statement
                                    <*> (optionMaybe (reserved "else" *> statement))
```

【F:Parse.hs†L364-L395】

`parseForStatement` recognises `for (pat in expr)` loops with an optional `if`
guard, while `parseIfStatement` handles conditional branching with an optional
`else` clause. Both return structured statement nodes used by the rule parser.

### Expression Grammar

The `expr` rule is defined through `buildExpressionParser` and a series of
helper terms:

```haskell
expr = buildExpressionParser etable term
    <?> "expression"

term  =  elhs
     <|> (withPos $ eTuple <$> (parens $ commaSepEnd expr))
     <|> braces eseq
     <|> term'
     <?> "expression term"
term' = withPos $
         ebinding
     <|> epholder
     <|> estruct
     <|> enumber
     <|> ebool
     <|> estring
     <|> einterned_string
     <|> efunc
     <|> evar
     <|> ematch
     <|> eite
     <|> efor
     <|> evardcl
     <|> econtinue
     <|> ebreak
     <|> ereturn
     <|> emap_literal
     <|> evec_literal
     <|> eclosure
```

【F:Parse.hs†L566-L611】

The table `etable` defines operator precedence, including assignment and logical
connectives. Complex features such as group-by extraction are handled in helper
functions like `extractGroupBy`.

### Supporting Utilities

Two helper mechanisms appear throughout the parser. `removeTabs` sanitises the
input by replacing any tab character with a single space:

```haskell
removeTabs = do s <- getInput
                let s' = map (\c -> if c == '\t' then ' ' else c ) s
                setInput s'
```

【F:Parse.hs†L182-L186】

Position information is attached using the `withPos` combinator from the `Pos`
module:

```haskell
withPos x = (\ s a e -> atPos a (s,e)) <$> getPosition <*> x <*> getPosition
```

【F:Pos.hs†L92-L94】

`withPosMany` performs the same operation for lists of nodes. These helpers
allow downstream analyses to preserve accurate source spans.

Each rule constructs an appropriate structure from
`Language.DifferentialDatalog.Syntax`, ensuring that positions and attributes
are preserved.

## Lexical Elements

The complete set of lexical tokens derived from the parser includes:

- **Keywords** – the union of `ddlogKeywords` and `rustKeywords`.
- **Operators** – all strings in `reservedOpNames` such as `::`, `=>`, `==`,
  `>=`, and so on.
- **Punctuation** – parentheses, brackets, braces, commas, semicolons, dots and
  colons as provided by the `TokenParser` helpers.
- **Comments** – block comments delimited by `/*` and `*/` and line comments
  starting with `//`.
- **Identifiers** – parsed using `identifier`, `lcIdentifier` and `ucIdentifier`
  which enforce naming rules for variables, types and constructors.

These lexical elements will translate directly into `SyntaxKind` token variants
in the Rust implementation.

## Porting Notes

When porting to Rust, the constructs above can be expressed with `chumsky`
combinators and a `rowan` CST. The `expr` grammar maps well to
`chumsky::recursive`, while the statement parsers become small combinators that
emit both AST nodes and CST events. Group-by extraction can be implemented using
a post-processing step mirroring `extractGroupBy`. Patterns for `match` and
`for` loops translate to nested `chumsky` parsers building structured nodes.

Keep this file in lockstep with `Parse.hs` when changes land so that the Rust
implementation remains accurate.

______________________________________________________________________
