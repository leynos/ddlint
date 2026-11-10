# Migration guide: v0.1.0 to v0.2.0

Version 0.2.0 replaces the string binder list returned by
`Relation::primary_key()` with a typed primary-key clause. This guide moves
callers from the old `Vec<String>` shape to the new accessors.

The crate manifests still report `0.1.0` until the release is cut; the changes
described here are the breaking changes recorded under `Unreleased` in the
[changelog](../CHANGELOG.md).

## Who should read this

Read this guide before upgrading code that calls `Relation::primary_key()`, or
that slices the source text following a `primary key (...)` clause. The
primary-key change described here needs no action from code that only uses the
linter or the command line, but 0.2.0 carries other breaking changes that do
apply to those users. In particular, the legacy `DDlog` tokens that 0.2.0
rejects — `typedef`, `bigint`, `bit`, `double`, `float`, `signed`, bare `#`, and
`<=>` — must be migrated before `Runner::run` returns lint diagnostics again.
Read the [changelog](../CHANGELOG.md) for the full list before upgrading.

## What changed

- `Relation::primary_key()` returns
  `Result<Option<PrimaryKey>, RelationParseErrors>` rather than
  `Result<Option<Vec<String>>, RelationParseErrors>`.
- `PrimaryKey` exposes `binders()`, `binder_names()`, `span()`, and
  `expression()`.
- `PrimaryKeyBinder` exposes `name()` and a source-absolute `span()`.
- `PrimaryKeyExpression` exposes `text()`, `span()`, and a lazy `parse()`.

`PrimaryKey` owns its binders, the expression text, and every span. It does not
borrow the relation node, so it may outlive the parsed tree. The names returned
by `binder_names()` borrow from the `PrimaryKey`, not from the relation node;
copy them when the `PrimaryKey` itself is dropped.

## Replace the binder list

The binder list is the only part that changed shape. Comparisons against owned
strings become comparisons against borrowed names.

Before:

```rust,no_run
let names: Option<Vec<String>> = relation.primary_key()?;
if let Some(names) = names {
    assert_eq!(names, vec!["row".to_owned()]);
}
```

After:

```rust,no_run
if let Some(key) = relation.primary_key()? {
    assert_eq!(key.binder_names(), ["row"]);
}
```

Callers that need owned names, such as a value stored in a struct, bind the
`PrimaryKey` and map the borrowed names themselves:

```rust,no_run
let key = relation.primary_key()?.expect("primary key clause");
let owned: Vec<String> = key
    .binder_names()
    .into_iter()
    .map(str::to_owned)
    .collect();
```

## Read binder and clause spans

Previously a caller had to search the source for each name. With a `PrimaryKey`
in hand, every binder and the clause itself carry source-absolute spans:

```rust,no_run
for binder in key.binders() {
    assert_eq!(src.get(binder.span()), Some(binder.name()));
}
assert!(src.get(key.span()).is_some_and(|text| text.starts_with("primary key (")));
```

## Capture the spec-form expression

A clause may carry an expression after the binder list, as in
`primary key (row) (row.author, row.title)`. The scanner preserves it as CST
text, so no caller needs to re-scan for it.

```rust,no_run
let key = relation.primary_key()?.expect("clause");
assert_eq!(key.binder_names(), ["row"]);
let expression = key.expression().expect("spec-form expression");
assert_eq!(expression.text(), "(row.author, row.title)");
assert_eq!(src.get(expression.span()), Some(expression.text()));
```

Parsing the expression is a separate, lazy step. `parse()` reports errors at
source-absolute spans and never hides the binders, so a caller can report a
malformed expression while still using the binder list:

```rust,no_run
match expression.parse() {
    Ok(expr) => println!("{expr:?}"),
    Err(errors) => {
        for error in errors {
            eprintln!("{error:?} at {:?}", error.span());
        }
    }
}
```

## Scanner behaviour changes

Two scanner boundaries changed alongside the API:

- A `;` at depth zero now terminates a primary-key clause instead of being
  absorbed into its expression, so a declaration that follows on the same line
  is still scanned. A semicolon inside nested delimiters is not a statement
  boundary and stays part of the expression.
- A binder list that is not a comma-separated list of identifiers, such as
  `(x.y)` or `(x,)`, reports `D-REL-007` instead of being accepted.

Recovery from unbalanced delimiters has not changed. A clause such as
`primary key (x) (` reports `D-REL-007`, and recovery skips the line. No
relation is recorded for that source, so `primary_key()` has no clause to
return. That behaviour predates 0.2.0.

No diagnostic code or message changed; only the inputs that reach `D-REL-007`
did. See the [users' guide](./users-guide.md#relation-diagnostics) for the full
table.

## What has not changed

- `RelationRole`, `RelationKind`, `RelationBody`, and the keyword-presence
  accessors keep their signatures and behaviour.
- No CST node is added. The expression remains the non-trivia hull of the
  existing `N_RELATION_DECL` tokens after the binder list.
- The syntax layer still does not decide whether a binder names a row or a
  column; that remains semantic-layer work.
