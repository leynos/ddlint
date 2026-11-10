# Migration guide: v0.1.0 to v0.2.0

Version 0.2.0 replaces the string binder list returned by
`Relation::primary_key()` with a typed primary-key clause. This guide moves
callers from the old `Vec<String>` shape to the new accessors.

The crate manifests still report `0.1.0` until the release is cut; the changes
described here are the breaking changes recorded under `Unreleased` in the
[changelog](../CHANGELOG.md).

## Who should read this

Read this guide before upgrading code that calls `Relation::primary_key()`, or
that slices the source text following a `primary key (...)` clause. Sources
that only use the linter or the command line need no change.

## What changed

- `Relation::primary_key()` returns
  `Result<Option<PrimaryKey>, RelationParseErrors>` rather than
  `Result<Option<Vec<String>>, RelationParseErrors>`.
- `PrimaryKey` exposes `binders()`, `binder_names()`, `span()`, and
  `expression()`.
- `PrimaryKeyBinder` exposes `name()` and a source-absolute `span()`.
- `PrimaryKeyExpression` exposes `text()`, `span()`, and a lazy `parse()`.

The typed clause borrows the relation node, so it lives no longer than the
parsed tree. Copy the names when they must outlive it.

## Replace the binder list

The binder list is the only part that changed shape. Comparisons against owned
strings become comparisons against borrowed names.

Before:

```rust
let names: Option<Vec<String>> = relation.primary_key()?;
if let Some(names) = names {
    assert_eq!(names, vec!["row".to_owned()]);
}
```

After:

```rust
let key = relation.primary_key()?;
if let Some(key) = key {
    assert_eq!(key.binder_names(), ["row"]);
}
```

Callers that need owned names, such as a value stored in a struct, map the
borrowed names themselves:

```rust
let owned: Vec<String> = key
    .binder_names()
    .into_iter()
    .map(str::to_owned)
    .collect();
```

## Read binder and clause spans

Previously a caller had to search the source for each name. Every binder and
the clause itself now carry source-absolute spans:

```rust
for binder in key.binders() {
    assert_eq!(src.get(binder.span()), Some(binder.name()));
}
assert!(src.get(key.span()).is_some_and(|text| text.starts_with("primary key (")));
```

## Capture the spec-form expression

A clause may carry an expression after the binder list, as in
`primary key (row) (row.author, row.title)`. The scanner preserves it as CST
text, so no caller needs to re-scan for it.

```rust
let key = relation.primary_key()?.expect("clause");
assert_eq!(key.binder_names(), ["row"]);
let expression = key.expression().expect("spec-form expression");
assert_eq!(expression.text(), "(row.author, row.title)");
assert_eq!(src.get(expression.span()), Some(expression.text()));
```

Parsing the expression is a separate, lazy step. `parse()` reports errors at
source-absolute spans and never hides the binders, so a caller can report a
malformed expression while still using the binder list:

```rust
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

Three scanner boundaries changed alongside the API:

- A `;` now terminates a primary-key clause instead of being absorbed into its
  expression, so a declaration that follows on the same line is still scanned.
- A binder list that is not a comma-separated list of identifiers, such as
  `(x.y)` or `(x,)`, reports `D-REL-007` instead of being accepted.
- A clause with unbalanced delimiters, such as `primary key (x) (`, reports
  `D-REL-007`, and recovery skips the line. No relation is recorded for that
  source, so `primary_key()` has no clause to return.

The diagnostics themselves are unchanged; see the
[users' guide](./users-guide.md#relation-diagnostics) for the full table.

## What has not changed

- `RelationRole`, `RelationKind`, `RelationBody`, and the keyword-presence
  accessors keep their signatures and behaviour.
- No CST node is added. The expression remains the non-trivia hull of the
  existing `N_RELATION_DECL` tokens after the binder list.
- The syntax layer still does not decide whether a binder names a row or a
  column; that remains semantic-layer work.
