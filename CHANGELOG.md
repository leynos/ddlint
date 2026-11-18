# Changelog

## Unreleased

### Breaking changes

- Change `Relation::primary_key()` to return
  `Result<Option<PrimaryKey>, RelationParseErrors>` instead of
  `Result<Option<Vec<String>>, RelationParseErrors>`. `PrimaryKey` exposes
  `binders()`, `binder_names()`, `span()`, and `expression()`;
  `PrimaryKeyBinder` adds `name()` and a source-absolute span, and
  `PrimaryKeyExpression` adds `text()`, `span()`, and a lazy `parse()` that
  reports source-absolute error spans. Replace comparisons against
  `Vec<String>` with `binder_names()`.
- Correct two primary-key scanner boundaries. A `;` now terminates the key
  clause instead of being absorbed into its expression, and a following
  declaration on the same line is scanned again. Binder lists that are not a
  comma-separated list of identifiers, such as `(x.y)` or `(x,)`, now report
  D-REL-007 instead of being accepted by the scanner.

  The [v0.2.0 migration guide](docs/v0-2-0-migration-guide.md) shows the
  before-and-after call shapes.
- Reject legacy DDlog tokens that previously had inconsistent parser treatment:
  `typedef`, `bigint`, `bit`, `double`, `float`, `signed`, bare `#`, and `<=>`.
  Use `type`, sized integer types such as `u32`/`i64`, `f32`/`f64`, and
  `#[...]` attributes instead.

  Sources containing these forms now receive parser errors, and `Runner::run`
  returns no lint diagnostics until the syntax is migrated.

  For example, `typedef Foo = u32;` is rejected with:

  ```text
  `typedef` is a legacy DDlog keyword; use `type` instead
  ```

### Changed

- Braces now always parse as block expressions. `{ a; b }` is a block with `a`
  as a statement and `b` as its value; a trailing `;` discards the value, and
  `{}` is the empty block. Brace groups, brace map literals, and the
  colon-terminating map-key mode are removed.
- A block item that is followed by a comma or by another item without `;`
  reports `expected ';' or '}' after a block item`; an empty item reports
  `empty statement in block: remove the extra ';'`; an unterminated block
  reports `expected '}' to close block`.
- Map literals use brackets: `[k -> v]`. An empty vector literal `[]` is
  rejected, and a literal that mixes vector elements and map entries is
  rejected.

### Added

- `Expr::Block(BlockExpr)` for brace-delimited expressions. `BlockExpr` exposes
  `statements`, `value`, `items()`, `into_items()`, and `ends_with_semicolon()`.
