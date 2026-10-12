# Changelog

## Unreleased

### Breaking changes

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
