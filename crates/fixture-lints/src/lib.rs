//! Lint-scoped attribute helper for `rstest` fixture expansions.
//!
//! A fixture written as a single expression on one line — whether by hand or
//! by a formatter configured with `fn_single_line` — is re-emitted by `rstest`
//! inside generated code as `{ { body } }`, and rustc's `unused_braces` lint
//! reports the fixture's own braces as redundant. The diagnostic is a
//! formatting artefact rather than a defect in the fixture, so this attribute
//! adds a narrowly scoped `allow` before `rstest` expands the item.
//!
//! `unused_braces` is a rustc lint, not a Clippy lint. The attribute is
//! deliberately limited to `rstest` fixture items and must be applied above
//! `#[fixture]`:
//!
//! ```ignore
//! #[allow_fixture_expansion_lints]
//! #[fixture]
//! fn source() -> &'static str { "input relation R(x: u32);" }
//! ```
//!
//! The macro emits a reasoned `allow` only when the fixture body is exactly one
//! expression statement without a trailing semicolon. Multi-statement fixtures
//! are returned unchanged, so a genuine `unused_braces` finding still surfaces.
//! It cannot use `#[expect]`: `rstest` drops user-written expectations, and a
//! macro-emitted expectation is silently unchecked because
//! `report_in_external_macro` is false. The attribute must not be used to
//! suppress lints in ordinary functions or modules.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

/// Return whether `item` is a function whose body is a single expression
/// statement.
///
/// A trailing semicolon turns the statement into an expression with a `Some`
/// terminator, so it does not qualify. Unparseable items also return `false`,
/// which keeps the macro transparent for input it does not recognise.
///
/// ```ignore
/// // One expression, no semicolon: `true`.
/// fn source() -> u8 { 1 }
///
/// // Multi-statement body: `false`.
/// fn source() -> u8 {
///     let value = 1;
///     value
/// }
/// ```
fn has_single_expression_body(item: &TokenStream2) -> bool {
    syn::parse2::<syn::ItemFn>(item.clone())
        .is_ok_and(|function| matches!(function.block.stmts.as_slice(), [syn::Stmt::Expr(_, None)]))
}

/// Allow `unused_braces` in the expansion of the annotated `rstest` fixture.
///
/// The generated `allow` carries a reason so the repository's
/// `allow_attributes_without_reason` policy remains satisfied. Because the
/// attribute is emitted by an external macro, Clippy's `allow_attributes` lint
/// does not require it to be an `expect`.
///
/// Items that are not a single-expression function, and items that cannot be
/// parsed as a function, pass through unchanged. That narrowing leaves genuine
/// `unused_braces` findings in multi-statement fixtures visible to the
/// compiler.
///
/// # Parameters
///
/// - `_attribute`: the attribute arguments, which this macro ignores.
/// - `item`: the item to inspect and optionally annotate.
///
/// # Returns
///
/// The item token stream, optionally prefixed with a scoped `unused_braces`
/// allowance carrying the reason.
///
/// # Examples
///
/// ```rust
/// use fixture_lints::allow_fixture_expansion_lints;
/// use rstest::fixture;
///
/// #[allow_fixture_expansion_lints]
/// #[fixture]
/// fn source() -> &'static str { "input" }
/// ```
#[proc_macro_attribute]
pub fn allow_fixture_expansion_lints(_attribute: TokenStream, item: TokenStream) -> TokenStream {
    let tokens = TokenStream2::from(item);
    if has_single_expression_body(&tokens) {
        quote! {
            #[allow(unused_braces, reason = "rstest fixture expansion")]
            #tokens
        }
        .into()
    } else {
        tokens.into()
    }
}
