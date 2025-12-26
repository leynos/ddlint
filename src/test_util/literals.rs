//! Helpers for constructing literal expression nodes in tests.

use crate::parser::ast::{Expr, Literal, StringKind, StringLiteral};
use crate::parser::expression::parse_numeric_literal;

/// Newtype wrapper for numeric literal text.
///
/// Provides type safety for numeric literal source text passed to [`lit_num`].
#[derive(Debug, Clone)]
pub struct NumericText(String);

impl From<&str> for NumericText {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl AsRef<str> for NumericText {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Newtype wrapper for string literal body content.
///
/// Provides type safety for string body content passed to string literal
/// constructors.
#[derive(Debug, Clone)]
pub struct StringBody(String);

impl From<&str> for StringBody {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl AsRef<str> for StringBody {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Construct a numeric [`Expr::Literal`].
#[must_use]
pub fn lit_num(n: impl Into<NumericText>) -> Expr {
    let text: NumericText = n.into();
    let src = text.as_ref();
    let literal = parse_numeric_literal(src)
        .unwrap_or_else(|err| panic!("failed to parse numeric literal '{src}': {}", err.message()));
    Expr::Literal(Literal::Number(literal))
}

/// Generates a string literal helper function.
macro_rules! string_literal_helper {
    ($name:ident, $kind:ident, $interpolated:expr, $interned:expr, $doc:expr) => {
        #[doc = $doc]
        #[must_use]
        pub fn $name(s: impl Into<StringBody>) -> Expr {
            let body: StringBody = s.into();
            string_literal(
                body.as_ref(),
                StringKind::$kind {
                    interpolated: $interpolated,
                },
                $interned,
            )
        }
    };
}

string_literal_helper!(
    lit_str,
    Standard,
    false,
    false,
    "Construct a string [`Expr::Literal`]."
);

string_literal_helper!(
    lit_interpolated_str,
    Standard,
    true,
    false,
    "Construct an interpolated string [`Expr::Literal`]."
);

string_literal_helper!(
    lit_raw_str,
    Raw,
    false,
    false,
    "Construct a raw string [`Expr::Literal`]."
);

string_literal_helper!(
    lit_raw_interpolated_str,
    Raw,
    true,
    false,
    "Construct a raw interpolated string [`Expr::Literal`]."
);

string_literal_helper!(
    lit_interned_str,
    Standard,
    false,
    true,
    "Construct an interned standard string [`Expr::Literal`]."
);

string_literal_helper!(
    lit_interned_raw_str,
    Raw,
    false,
    true,
    "Construct an interned raw string [`Expr::Literal`]."
);

string_literal_helper!(
    lit_interned_raw_interpolated_str,
    Raw,
    true,
    true,
    "Construct an interned interpolated raw string [`Expr::Literal`]."
);

fn string_literal(body: &str, kind: StringKind, interned: bool) -> Expr {
    Expr::Literal(Literal::String(StringLiteral {
        body: body.to_string(),
        kind,
        interned,
    }))
}

/// Construct a boolean [`Expr::Literal`].
#[must_use]
pub fn lit_bool(b: bool) -> Expr {
    Expr::Literal(Literal::Bool(b))
}
