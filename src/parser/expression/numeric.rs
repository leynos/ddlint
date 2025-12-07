//! Numeric literal parsing and validation.
//!
//! This module recognises integer and floating-point literals, including
//! width-qualified forms such as `8'hFF` and `3.14'f32`. It performs basic
//! validation (positive widths, integer fit checks, and supported float
//! widths) and returns structured literal representations for the AST.

use crate::parser::ast::{FloatLiteral, IntBase, IntLiteral, NumberLiteral};
use num_bigint::BigInt;
use num_traits::{One, Signed};

/// Error returned when a numeric literal fails validation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumericLiteralError {
    /// Width segment could not be parsed as an integer.
    InvalidWidth(String),
    /// Width was parsed but evaluates to zero or a negative value.
    NonPositiveWidth(u32),
    /// A width-qualified literal omitted its base (e.g. `8'`).
    MissingBase,
    /// Base marker was not recognised.
    InvalidBase(char),
    /// Literal did not include any digits.
    MissingDigits,
    /// Digits were not valid for the detected base.
    InvalidDigits(String),
    /// Integer did not fit the declared width.
    IntOutOfRange {
        width: u32,
        signed: bool,
        value: BigInt,
    },
    /// Unsigned literal used a negative value.
    NegativeUnsigned { width: u32, value: BigInt },
    /// Float width other than 32 or 64 bits.
    UnsupportedFloatWidth(String),
}

impl NumericLiteralError {
    #[must_use]
    pub fn message(&self) -> String {
        match self {
            Self::InvalidWidth(w) => format!("invalid numeric width '{w}'"),
            Self::NonPositiveWidth(w) => format!("numeric width must be positive (found {w})"),
            Self::MissingBase => "width-qualified literal is missing a base".to_string(),
            Self::InvalidBase(b) => format!("invalid numeric base '{b}'"),
            Self::MissingDigits => "numeric literal is missing digits".to_string(),
            Self::InvalidDigits(digits) => {
                format!("invalid digits '{digits}' for numeric literal")
            }
            Self::IntOutOfRange {
                width,
                signed,
                value,
            } => {
                let kind = if *signed { "signed" } else { "unsigned" };
                format!("value {value} does not fit {kind} width {width}")
            }
            Self::NegativeUnsigned { width, value } => {
                format!("unsigned width {width} cannot encode negative value {value}")
            }
            Self::UnsupportedFloatWidth(suffix) => {
                format!("unsupported float width '{suffix}': only 'f32 and 'f64 are allowed")
            }
        }
    }
}

/// Parse a numeric literal into a structured representation.
///
/// # Errors
/// Returns a [`NumericLiteralError`] when the literal uses an invalid width,
/// base, or value that does not fit the declared width, or when a float width
/// other than `32`/`64` is specified.
pub fn parse_numeric_literal(src: &str) -> Result<NumberLiteral, NumericLiteralError> {
    if let Some((width_part, rest)) = src.split_once('\'') {
        if let Some(suffix) = rest.strip_prefix(['f', 'F']) {
            return parse_width_float(src, suffix);
        }
        return parse_width_qualified_literal(src, width_part, rest);
    }
    parse_unqualified_literal(src)
}

fn parse_width_qualified_literal(
    raw: &str,
    width_part: &str,
    rest: &str,
) -> Result<NumberLiteral, NumericLiteralError> {
    let width = parse_width(width_part)?;
    parse_width_int(raw, width, rest)
}

fn parse_width_float(raw: &str, suffix: &str) -> Result<NumberLiteral, NumericLiteralError> {
    let float_width = match suffix {
        "32" => 32,
        "64" => 64,
        other => {
            return Err(NumericLiteralError::UnsupportedFloatWidth(
                other.to_string(),
            ));
        }
    };
    Ok(NumberLiteral::Float(FloatLiteral {
        raw: raw.to_string(),
        width: Some(float_width),
    }))
}

fn parse_width_int(
    raw: &str,
    width: u32,
    rest: &str,
) -> Result<NumberLiteral, NumericLiteralError> {
    let (signed, base_and_digits) = rest
        .strip_prefix(['s', 'S'])
        .map_or((false, rest), |digits| (true, digits));
    let (base, digits) = split_base_digits(base_and_digits)?;
    let literal = build_int_literal(raw, Some(width), signed, base, &digits)?;
    Ok(NumberLiteral::Int(literal))
}

fn parse_unqualified_literal(src: &str) -> Result<NumberLiteral, NumericLiteralError> {
    let (base, digits) = classify_int_base(src);
    if base == IntBase::Decimal && looks_like_decimal_float(src) {
        return Ok(NumberLiteral::Float(FloatLiteral {
            raw: src.to_string(),
            width: None,
        }));
    }
    let literal = build_int_literal(src, None, false, base, digits)?;
    Ok(NumberLiteral::Int(literal))
}

fn parse_width(width_part: &str) -> Result<u32, NumericLiteralError> {
    let width = width_part
        .parse::<u32>()
        .map_err(|_| NumericLiteralError::InvalidWidth(width_part.to_string()))?;
    if width == 0 {
        return Err(NumericLiteralError::NonPositiveWidth(width));
    }
    Ok(width)
}

fn split_base_digits(input: &str) -> Result<(IntBase, String), NumericLiteralError> {
    let mut chars = input.chars();
    let base_char = chars.next().ok_or(NumericLiteralError::MissingBase)?;
    let base = match base_char {
        'b' | 'B' => IntBase::Binary,
        'o' | 'O' => IntBase::Octal,
        'd' | 'D' => IntBase::Decimal,
        'h' | 'H' => IntBase::Hex,
        other => return Err(NumericLiteralError::InvalidBase(other)),
    };
    let digits: String = chars.collect();
    if digits.is_empty() {
        return Err(NumericLiteralError::MissingDigits);
    }
    Ok((base, digits))
}

fn classify_int_base(src: &str) -> (IntBase, &str) {
    if let Some(rest) = src.strip_prefix("0x").or_else(|| src.strip_prefix("0X")) {
        return (IntBase::Hex, rest);
    }
    if let Some(rest) = src.strip_prefix("0b").or_else(|| src.strip_prefix("0B")) {
        return (IntBase::Binary, rest);
    }
    if let Some(rest) = src.strip_prefix("0o").or_else(|| src.strip_prefix("0O")) {
        return (IntBase::Octal, rest);
    }
    (IntBase::Decimal, src)
}

fn build_int_literal(
    raw: &str,
    width: Option<u32>,
    signed: bool,
    base: IntBase,
    digits: &str,
) -> Result<IntLiteral, NumericLiteralError> {
    let cleaned: String = digits.chars().filter(|c| *c != '_').collect();
    if cleaned.is_empty() {
        return Err(NumericLiteralError::MissingDigits);
    }
    let value = parse_int_value(&cleaned, base)?;
    if let Some(width_bits) = width {
        validate_int_range(width_bits, signed, &value)?;
    }
    Ok(IntLiteral {
        raw: raw.to_string(),
        width,
        base,
        signed,
        value,
    })
}

fn parse_int_value(cleaned: &str, base: IntBase) -> Result<BigInt, NumericLiteralError> {
    let (is_negative, magnitude) = cleaned.strip_prefix('-').map_or_else(
        || {
            cleaned
                .strip_prefix('+')
                .map_or((false, cleaned), |rest| (false, rest))
        },
        |rest| (true, rest),
    );

    if magnitude.is_empty() {
        return Err(NumericLiteralError::MissingDigits);
    }

    let value = BigInt::parse_bytes(magnitude.as_bytes(), base.radix())
        .ok_or_else(|| NumericLiteralError::InvalidDigits(cleaned.to_string()))?;
    let value = if is_negative { -value } else { value };
    Ok(value)
}

fn validate_int_range(width: u32, signed: bool, value: &BigInt) -> Result<(), NumericLiteralError> {
    if signed {
        let shift = usize::try_from(width.saturating_sub(1))
            .map_err(|_| NumericLiteralError::NonPositiveWidth(width))?;
        let bound = BigInt::one() << shift;
        let min = -bound.clone();
        let max = bound - BigInt::one();
        if value < &min || value > &max {
            return Err(NumericLiteralError::IntOutOfRange {
                width,
                signed,
                value: value.clone(),
            });
        }
        return Ok(());
    }

    if value.is_negative() {
        return Err(NumericLiteralError::NegativeUnsigned {
            width,
            value: value.clone(),
        });
    }
    let shift = usize::try_from(width).map_err(|_| NumericLiteralError::NonPositiveWidth(width))?;
    let max = (BigInt::one() << shift) - BigInt::one();
    if value > &max {
        return Err(NumericLiteralError::IntOutOfRange {
            width,
            signed,
            value: value.clone(),
        });
    }
    Ok(())
}

fn looks_like_decimal_float(src: &str) -> bool {
    src.contains('.')
        || src.contains('e')
        || src.contains('E')
        || src.contains("inf")
        || src.contains("NaN")
}
