//! Thin domain-string wrappers for numeric literal parsing.
//!
//! These newtypes provide type safety for the various string components
//! involved in parsing numeric literals, preventing accidental mixing of
//! raw literals, width text, suffixes, and digit strings.

use derive_more::{AsRef, From};

/// Raw source text preserved for AST.
#[derive(Debug, Clone, PartialEq, Eq, AsRef, From)]
#[as_ref(str)]
#[from(&str)]
pub struct RawLiteral(String);

impl RawLiteral {
    /// Returns the raw literal text as a string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Width component before apostrophe.
#[derive(Debug, Clone, PartialEq, Eq, AsRef, From)]
#[as_ref(str)]
#[from(&str)]
pub struct WidthText(String);

impl WidthText {
    /// Returns the width text as a string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Float width suffix (32/64).
#[derive(Debug, Clone, PartialEq, Eq, AsRef, From)]
#[as_ref(str)]
#[from(&str)]
pub struct FloatSuffix(String);

impl FloatSuffix {
    /// Returns the float suffix as a string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Digit characters for parsing.
#[derive(Debug, Clone, PartialEq, Eq, AsRef, From)]
#[as_ref(str)]
#[from(&str)]
pub struct DigitString(String);

impl DigitString {
    /// Constructs a new digit string from any string-like value.
    #[inline]
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    /// Returns the digit string as a string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Returns a new digit string with all underscores removed.
    pub fn remove_underscores(&self) -> Self {
        Self(self.0.chars().filter(|c| *c != '_').collect())
    }
}

/// Text after apostrophe in qualified literals.
#[derive(Debug, Clone, PartialEq, Eq, AsRef, From)]
#[as_ref(str)]
#[from(&str)]
pub struct QualifierRest(String);

impl QualifierRest {
    /// Returns the qualifier rest text as a string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
