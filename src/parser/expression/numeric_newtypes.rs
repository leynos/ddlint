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
    #[inline]
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Remove underscores from the digit string.
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
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
