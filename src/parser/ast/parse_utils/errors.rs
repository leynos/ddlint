//! Error types and helpers for delimiter tracking during type parsing.
//!
//! The functions in `type_parsing` rely on these structures to report
//! mismatched or unclosed delimiters when reading parameter lists and type
//! expressions.

use rowan::TextRange;

use crate::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Delim {
    Paren,
    Angle,
    Bracket,
    Brace,
}

#[derive(Default)]
pub(crate) struct DelimStack(pub(crate) Vec<Delim>);

/// An error emitted when a closing token does not match the expected delimiter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DelimiterError {
    pub(super) expected: Delim,
    pub(super) found: SyntaxKind,
    pub(super) span: TextRange,
}

/// Error types produced when parsing name-type pairs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ParseError {
    /// A closing delimiter did not match the expected opener.
    Delimiter(DelimiterError),
    /// An opening delimiter was not closed before the expression ended.
    UnclosedDelimiter { delimiter: char, span: TextRange },
    /// A parameter name was not followed by a colon.
    MissingColon { message: String, span: TextRange },
    /// A parameter name was not provided.
    MissingName { span: TextRange },
    /// A parameter type was not provided.
    MissingType { span: TextRange },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Delimiter(err) => write!(f, "{err}"),
            Self::UnclosedDelimiter { delimiter, span } => {
                write!(f, "unclosed delimiter '{delimiter}' at {span:#?}")
            }
            Self::MissingColon { message, span } => write!(f, "{message} at {span:#?}"),
            Self::MissingName { span } => write!(f, "parameter name missing at {span:#?}"),
            Self::MissingType { span } => write!(f, "parameter type missing at {span:#?}"),
        }
    }
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for DelimiterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expected = match self.expected {
            Delim::Paren => ")",
            Delim::Angle => ">",
            Delim::Bracket => "]",
            Delim::Brace => "}",
        };
        let found = match self.found {
            SyntaxKind::T_RPAREN => ")",
            SyntaxKind::T_GT => ">",
            SyntaxKind::T_SHR => ">>",
            SyntaxKind::T_RBRACKET => "]",
            SyntaxKind::T_RBRACE => "}",
            _ => "?",
        };
        write!(
            f,
            "expected '{}' before '{}' at {:#?}",
            expected, found, self.span
        )
    }
}

impl std::error::Error for DelimiterError {}

impl DelimStack {
    pub(super) fn open(&mut self, delim: Delim, count: usize) {
        for _ in 0..count {
            self.0.push(delim);
        }
    }

    pub(super) fn close(&mut self, delim: Delim, count: usize) -> usize {
        let mut closed = 0;
        for _ in 0..count {
            match self.0.pop() {
                Some(d) if d == delim => closed += 1,
                Some(d) => {
                    self.0.push(d);
                    break;
                }
                None => break,
            }
        }
        closed
    }

    /// Checks whether the delimiter stack contains no open delimiters.
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
