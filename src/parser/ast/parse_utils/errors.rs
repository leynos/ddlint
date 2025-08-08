//! Error types and helpers for delimiter tracking during type parsing.
//!
//! The functions in `type_parsing` rely on these structures to report
//! mismatched or unclosed delimiters when reading parameter lists and type
//! expressions.

use rowan::TextRange;

use crate::SyntaxKind;

/// Represents the four types of delimiters tracked during parsing.
///
/// `DelimStack` records these variants as delimiters are opened so
/// mismatches and unclosed delimiters can be reported precisely.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Delim {
    Paren,
    Angle,
    Bracket,
    Brace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OpenDelimiter {
    kind: Delim,
    span: TextRange,
}

#[derive(Default, Debug)]
pub(crate) struct DelimStack(Vec<OpenDelimiter>);

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
            // Any unexpected token kind is represented as "?"
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
    /// Opens one or more delimiters of the same type.
    ///
    /// Pushes `count` instances of `delim` onto the stack, each recording the
    /// `span` of the opening token so it can be reported if left unclosed.
    pub(super) fn open(&mut self, delim: Delim, span: TextRange, count: usize) {
        for _ in 0..count {
            self.0.push(OpenDelimiter { kind: delim, span });
        }
    }

    /// Attempts to close delimiters of the specified type.
    ///
    /// Pops up to `count` matching delimiters from the stack and returns the
    /// number of delimiters actually closed. The operation stops if the stack is
    /// empty or the top delimiter does not match `delim`.
    pub(super) fn close(&mut self, delim: Delim, count: usize) -> usize {
        let mut closed = 0;
        for _ in 0..count {
            if matches!(self.0.last(), Some(d) if d.kind == delim) {
                self.0.pop();
                closed += 1;
            } else {
                break;
            }
        }
        closed
    }

    /// Returns an iterator over any remaining unclosed delimiters.
    pub(crate) fn unclosed(&mut self) -> impl Iterator<Item = (Delim, TextRange)> + '_ {
        self.0.drain(..).map(|d| (d.kind, d.span))
    }

    /// Checks whether the delimiter stack contains no open delimiters.
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
