//! String literal types used by expression AST nodes.
//!
//! Captures interpolation and interning flags so downstream passes can reason
//! about string semantics without re-parsing the source text.

fn is_valid_string_prefix(s: &str) -> bool {
    s.starts_with('"') || s.starts_with("[|") || s.starts_with("$[|")
}

fn contains_unescaped_interpolation(body: &str) -> bool {
    let mut search_from = 0;

    while let Some(pos) = body.get(search_from..).and_then(|s| s.find("${")) {
        let absolute_pos = search_from + pos;

        let mut backslash_count = 0;
        let mut check_pos = absolute_pos;
        while check_pos > 0 {
            check_pos -= 1;
            if matches!(body.as_bytes().get(check_pos), Some(b'\\')) {
                backslash_count += 1;
            } else {
                break;
            }
        }

        if backslash_count & 1 == 0 {
            return true;
        }

        search_from = absolute_pos + 1;
    }

    false
}

enum StringPrefix {
    Standard,
    Raw { interpolated: bool },
}

fn parse_prefix(text: &str) -> (bool, StringPrefix, &str) {
    let (interned, rest) = text.strip_prefix('i').map_or((false, text), |rest| {
        if is_valid_string_prefix(rest) {
            (true, rest)
        } else {
            (false, text)
        }
    });

    if let Some(content) = rest.strip_prefix("$[|") {
        return (interned, StringPrefix::Raw { interpolated: true }, content);
    }
    if let Some(content) = rest.strip_prefix("[|") {
        return (
            interned,
            StringPrefix::Raw {
                interpolated: false,
            },
            content,
        );
    }

    (interned, StringPrefix::Standard, rest)
}

/// Distinguishes the surface syntax of string literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringKind {
    /// Standard quoted string supporting escape sequences and interpolation.
    Standard { interpolated: bool },
    /// Raw string delimited by `[|` and `|]`, with optional interpolation.
    Raw { interpolated: bool },
}

/// Representation of string literals including interning and interpolation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    /// Contents of the literal without delimiters.
    pub body: String,
    /// Source syntax used to write the string.
    pub kind: StringKind,
    /// Whether the literal is prefixed with `i` and should be interned.
    pub interned: bool,
}

impl StringLiteral {
    /// Parse a string literal token into a structured representation.
    ///
    /// # Errors
    /// Returns an error string when the token text is not a supported string
    /// literal form.
    pub fn parse(text: &str) -> Result<Self, &'static str> {
        let (interned, prefix, rest) = parse_prefix(text);

        match prefix {
            StringPrefix::Raw { interpolated } => {
                let body = rest
                    .strip_suffix("|]")
                    .ok_or("unterminated raw string literal")?;
                Ok(Self {
                    body: body.to_string(),
                    kind: StringKind::Raw { interpolated },
                    interned,
                })
            }
            StringPrefix::Standard => {
                let content = rest.strip_prefix('"').ok_or("expected '\"'")?;
                let body = content
                    .strip_suffix('"')
                    .ok_or("unterminated string literal")?;
                let interpolated = contains_unescaped_interpolation(body);
                Ok(Self {
                    body: body.to_string(),
                    kind: StringKind::Standard { interpolated },
                    interned,
                })
            }
        }
    }

    /// True when the literal includes interpolation markers.
    #[must_use]
    pub fn is_interpolated(&self) -> bool {
        matches!(
            self.kind,
            StringKind::Standard { interpolated: true } | StringKind::Raw { interpolated: true }
        )
    }

    /// Render the literal using `DDlog` surface syntax.
    #[must_use]
    pub fn to_source(&self) -> String {
        match self.kind {
            StringKind::Standard { .. } => {
                let rendered = format!("{:?}", self.body);
                if self.interned {
                    format!("i{rendered}")
                } else {
                    rendered
                }
            }
            StringKind::Raw { interpolated } => {
                let mut prefix = String::new();
                if self.interned {
                    prefix.push('i');
                }
                if interpolated {
                    prefix.push('$');
                }
                format!("{prefix}[|{}|]", self.body)
            }
        }
    }

    /// Render the literal for `Expr::to_sexpr` output.
    #[must_use]
    pub fn to_sexpr(&self) -> String {
        self.to_source()
    }
}
