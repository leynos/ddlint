//! Span storage and validation helpers.

use crate::Span;

/// Spans for each parsed statement category.
///
/// Instances are constructed via [`ParsedSpans::builder`] to ensure span lists
/// are sorted and non-overlapping in debug builds.
#[non_exhaustive]
#[derive(Debug, Default, Clone, PartialEq)]
pub struct ParsedSpans {
    /// `import` statement spans.
    imports: Vec<Span>,
    /// `typedef` statement spans.
    typedefs: Vec<Span>,
    /// `relation` declaration spans.
    relations: Vec<Span>,
    /// `index` declaration spans.
    indexes: Vec<Span>,
    /// `function` definition spans.
    functions: Vec<Span>,
    /// `transformer` declaration spans.
    transformers: Vec<Span>,
    /// Rule spans.
    rules: Vec<Span>,
}

/// Builder for [`ParsedSpans`].
#[derive(Default)]
pub struct ParsedSpansBuilder {
    imports: Vec<Span>,
    typedefs: Vec<Span>,
    relations: Vec<Span>,
    indexes: Vec<Span>,
    functions: Vec<Span>,
    transformers: Vec<Span>,
    rules: Vec<Span>,
}

impl ParsedSpansBuilder {
    /// Set the `import` statement spans.
    #[must_use]
    pub fn imports(mut self, spans: Vec<Span>) -> Self {
        self.imports = spans;
        self
    }

    /// Set the `typedef` statement spans.
    #[must_use]
    pub fn typedefs(mut self, spans: Vec<Span>) -> Self {
        self.typedefs = spans;
        self
    }

    /// Set the `relation` declaration spans.
    #[must_use]
    pub fn relations(mut self, spans: Vec<Span>) -> Self {
        self.relations = spans;
        self
    }

    /// Set the `index` declaration spans.
    #[must_use]
    pub fn indexes(mut self, spans: Vec<Span>) -> Self {
        self.indexes = spans;
        self
    }

    /// Set the `function` definition spans.
    #[must_use]
    pub fn functions(mut self, spans: Vec<Span>) -> Self {
        self.functions = spans;
        self
    }

    /// Set the `transformer` declaration spans.
    #[must_use]
    pub fn transformers(mut self, spans: Vec<Span>) -> Self {
        self.transformers = spans;
        self
    }

    /// Set the rule spans.
    #[must_use]
    pub fn rules(mut self, spans: Vec<Span>) -> Self {
        self.rules = spans;
        self
    }

    /// Build the [`ParsedSpans`].
    #[must_use]
    pub fn build(self) -> ParsedSpans {
        ParsedSpans::new(
            self.imports,
            self.typedefs,
            self.relations,
            self.indexes,
            self.functions,
            self.transformers,
            self.rules,
        )
    }
}

impl ParsedSpans {
    /// Start building a [`ParsedSpans`] instance.
    #[must_use]
    pub fn builder() -> ParsedSpansBuilder {
        ParsedSpansBuilder::default()
    }

    pub(super) fn new(
        imports: Vec<Span>,
        typedefs: Vec<Span>,
        relations: Vec<Span>,
        indexes: Vec<Span>,
        functions: Vec<Span>,
        transformers: Vec<Span>,
        rules: Vec<Span>,
    ) -> Self {
        let result = validate_span_lists_sorted(&[
            ("imports", &imports),
            ("typedefs", &typedefs),
            ("relations", &relations),
            ("indexes", &indexes),
            ("functions", &functions),
            ("transformers", &transformers),
            ("rules", &rules),
        ]);
        debug_assert!(result.is_ok(), "{}", result.err().unwrap_or_default());

        Self {
            imports,
            typedefs,
            relations,
            indexes,
            functions,
            transformers,
            rules,
        }
    }

    /// Access `import` statement spans.
    #[must_use]
    pub fn imports(&self) -> &[Span] {
        &self.imports
    }

    /// Access `typedef` statement spans.
    #[must_use]
    pub fn typedefs(&self) -> &[Span] {
        &self.typedefs
    }

    /// Access `relation` declaration spans.
    #[must_use]
    pub fn relations(&self) -> &[Span] {
        &self.relations
    }

    /// Access `index` declaration spans.
    #[must_use]
    pub fn indexes(&self) -> &[Span] {
        &self.indexes
    }

    /// Access `function` definition spans.
    #[must_use]
    pub fn functions(&self) -> &[Span] {
        &self.functions
    }

    /// Access `transformer` declaration spans.
    #[must_use]
    pub fn transformers(&self) -> &[Span] {
        &self.transformers
    }

    /// Access rule spans.
    #[must_use]
    pub fn rules(&self) -> &[Span] {
        &self.rules
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SpanOrderError {
    prev: Span,
    next: Span,
}

impl std::fmt::Display for SpanOrderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "spans overlap or are unsorted: {:?} then {:?}",
            self.prev, self.next
        )
    }
}

impl std::error::Error for SpanOrderError {}

fn validate_spans_sorted(spans: &[Span]) -> Result<(), SpanOrderError> {
    for pair in spans.windows(2) {
        let [first, second] = pair else { continue };
        if first.end > second.start {
            return Err(SpanOrderError {
                prev: first.clone(),
                next: second.clone(),
            });
        }
    }
    Ok(())
}

fn validate_span_lists_sorted(lists: &[(&str, &[Span])]) -> Result<(), String> {
    let mut errors = Vec::new();
    for (name, spans) in lists {
        if let Err(e) = validate_spans_sorted(spans) {
            errors.push(format!("{name} not sorted: {e}"));
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors.join("\n"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_panic_with_message<F>(f: F) -> String
    where
        F: FnOnce() + std::panic::UnwindSafe,
    {
        let result = std::panic::catch_unwind(f);
        let Err(err) = result else {
            panic!("expected panic")
        };
        err.downcast_ref::<String>()
            .cloned()
            .or_else(|| err.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default()
    }

    #[test]
    fn validate_spans_sorted_err_on_overlap() {
        let spans = vec![0..5, 4..8];
        assert!(validate_spans_sorted(&spans).is_err());
    }

    #[test]
    fn validate_spans_sorted_err_on_unsorted() {
        let spans = vec![5..10, 0..2];
        assert!(validate_spans_sorted(&spans).is_err());
    }

    #[test]
    fn validate_spans_sorted_ok_on_empty() {
        let spans: Vec<Span> = Vec::new();
        assert!(validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn validate_spans_sorted_ok_on_single() {
        let spans: Vec<Span> = std::iter::once(0..3).collect();
        assert!(validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn validate_spans_sorted_ok_on_sorted() {
        let spans = vec![0..2, 3..5, 5..8];
        assert!(validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn builder_panics_on_unsorted() {
        let unsorted = vec![1..2, 0..1];
        let text = assert_panic_with_message(|| {
            let _ = ParsedSpans::builder().imports(unsorted).build();
        });
        assert!(text.contains("imports not sorted"));
    }

    #[test]
    fn builder_reports_all_errors() {
        let imports = vec![1..2, 0..1];
        let typedefs = vec![4..5, 3..4];
        let text = assert_panic_with_message(|| {
            let _ = ParsedSpans::builder()
                .imports(imports)
                .typedefs(typedefs)
                .build();
        });
        assert!(text.contains("imports not sorted"));
        assert!(text.contains("typedefs not sorted"));
    }
}
