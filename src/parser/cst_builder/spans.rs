//! Span storage and validation helpers used when building the CST.
//!
//! `ParsedSpans` groups the byte ranges for each statement category after
//! scanning the token stream. During [`build_green_tree`](super::tree::build_green_tree)
//! these spans determine where nodes start and end so the resulting tree
//! mirrors the source layout. The builder enforces that every span list is
//! sorted and free from overlaps, catching mistakes early.

use crate::Span;

/// A single span ordering issue within a named span list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpanListIssue {
    list: String,
    prev: Span,
    next: Span,
}

impl SpanListIssue {
    fn new(list: impl Into<String>, prev: Span, next: Span) -> Self {
        Self {
            list: list.into(),
            prev,
            next,
        }
    }

    /// The span list name that failed validation.
    #[must_use]
    pub fn list(&self) -> &str {
        &self.list
    }

    /// The earlier span in the invalid ordering.
    #[must_use]
    pub fn prev(&self) -> &Span {
        &self.prev
    }

    /// The later span in the invalid ordering.
    #[must_use]
    pub fn next(&self) -> &Span {
        &self.next
    }
}

impl std::fmt::Display for SpanListIssue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} not sorted: spans overlap or are unsorted: {:?} then {:?}",
            self.list, self.prev, self.next
        )
    }
}

/// Errors returned when span list validation fails.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("{message}")]
pub struct SpanListValidationError {
    message: String,
    issues: Vec<SpanListIssue>,
}

impl SpanListValidationError {
    fn new(issues: Vec<SpanListIssue>) -> Self {
        let message = issues
            .iter()
            .map(SpanListIssue::to_string)
            .collect::<Vec<_>>()
            .join("\n");
        Self { message, issues }
    }

    /// Access the underlying validation issues.
    #[must_use]
    pub fn issues(&self) -> &[SpanListIssue] {
        &self.issues
    }
}

/// Spans for each parsed statement category.
///
/// Instances are constructed via [`ParsedSpans::builder`] to ensure span lists
/// are sorted and non-overlapping.
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
    /// Expression spans.
    expressions: Vec<Span>,
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
    expressions: Vec<Span>,
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

    /// Set expression spans.
    #[must_use]
    pub fn expressions(mut self, spans: Vec<Span>) -> Self {
        self.expressions = spans;
        self
    }

    /// Build the [`ParsedSpans`], returning an error for invalid span lists.
    pub fn try_build(self) -> Result<ParsedSpans, SpanListValidationError> {
        enforce_valid_span_lists(&self.span_lists())?;
        Ok(self.into_parsed_spans())
    }

    /// Build the [`ParsedSpans`].
    #[must_use]
    pub fn build(self) -> ParsedSpans {
        enforce_valid_span_lists(&self.span_lists()).unwrap_or_else(|error| {
            panic!("{error}");
        });
        self.into_parsed_spans()
    }

    fn span_lists(&self) -> [(&'static str, &[Span]); 8] {
        [
            ("imports", &self.imports),
            ("typedefs", &self.typedefs),
            ("relations", &self.relations),
            ("indexes", &self.indexes),
            ("functions", &self.functions),
            ("transformers", &self.transformers),
            ("rules", &self.rules),
            ("expressions", &self.expressions),
        ]
    }

    fn into_parsed_spans(self) -> ParsedSpans {
        let Self {
            imports,
            typedefs,
            relations,
            indexes,
            functions,
            transformers,
            rules,
            expressions,
        } = self;

        ParsedSpans {
            imports,
            typedefs,
            relations,
            indexes,
            functions,
            transformers,
            rules,
            expressions,
        }
    }
}

impl ParsedSpans {
    /// Start building a [`ParsedSpans`] instance.
    #[must_use]
    pub fn builder() -> ParsedSpansBuilder {
        ParsedSpansBuilder::default()
    }

    // constructor removed; instances are built via the builder

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

    /// Access expression spans.
    #[must_use]
    pub fn expressions(&self) -> &[Span] {
        &self.expressions
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

fn enforce_valid_span_lists(lists: &[(&str, &[Span])]) -> Result<(), SpanListValidationError> {
    validate_span_lists_sorted(lists)
}

fn validate_span_lists_sorted(lists: &[(&str, &[Span])]) -> Result<(), SpanListValidationError> {
    let mut issues = Vec::new();
    for &(name, spans) in lists {
        if let Err(e) = validate_spans_sorted(spans) {
            let SpanOrderError { prev, next } = e;
            issues.push(SpanListIssue::new(name, prev, next));
        }
    }
    if issues.is_empty() {
        Ok(())
    } else {
        Err(SpanListValidationError::new(issues))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::assert_panic_with_message;

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
    fn validate_span_lists_sorted_reports_list_name() {
        let imports = vec![2..4, 1..2];
        let Err(err) = validate_span_lists_sorted(&[("imports", &imports)]) else {
            panic!("expected validation error");
        };
        let Some(issue) = err.issues().first() else {
            panic!("expected at least one issue");
        };
        assert_eq!(issue.list(), "imports");
    }

    #[test]
    fn builder_succeeds_on_sorted_spans() {
        let spans = vec![0..2, 2..4];
        let parsed = ParsedSpans::builder().imports(spans.clone()).build();
        assert_eq!(parsed.imports(), spans.as_slice());
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
    fn builder_panics_on_overlap() {
        let overlapping = vec![0..3, 2..4];
        let text = assert_panic_with_message(|| {
            let _ = ParsedSpans::builder().imports(overlapping).build();
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

    #[test]
    fn try_build_errs_on_unsorted_spans() {
        let unsorted = vec![1..2, 0..1];
        let result = ParsedSpans::builder().imports(unsorted).try_build();
        let Err(err) = result else {
            panic!("expected validation error");
        };
        assert_eq!(err.issues().len(), 1);
        let Some(issue) = err.issues().first() else {
            panic!("expected at least one issue");
        };
        assert_eq!(issue.list(), "imports");
    }

    #[test]
    fn try_build_ok_on_sorted_spans() {
        let spans = vec![0..1, 2..3];
        let parsed = match ParsedSpans::builder().imports(spans.clone()).try_build() {
            Ok(parsed) => parsed,
            Err(err) => panic!("expected valid spans, got: {err}"),
        };
        assert_eq!(parsed.imports(), spans.as_slice());
    }
}
