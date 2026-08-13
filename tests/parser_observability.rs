//! Integration tests for backend-neutral parser observability.

use ddlint::{DiagnosticCategory, DiagnosticCode, DiagnosticSeverity, parse, parse_with_observer};
use rstest::rstest;

#[path = "support/observability.rs"]
mod support;
use support::{RecordedParseEvent, RecordingParseObserver};

#[rstest]
#[case::kind_before_role(
    "relation input R(id: u32)\noutput R(id: u32)",
    DiagnosticCode::RelationKindBeforeRole,
    9..14
)]
#[case::duplicate_role(
    "input output R(id: u32)\noutput R(id: u32)",
    DiagnosticCode::RelationDuplicateRole,
    6..12
)]
#[case::duplicate_kind(
    "stream multiset R(id: u32)\noutput R(id: u32)",
    DiagnosticCode::RelationDuplicateKind,
    7..15
)]
#[case::bracket_primary_key(
    "input R[u32] primary key (id)\noutput R(id: u32)",
    DiagnosticCode::RelationBracketPrimaryKey,
    12..29
)]
#[case::invalid_bracket_element(
    "input R[]\noutput R(id: u32)",
    DiagnosticCode::RelationInvalidBracketElementType,
    9..10
)]
#[case::primary_key_on_non_input(
    "output R(id: u32) primary key (id)\noutput S(id: u32)",
    DiagnosticCode::RelationPrimaryKeyOnNonInput,
    17..34
)]
#[case::malformed_primary_key(
    "input R(id: u32) primary value\noutput R(id: u32)",
    DiagnosticCode::RelationMalformedPrimaryKey,
    25..30
)]
#[case::bracket_wrapped_primary_key(
    "input R(id: u32) [ primary key (id) ]\noutput S(id: u32)",
    DiagnosticCode::RelationBracketWrappedPrimaryKey,
    16..37
)]
fn relation_diagnostics_emit_stable_code_and_span(
    #[case] source: &str,
    #[case] expected_code: DiagnosticCode,
    #[case] expected_span: std::ops::Range<usize>,
) {
    let observer = RecordingParseObserver::default();

    let parsed = parse_with_observer(source, &observer);

    let diagnostics: Vec<_> = observer
        .events()
        .into_iter()
        .filter_map(|event| match event {
            RecordedParseEvent::Diagnostic {
                code: Some(code),
                category,
                span,
                severity,
                message,
            } => Some((code, category, span, severity, message)),
            _ => None,
        })
        .collect();
    let [diagnostic] = diagnostics.as_slice() else {
        panic!("expected exactly one coded diagnostic, got {diagnostics:?}");
    };
    let (code, category, span, severity, message) = diagnostic;
    assert_eq!(*code, expected_code);
    assert_eq!(*category, DiagnosticCategory::Relation);
    assert_eq!(*span, expected_span);
    assert_eq!(*severity, DiagnosticSeverity::Error);
    assert!(message.starts_with(expected_code.as_str()));
    assert!(!parsed.errors().is_empty());
}

#[test]
fn valid_parse_attempts_cover_every_parser_category_in_order() {
    let observer = RecordingParseObserver::default();
    let source = "input relation Source(id: u32)\nOutput(id) :- Source(id).";

    let parsed = parse_with_observer(source, &observer);

    assert!(parsed.errors().is_empty());
    let started: Vec<_> = observer
        .events()
        .into_iter()
        .filter_map(|event| match event {
            RecordedParseEvent::AttemptStarted(category) => Some(category),
            _ => None,
        })
        .collect();
    assert_eq!(
        started,
        vec![
            DiagnosticCategory::Parser,
            DiagnosticCategory::Attribute,
            DiagnosticCategory::Import,
            DiagnosticCategory::Typedef,
            DiagnosticCategory::Relation,
            DiagnosticCategory::Index,
            DiagnosticCategory::Function,
            DiagnosticCategory::Transformer,
            DiagnosticCategory::Apply,
            DiagnosticCategory::Rule,
            DiagnosticCategory::Lexer,
            DiagnosticCategory::SpanBuilder,
            DiagnosticCategory::TopLevelFor,
            DiagnosticCategory::NameUniqueness,
        ]
    );
    let completed: Vec<_> = observer
        .events()
        .into_iter()
        .filter_map(|event| match event {
            RecordedParseEvent::AttemptCompleted {
                category,
                diagnostic_count,
            } => Some((category, diagnostic_count)),
            _ => None,
        })
        .collect();
    assert_eq!(
        completed,
        [
            DiagnosticCategory::Attribute,
            DiagnosticCategory::Import,
            DiagnosticCategory::Typedef,
            DiagnosticCategory::Relation,
            DiagnosticCategory::Index,
            DiagnosticCategory::Function,
            DiagnosticCategory::Transformer,
            DiagnosticCategory::Apply,
            DiagnosticCategory::Rule,
            DiagnosticCategory::Lexer,
            DiagnosticCategory::SpanBuilder,
            DiagnosticCategory::TopLevelFor,
            DiagnosticCategory::NameUniqueness,
            DiagnosticCategory::Parser,
        ]
        .into_iter()
        .map(|category| (category, 0))
        .collect::<Vec<_>>()
    );
}

#[test]
fn observer_events_and_parser_output_are_deterministic() {
    let source = concat!(
        "relation input Broken(id: u32)\n",
        "input relation Source(id: u32)\n",
        "Output(id) :- Source(id).\n",
    );
    let baseline = parse(source);
    let mut expected_events = None;

    for _ in 0..5 {
        let observer = RecordingParseObserver::default();
        let instrumented_parse = parse_with_observer(source, &observer);
        let events = observer.events();

        assert_eq!(instrumented_parse.green(), baseline.green());
        assert_eq!(instrumented_parse.errors(), baseline.errors());
        if let Some(expected) = &expected_events {
            assert_eq!(&events, expected);
        } else {
            expected_events = Some(events);
        }
    }
}
