//! Integration tests for backend-neutral parser observability.

use ddlint::{DiagnosticCategory, DiagnosticCode, DiagnosticSeverity, parse, parse_with_observer};
use proptest::prelude::*;
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
    assert_ne!(parsed.errors(), []);
    assert_eq!(
        observer
            .events()
            .into_iter()
            .filter_map(|event| match event {
                RecordedParseEvent::AttemptCompleted {
                    category: DiagnosticCategory::Relation,
                    diagnostic_count,
                } => Some(diagnostic_count),
                _ => None,
            })
            .collect::<Vec<_>>(),
        [1]
    );
}

#[test]
fn valid_parse_attempts_cover_every_parser_category_in_order() {
    let observer = RecordingParseObserver::default();
    let source = "input relation Source(id: u32)\nOutput(id) :- Source(id).";

    let parsed = parse_with_observer(source, &observer);

    assert_eq!(parsed.errors(), []);
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
fn parser_completion_summarizes_child_diagnostics_without_reemitting_them() {
    let observer = RecordingParseObserver::default();
    let source = "relation input R(id: u32)\noutput R(id: u32)";

    let parsed = parse_with_observer(source, &observer);
    let events = observer.events();

    assert_eq!(parsed.errors().len(), 1);
    assert_eq!(
        events
            .iter()
            .filter(|event| matches!(event, RecordedParseEvent::Diagnostic { .. }))
            .count(),
        1
    );

    let relation_counts: Vec<_> = events
        .iter()
        .filter_map(|event| match event {
            RecordedParseEvent::AttemptCompleted {
                category: DiagnosticCategory::Relation,
                diagnostic_count,
            } => Some(*diagnostic_count),
            _ => None,
        })
        .collect();
    assert_eq!(relation_counts, [1]);

    let parser_counts: Vec<_> = events
        .iter()
        .filter_map(|event| match event {
            RecordedParseEvent::AttemptCompleted {
                category: DiagnosticCategory::Parser,
                diagnostic_count,
            } => Some(*diagnostic_count),
            _ => None,
        })
        .collect();
    assert_eq!(parser_counts, [1]);
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

proptest::proptest! {
    #![proptest_config(ProptestConfig::with_cases(64))]

    #[test]
    fn observer_does_not_change_parse_results_or_event_order(
        // This property checks observer parity, not delimiter recovery; the
        // expression span walker asserts on unmatched closing delimiters.
        source in prop::collection::vec(
            any::<char>().prop_filter("no unmatched closing delimiters", |character| {
                !matches!(character, ')' | '}' | ']')
            }),
            0..128,
        )
            .prop_map(|characters| characters.into_iter().collect::<String>())
    ) {
        let baseline = parse(&source);
        let first_observer = RecordingParseObserver::default();
        let first_parse = parse_with_observer(&source, &first_observer);
        let first_events = first_observer.events();
        let second_observer = RecordingParseObserver::default();
        let second_parse = parse_with_observer(&source, &second_observer);

        proptest::prop_assert_eq!(first_parse.green(), baseline.green());
        proptest::prop_assert_eq!(first_parse.errors(), baseline.errors());
        proptest::prop_assert_eq!(second_parse.green(), baseline.green());
        proptest::prop_assert_eq!(second_parse.errors(), baseline.errors());
        proptest::prop_assert_eq!(second_observer.events(), first_events);
    }
}

#[rstest]
#[case::typedef("typedef Old = u32;", "typedef")]
#[case::bigint("type Number = bigint;", "bigint")]
#[case::bit("type Word = bit<32>;", "bit")]
#[case::double("type Number = double;", "double")]
#[case::float("type Number = float;", "float")]
#[case::signed("type Number = signed<32>;", "signed")]
#[case::hash("#", "#")]
#[case::spaceship("type Number = A <=> B;", "<=>")]
#[case::child_expression("for (x in Items(x)) Items(x <=> x).", "<=>")]
fn completed_reserved_diagnostics_are_observed_once(#[case] source: &str, #[case] token: &str) {
    let observer = RecordingParseObserver::default();
    let parsed = parse_with_observer(source, &observer);
    let start = source
        .find(token)
        .expect("reserved token should be present");
    let expected_span = start..start + token.len();
    let expected = parsed
        .errors()
        .iter()
        .find(|error| {
            error.span() == expected_span
                && matches!(error.reason(), chumsky::error::SimpleReason::Custom(_))
        })
        .expect("reserved diagnostic should be retained");
    let chumsky::error::SimpleReason::Custom(expected_message) = expected.reason() else {
        panic!("reserved diagnostic should have a custom message");
    };
    assert!(expected_message.starts_with(&format!("`{token}`")));
    let events = observer.events();
    let matching: Vec<_> = events
        .iter()
        .filter(|event| {
            matches!(event, RecordedParseEvent::Diagnostic { code: None, span, message, .. }
                if *span == expected_span && message == expected_message)
        })
        .collect();
    assert_eq!(
        matching.len(),
        1,
        "one callback per retained reserved diagnostic"
    );
    assert_eq!(
        events.last(),
        Some(&RecordedParseEvent::AttemptCompleted {
            category: DiagnosticCategory::Parser,
            diagnostic_count: parsed.errors().len(),
        })
    );
}
