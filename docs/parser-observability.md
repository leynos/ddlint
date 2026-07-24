# How to export parser observability

This guide shows host-application maintainers how to collect parser attempts
and diagnostics without coupling parser behaviour to a telemetry backend. Use
it when a service, command-line application, or batch worker needs parser logs,
metrics, or alerts.

## Prerequisites

- A host application that calls `ddlint::parse`.
- An existing logging or metrics backend if events must leave the process.
- The `observability` feature only when using the supplied `tracing` and
  `metrics` adapter. Custom observers do not need this feature.

The library never installs a tracing subscriber, metrics recorder, or exporter.
Only the host application initializes global telemetry.

## Choose an observer

For a custom backend, implement `ParseObserver` and copy any borrowed
diagnostic data needed after the callback:

```rust
use ddlint::{
    DiagnosticContext, ParseAttemptContext, ParseObserver, parse_with_observer,
};

struct HostObserver;

impl ParseObserver for HostObserver {
    fn parse_attempt_completed(&self, context: ParseAttemptContext) {
        host_metrics::record_attempt(
            context.category().as_str(),
            context.diagnostic_count(),
        );
    }

    fn diagnostic_emitted(&self, context: &DiagnosticContext<'_>) {
        host_log::record_parser_failure(
            context.code().map(|code| code.as_str()),
            context.category().as_str(),
            context.span(),
            context.severity().as_str(),
            context.message(),
        );
    }
}

let parsed = parse_with_observer(source, &HostObserver);
```

Do not use message text, source spans, source text, paths, or request
identifiers as metrics labels. They are unbounded and belong in structured logs
or traces.

For the supplied facade adapter, enable the feature:

```toml
[dependencies]
ddlint = { version = "0.1", features = ["observability"] }
```

Initialize the application's existing tracing subscriber and metrics recorder,
then pass `TelemetryObserver`:

```rust
use ddlint::{TelemetryObserver, parse_with_observer};

// The application initializes its subscriber, recorder, and exporters first.
let observer = TelemetryObserver::new();
let parsed = parse_with_observer(source, &observer);
```

Without a configured subscriber or recorder, the facades discard events and
parsing remains deterministic. Calling `parse(source)` uses `NoopParseObserver`
and does not require a telemetry runtime.

## Export logs and metrics

`TelemetryObserver` emits the following counters:

| Metric                            | Labels                         | Meaning                            |
| --------------------------------- | ------------------------------ | ---------------------------------- |
| `ddlint_parser_attempts_total`    | `category`                     | Parser or scanner attempts started |
| `ddlint_parser_diagnostics_total` | `code`, `category`, `severity` | Diagnostics emitted                |

The `code` label is the stable diagnostic code or `uncoded`. Categories and
severity are bounded enums. The adapter emits attempt start/completion events at
`DEBUG` and diagnostics at `ERROR` under the `ddlint::parser` tracing target.
Diagnostic events include `code`, `category`, `severity`, `span_start`,
`span_end`, and `message` fields.

Configure the host's normal OpenTelemetry, Prometheus, or vendor exporter to
collect these facade events. Keep exporter lifecycle, batching, retries, and
shutdown in the application boundary.

## Configure alerts

Alert on rates and ratios over a sustained window rather than raw counter
values:

1. Use `ddlint_parser_attempts_total{category="parser"}` as the parse-volume
   denominator.
2. Alert when the diagnostic-to-parser-attempt ratio exceeds the
   application's error budget after a minimum traffic threshold.
3. Group failures by `category` to distinguish scanner regressions from lexer,
   span-builder, top-level-`for`, or name-validation failures.
4. Add focused alerts for unexpected increases in `D-REL-001` through
   `D-REL-008` when relation input quality is operationally significant.
5. Track `code="uncoded"` separately. A sustained increase can identify a
   scanner family that needs its own stable code.

Do not page solely because a diagnostic exists: invalid user input can be an
expected outcome. Choose thresholds from the host application's baseline and
route alerts to the team that owns its input pipeline.

## Stable diagnostic contract

The diagnostic-code list is a compatibility surface under ADR-001 Phase 2.
Existing code meanings do not change; new codes may be added. Human-facing
message wording may become clearer, so automation must match `DiagnosticCode` or
`DiagnosticCategory`, not message text.

| Code        | Relation failure                               |
| ----------- | ---------------------------------------------- |
| `D-REL-001` | Kind keyword appears before the role keyword   |
| `D-REL-002` | More than one role keyword                     |
| `D-REL-003` | More than one kind keyword                     |
| `D-REL-004` | Bracket-form relation declares a primary key   |
| `D-REL-005` | Bracket form lacks one element type            |
| `D-REL-006` | Non-input relation declares a primary key      |
| `D-REL-007` | Malformed primary-key clause                   |
| `D-REL-008` | Unsupported bracket-wrapped primary-key clause |

Uncoded diagnostics retain a stable category. Current categories are `parser`,
`attribute`, `import`, `typedef`, `relation`, `index`, `function`,
`transformer`, `apply`, `rule`, `lexer`, `span_builder`, `top_level_for`, and
`name_uniqueness`.

## See also

- [Parser implementation notes](./parser-implementation-notes.md)
- [Parser conformance register](./parser-conformance-register.md)
- [ADR-001: parser crate split](./adr-001-parser-crate-split.md)
