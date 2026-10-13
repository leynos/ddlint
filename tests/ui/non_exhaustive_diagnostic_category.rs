//! External-consumer fixture for additive diagnostic categories.

use ddlint::DiagnosticCategory;

fn category(value: DiagnosticCategory) {
    match value {
        DiagnosticCategory::Parser => {}
        DiagnosticCategory::Attribute => {}
        DiagnosticCategory::Import => {}
        DiagnosticCategory::Typedef => {}
        DiagnosticCategory::Relation => {}
        DiagnosticCategory::Index => {}
        DiagnosticCategory::Function => {}
        DiagnosticCategory::Transformer => {}
        DiagnosticCategory::Apply => {}
        DiagnosticCategory::Rule => {}
        DiagnosticCategory::Lexer => {}
        DiagnosticCategory::SpanBuilder => {}
        DiagnosticCategory::TopLevelFor => {}
        DiagnosticCategory::NameUniqueness => {}
    }
}

fn main() {
    category(DiagnosticCategory::Parser);
}
