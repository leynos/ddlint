//! External-consumer fixture for additive diagnostic severities.

use ddlint::DiagnosticSeverity;

fn severity(value: DiagnosticSeverity) {
    match value {
        DiagnosticSeverity::Error => {}
    }
}

fn main() {
    severity(DiagnosticSeverity::Error);
}
