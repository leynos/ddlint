//! External-consumer fixture for additive diagnostic codes.

use ddlint::DiagnosticCode;

fn code(value: DiagnosticCode) {
    match value {
        DiagnosticCode::RelationKindBeforeRole => {}
        DiagnosticCode::RelationDuplicateRole => {}
        DiagnosticCode::RelationDuplicateKind => {}
        DiagnosticCode::RelationBracketPrimaryKey => {}
        DiagnosticCode::RelationInvalidBracketElementType => {}
        DiagnosticCode::RelationPrimaryKeyOnNonInput => {}
        DiagnosticCode::RelationMalformedPrimaryKey => {}
        DiagnosticCode::RelationBracketWrappedPrimaryKey => {}
    }
}

fn main() {
    code(DiagnosticCode::RelationKindBeforeRole);
}
