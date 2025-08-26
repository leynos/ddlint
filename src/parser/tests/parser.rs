//! Integration tests for the parser skeleton.
//!
//! These tests verify that the parser builds a correct CST and that source
//! round-trips through `pretty_print` unchanged.

use crate::{SyntaxKind, ast::AstNode};
use rstest::rstest;
use super::helpers::{parse_err, parse_ok, round_trip};

mod programs;
use programs::{
    BasicProgram,
    FunctionProgram,
    IndexProgram,
    RelationProgram,
    RuleProgram,
    TransformerProgram,
};
mod specs;
use specs::{FnSpec, IndexSpec, RelationSpec, TransformerSpec};

const USER_ID: &str = "user_id";
const USERNAME: &str = "username";

#[rstest]
#[case(BasicProgram::Simple)]
#[case(BasicProgram::Complex)]
#[case(BasicProgram::MultilineRelation)]
fn round_trip_program(#[case] prog: BasicProgram) {
    round_trip(prog.source());
}

#[rstest]
#[case(BasicProgram::Complex, 2)]
#[case(BasicProgram::Simple, 1)]
#[case(BasicProgram::Empty, 0)]
fn relation_counts(#[case] prog: BasicProgram, #[case] expected: usize) {
    let parsed = parse_ok(prog.source());
    assert_eq!(parsed.root().relations().len(), expected);
}

#[rstest]
#[case(RelationProgram::InputRelationPk, RelationSpec::new("User").input().column(USER_ID, "u32").column(USERNAME, "string").pk(vec![USER_ID]))]
#[case(RelationProgram::OutputRelationNoPk, RelationSpec::new("Alert").output().column("message", "string").column("timestamp", "u64"))]
#[case(RelationProgram::InternalRelationCompoundPk, RelationSpec::new("UserSession").column(USER_ID, "u32").column("session_id", "string").column("start_time", "u64").pk(vec![USER_ID, "session_id"]))]
fn relation_parsing(#[case] prog: RelationProgram, #[case] spec: RelationSpec) {
    let parsed = parse_ok(prog.source());
    assert_eq!(parsed.root().relations().len(), 1, "expected exactly one relation");
    let rel = parsed.root().relations().first().expect("relation missing");
    spec.assert(rel);
}

#[rstest]
#[case(RelationProgram::RelationUnbalancedParentheses)]
#[case(RelationProgram::RelationEmptyColumns)]
#[case(RelationProgram::RelationWhitespaceColumns)]
#[case(RelationProgram::RelationInvalidPk)]
#[case(RelationProgram::RelationPkEmpty)]
#[case(RelationProgram::RelationPkTrailingComma)]
fn relation_invalid(#[case] prog: RelationProgram) {
    let parsed = parse_err(prog.source());
    assert!(parsed.root().relations().is_empty());
}

#[rstest]
#[case(IndexProgram::IndexSingleColumn, IndexSpec::new("Idx_User_username", "User").column(USERNAME))]
#[case(IndexProgram::IndexMultiColumn, IndexSpec::new("Idx_Session_user_time", "UserSession").column(USER_ID).column("start_time"))]
#[case(IndexProgram::IndexNestedFunction, IndexSpec::new("Idx_lower_username", "User").column("lower(username)"))]
#[case(IndexProgram::IndexWhitespaceVariations, IndexSpec::new("Idx_User_ws", "User").column(USERNAME))]
fn index_parsing(#[case] prog: IndexProgram, #[case] spec: IndexSpec) {
    let parsed = parse_ok(prog.source());
    assert_eq!(parsed.root().indexes().len(), 1, "expected exactly one index");
    let idx = parsed.root().indexes().first().expect("index missing");
    spec.assert(idx);
}

#[rstest]
#[case(IndexProgram::IndexInvalidMissingOn)]
#[case(IndexProgram::IndexUnbalancedParentheses)]
fn index_errors(#[case] prog: IndexProgram) {
    let parsed = parse_err(prog.source());
    assert!(parsed.root().indexes().is_empty());
}

#[rstest]
#[case(RuleProgram::SimpleRule)]
#[case(RuleProgram::MultiLiteralRule)]
#[case(RuleProgram::FactRule)]
fn rule_parsing(#[case] prog: RuleProgram) {
    round_trip(prog.source());
}

#[rstest]
#[case(concat!(":- User(", USER_ID, ", ", USERNAME, ", _)."))]
#[case("UserLogin(username, session_id) :- .")]
#[case("UserLogin(username, session_id) User(user_id, username, _).")]
#[case(concat!("UserLogin(", USERNAME, ", session_id) :- User(", USER_ID, ", ", USERNAME, ", _)"))]
fn rule_errors(#[case] src: &str) {
    parse_err(src);
}

#[rstest]
#[case("import standard_library", vec![("standard_library", None)])]
#[case("import collections::vector as vec", vec![("collections::vector", Some("vec"))])]
#[case("import a\nimport b as c", vec![("a", None), ("b", Some("c"))])]
fn import_parsing(#[case] src: &str, #[case] expected: Vec<(&str, Option<&str>)>) {
    let parsed = parse_ok(src);
    let actual: Vec<_> = parsed
        .root()
        .imports()
        .iter()
        .map(|i| (i.path(), i.alias()))
        .collect();
    let expected: Vec<_> = expected
        .into_iter()
        .map(|(p, a)| (p.to_string(), a.map(str::to_string)))
        .collect();
    assert_eq!(actual, expected);
}

#[rstest]
fn import_missing_path() {
    use chumsky::error::SimpleReason;
    let src = "import as missing_path";
    let parsed = parse_err(src);
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);
    let error = errors.first().expect("expected error");
    assert!(matches!(error.reason(), SimpleReason::Unexpected));
    assert!(
        error
            .expected()
            .any(|e| e.as_ref().is_some_and(|k| *k == SyntaxKind::T_IDENT))
    );
    assert_eq!(error.found(), Some(&SyntaxKind::K_AS));
    assert!(parsed.root().imports().is_empty());
}

#[rstest]
#[case("typedef Uuid = string\n", ("Uuid", false))]
#[case("typedef UserRecord = (name: string, age: u64, active: bool)\n", ("UserRecord", false))]
#[case("extern type FfiHandle\n", ("FfiHandle", true))]
fn typedef_parsing(#[case] src: &str, #[case] expect: (&str, bool)) {
    let parsed = parse_ok(src);
    let def = parsed
        .root()
        .type_defs()
        .first()
        .expect("typedef not found");
    assert_eq!(def.name(), Some(expect.0.into()));
    assert_eq!(def.is_extern(), expect.1);
}

#[rstest]
#[case("typedef = string\n")]
#[case("typedef MissingType\n")]
fn typedef_errors(#[case] src: &str) {
    let parsed = parse_err(src);
    assert!(parsed.root().type_defs().is_empty());
}

#[rstest]
#[case(FunctionProgram::ExternFunction, FnSpec::new("hash").extern_().param("data", "string").ret("u64"))]
#[case(FunctionProgram::FunctionWithBody, FnSpec::new("to_uppercase").param("s", "string").ret("string"))]
#[case(FunctionProgram::FunctionNoReturn, FnSpec::new("log_message").param("msg", "string"))]
#[case(FunctionProgram::FunctionNoParams, FnSpec::new("greet").ret("string"))]
#[case(FunctionProgram::FunctionMultiParams, FnSpec::new("concat").param("a", "string").param("b", "string").ret("string"))]
#[case(FunctionProgram::FunctionComplexParams, FnSpec::new("complex").param("p", "(u32,(u8,string))").ret("bool"))]
#[case(FunctionProgram::FunctionWsComments, FnSpec::new("spaced").param("x", "string").ret("u8"))]
#[case(FunctionProgram::FunctionGenericParams, FnSpec::new("example").param("arg", "Vec<(u32,string)>").param("map", "Map<string,u64>").ret("bool"))]
#[case(FunctionProgram::FunctionNestedGenerics, FnSpec::new("test").param("p", "Vec<Map<string,Vec<u8>>>").param("arr", "[Vec<u32>]").ret("bool"))]
#[case(FunctionProgram::FunctionShiftParam, FnSpec::new("shift").param("x", "Vec<<u8>>").ret("bool"))]
fn function_parsing(#[case] prog: FunctionProgram, #[case] spec: FnSpec) {
    let parsed = parse_ok(prog.source());
    assert_eq!(
        parsed.root().functions().len(),
        1,
        "expected exactly one function",
    );
    let func = parsed.root().functions().first().expect("function missing");
    spec.assert(func);
}

#[rstest]
#[case(FunctionProgram::ExternFunctionMissingColon)]
#[case(FunctionProgram::FunctionUnterminatedBody)]
#[case(FunctionProgram::FunctionUnclosedParams)]
fn function_errors(#[case] prog: FunctionProgram) {
    let parsed = parse_err(prog.source());
    assert!(parsed.root().functions().is_empty());
}

#[rstest]
#[case(TransformerProgram::TransformerSingleIo, TransformerSpec::new("normalize").input("input", "UnnormalizedData").output("NormalizedData"))]
#[case(TransformerProgram::TransformerMultiIo, TransformerSpec::new("correlate").input("users", "User").input("sessions", "UserSession").output("UserActivity").output("SessionAlerts"))]
#[case(TransformerProgram::TransformerNoInputs, TransformerSpec::new("no_inputs").output("OutputType"))]
#[case(TransformerProgram::TransformerExtraWs, TransformerSpec::new("spaced").input("foo", "Bar").input("baz", "Qux").output("Out1").output("Out2"))]
#[case(TransformerProgram::TransformerDupInputs, TransformerSpec::new("dup_inputs").input("foo", "Bar").input("foo", "Baz").output("Out"))]
#[case(TransformerProgram::TransformerReservedNames, TransformerSpec::new("reserved").input("transformer", "Type").input("extern", "Type").output("out"))]
fn transformer_parsing(#[case] prog: TransformerProgram, #[case] spec: TransformerSpec) {
    let parsed = parse_ok(prog.source());
    let t = parsed
        .root()
        .transformers()
        .first()
        .expect("transformer missing");
    spec.assert(t);
}

#[rstest]
#[case(TransformerProgram::TransformerInvalid)]
#[case(TransformerProgram::TransformerNoOutputs)]
fn transformer_errors(#[case] prog: TransformerProgram) {
    let parsed = parse_err(prog.source());
    assert!(parsed.root().transformers().is_empty());
}
