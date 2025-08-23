//! Integration tests for the parser skeleton.
//!
//! These tests verify that the parser builds a correct CST and that source
//! round-trips through `pretty_print` unchanged.

use crate::{
    SyntaxKind,
    ast::{AstNode, Function, Import, Index, Relation, Transformer, TypeDef},
    parse,
};
use rstest::rstest;

const USER_ID: &str = "user_id";
const USERNAME: &str = "username";

enum TestProgram {
    SimpleProg,
    ComplexProg,
    EmptyProg,
    InputRelationPk,
    OutputRelationNoPk,
    InternalRelationCompoundPk,
    MultilineRelation,
    IndexSingleColumn,
    IndexMultiColumn,
    IndexInvalidMissingOn,
    IndexNestedFunction,
    IndexUnbalancedParentheses,
    RelationUnbalancedParentheses,
    RelationEmptyColumns,
    RelationWhitespaceColumns,
    RelationInvalidPk,
    RelationPkEmpty,
    RelationPkTrailingComma,
    IndexWhitespaceVariations,
    SimpleRule,
    MultiLiteralRule,
    FactRule,
    ExternFunction,
    FunctionWithBody,
    FunctionNoReturn,
    ExternFunctionMissingColon,
    FunctionUnterminatedBody,
    FunctionNoParams,
    FunctionMultiParams,
    FunctionComplexParams,
    FunctionWsComments,
    FunctionUnclosedParams,
    FunctionGenericParams,
    FunctionNestedGenerics,
    FunctionShiftParam,
    TransformerSingleIo,
    TransformerMultiIo,
    TransformerInvalid,
    TransformerNoInputs,
    TransformerNoOutputs,
    TransformerExtraWs,
    TransformerDupInputs,
    TransformerReservedNames,
}

impl TestProgram {
    fn source(&self) -> &'static str {
        use TestProgram::*;
        match self {
            SimpleProg => "input relation R(x: u32);",
            ComplexProg => "input relation R(x: u32);\noutput relation S(y: string);",
            EmptyProg => "",
            InputRelationPk => {
                "input relation User(user_id: u32, username: string) primary key (user_id)"
            }
            OutputRelationNoPk => "output relation Alert(message: string, timestamp: u64)",
            InternalRelationCompoundPk => {
                "relation UserSession(user_id: u32, session_id: string, start_time: u64) primary key (user_id, session_id)"
            }
            MultilineRelation => {
                "input relation Log(\n    id: u32,\n    message: string\n) primary key (id)\n"
            }
            IndexSingleColumn => "index Idx_User_username on User(username)",
            IndexMultiColumn => "index Idx_Session_user_time on UserSession(user_id, start_time)",
            IndexInvalidMissingOn => "index Idx_Invalid User(username)",
            IndexNestedFunction => "index Idx_lower_username on User(lower(username))",
            IndexUnbalancedParentheses => "index Idx_Unbalanced on User(lower(username)",
            RelationUnbalancedParentheses => "relation Foo(x: u32",
            RelationEmptyColumns => "relation Foo()",
            RelationWhitespaceColumns => "relation Foo(   )",
            RelationInvalidPk => "relation Foo(x: u32) primary key x",
            RelationPkEmpty => "relation Foo(x: u32) primary key ()",
            RelationPkTrailingComma => "relation Foo(x: u32) primary key (x,)",
            IndexWhitespaceVariations => "  index  Idx_User_ws \t on\n  User (\n    username  )  ",
            SimpleRule => "ActiveUser(user_id) :- User(user_id, _, true).",
            MultiLiteralRule => {
                "UserLogin(username, session_id) :- User(user_id, username, _), UserSession(user_id, session_id, _)."
            }
            FactRule => "SystemAlert(\"System is now online.\").",
            ExternFunction => "extern function hash(data: string): u64\n",
            FunctionWithBody => "function to_uppercase(s: string): string {\n}\n",
            FunctionNoReturn => "function log_message(msg: string) {\n}\n",
            ExternFunctionMissingColon => "extern function missing_colon u32\n",
            FunctionUnterminatedBody => "function foo() {",
            FunctionNoParams => "function greet(): string {\n}\n",
            FunctionMultiParams => "function concat(a: string, b: string): string {\n}\n",
            FunctionComplexParams => "function complex(p: (u32,(u8,string))): bool {\n}\n",
            FunctionWsComments => "function  spaced  (  x : string )  :  u8 { /*empty*/ }\n",
            FunctionUnclosedParams => "function bad(a: string { }\n",
            FunctionGenericParams => {
                "function example(arg: Vec<(u32,string)>, map: Map<string,u64>): bool {\n}\n"
            }
            FunctionNestedGenerics => {
                "function test(p: Vec<Map<string,Vec<u8>>>, arr: [Vec<u32>]): bool {}\n"
            }
            FunctionShiftParam => "function shift(x: Vec<<u8>>): bool {}\n",
            TransformerSingleIo => {
                "extern transformer normalize(input: UnnormalizedData): NormalizedData"
            }
            TransformerMultiIo => {
                "extern transformer correlate(users: User, sessions: UserSession): UserActivity, SessionAlerts"
            }
            TransformerInvalid => "extern transformer incomplete_transformer(input: SomeData):",
            TransformerNoInputs => "extern transformer no_inputs(): OutputType",
            TransformerNoOutputs => "extern transformer no_outputs(input: InputType):",
            TransformerExtraWs => {
                " extern   transformer   spaced  (  foo  :  Bar  ,  baz : Qux )  :  Out1 , Out2 "
            }
            TransformerDupInputs => "extern transformer dup_inputs(foo: Bar, foo: Baz): Out",
            TransformerReservedNames => {
                "extern transformer reserved(transformer: Type, extern: Type): out"
            }
        }
    }
}

type SyntaxNode = rowan::SyntaxNode<crate::DdlogLanguage>;
type SyntaxElement = rowan::SyntaxElement<crate::DdlogLanguage>;

fn pretty_print(node: &SyntaxNode) -> String {
    let mut out = String::new();
    let mut stack = vec![SyntaxElement::Node(node.clone())];
    while let Some(item) = stack.pop() {
        match item {
            SyntaxElement::Token(t) => out.push_str(t.text()),
            SyntaxElement::Node(n) => stack.extend(n.children_with_tokens().rev()),
        }
    }
    out
}

fn normalise_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn assert_parse_success(parsed: &crate::Parsed, expected: &str) {
    assert!(parsed.errors().is_empty());
    assert_eq!(pretty_print(parsed.root().syntax()), expected);
    assert_eq!(parsed.root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
}

fn assert_parse_has_errors(parsed: &crate::Parsed) {
    assert!(!parsed.errors().is_empty());
}

struct RelationSpec<'a> {
    name: &'a str,
    input: bool,
    output: bool,
    columns: Vec<(&'a str, &'a str)>,
    pk: Option<Vec<&'a str>>,
}
impl<'a> RelationSpec<'a> {
    fn new(name: &'a str) -> Self {
        Self {
            name,
            input: false,
            output: false,
            columns: vec![],
            pk: None,
        }
    }
    fn input(mut self) -> Self {
        self.input = true;
        self
    }
    fn output(mut self) -> Self {
        self.output = true;
        self
    }
    fn column(mut self, name: &'a str, ty: &'a str) -> Self {
        self.columns.push((name, ty));
        self
    }
    fn pk(mut self, cols: Vec<&'a str>) -> Self {
        self.pk = Some(cols);
        self
    }
    fn assert(self, r: &Relation) {
        assert_eq!(r.name(), Some(self.name.into()));
        assert_eq!(r.is_input(), self.input);
        assert_eq!(r.is_output(), self.output);
        assert_eq!(
            r.columns(),
            self.columns
                .iter()
                .map(|(n, t)| ((*n).into(), (*t).into()))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            r.primary_key(),
            self.pk.map(|v| v.into_iter().map(String::from).collect())
        );
    }
}

struct IndexSpec<'a> {
    name: &'a str,
    relation: &'a str,
    columns: Vec<&'a str>,
}
impl<'a> IndexSpec<'a> {
    fn new(name: &'a str, relation: &'a str) -> Self {
        Self {
            name,
            relation,
            columns: vec![],
        }
    }
    fn column(mut self, col: &'a str) -> Self {
        self.columns.push(col);
        self
    }
    fn assert(self, i: &Index) {
        assert_eq!(i.name(), Some(self.name.into()));
        assert_eq!(i.relation(), Some(self.relation.into()));
        assert_eq!(
            i.columns(),
            self.columns
                .iter()
                .map(|c| (*c).to_string())
                .collect::<Vec<_>>()
        );
    }
}

struct FnSpec<'a> {
    name: &'a str,
    ext: bool,
    params: Vec<(&'a str, &'a str)>,
    ret: Option<&'a str>,
}
impl<'a> FnSpec<'a> {
    fn new(name: &'a str) -> Self {
        Self {
            name,
            ext: false,
            params: vec![],
            ret: None,
        }
    }
    fn extern_(mut self) -> Self {
        self.ext = true;
        self
    }
    fn param(mut self, name: &'a str, ty: &'a str) -> Self {
        self.params.push((name, ty));
        self
    }
    fn ret(mut self, ty: &'a str) -> Self {
        self.ret = Some(ty);
        self
    }
    fn assert(self, f: &Function) {
        assert_eq!(f.name(), Some(self.name.into()));
        assert_eq!(f.is_extern(), self.ext);
        assert_eq!(
            f.parameters(),
            self.params
                .iter()
                .map(|(n, t)| ((*n).into(), (*t).into()))
                .collect::<Vec<_>>()
        );
        assert_eq!(f.return_type(), self.ret.map(str::to_string));
    }
}

struct TransformerSpec<'a> {
    name: &'a str,
    inputs: Vec<(&'a str, &'a str)>,
    outputs: Vec<&'a str>,
}
impl<'a> TransformerSpec<'a> {
    fn new(name: &'a str) -> Self {
        Self {
            name,
            inputs: vec![],
            outputs: vec![],
        }
    }
    fn input(mut self, name: &'a str, ty: &'a str) -> Self {
        self.inputs.push((name, ty));
        self
    }
    fn output(mut self, ty: &'a str) -> Self {
        self.outputs.push(ty);
        self
    }
    fn assert(self, t: &Transformer) {
        assert_eq!(t.name(), Some(self.name.into()));
        assert_eq!(
            t.inputs(),
            self.inputs
                .iter()
                .map(|(n, ty)| ((*n).into(), (*ty).into()))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            t.outputs(),
            self.outputs
                .iter()
                .map(|o| (*o).to_string())
                .collect::<Vec<_>>()
        );
    }
}

#[rstest]
#[case(TestProgram::SimpleProg)]
#[case(TestProgram::ComplexProg)]
#[case(TestProgram::MultilineRelation)]
fn round_trip_program(#[case] prog: TestProgram) {
    let src = prog.source();
    let parsed = parse(src);
    assert_parse_success(&parsed, src);
}

#[rstest]
#[case(TestProgram::ComplexProg, 2)]
#[case(TestProgram::SimpleProg, 1)]
#[case(TestProgram::EmptyProg, 0)]
fn relation_counts(#[case] prog: TestProgram, #[case] expected: usize) {
    let parsed = parse(prog.source());
    assert_eq!(parsed.root().relations().len(), expected);
}

#[rstest]
#[case(TestProgram::InputRelationPk, RelationSpec::new("User").input().column(USER_ID, "u32").column(USERNAME, "string").pk(vec![USER_ID]))]
#[case(TestProgram::OutputRelationNoPk, RelationSpec::new("Alert").output().column("message", "string").column("timestamp", "u64"))]
#[case(TestProgram::InternalRelationCompoundPk, RelationSpec::new("UserSession").column(USER_ID, "u32").column("session_id", "string").column("start_time", "u64").pk(vec![USER_ID, "session_id"]))]
fn relation_parsing(#[case] prog: TestProgram, #[case] spec: RelationSpec) {
    let parsed = parse(prog.source());
    let rel = parsed.root().relations().first().expect("relation missing");
    spec.assert(rel);
}

#[rstest]
#[case(TestProgram::RelationUnbalancedParentheses)]
#[case(TestProgram::RelationEmptyColumns)]
#[case(TestProgram::RelationWhitespaceColumns)]
#[case(TestProgram::RelationInvalidPk)]
#[case(TestProgram::RelationPkEmpty)]
#[case(TestProgram::RelationPkTrailingComma)]
fn relation_invalid(#[case] prog: TestProgram) {
    let parsed = parse(prog.source());
    assert_parse_has_errors(&parsed);
    assert!(parsed.root().relations().is_empty());
}

#[rstest]
#[case(TestProgram::IndexSingleColumn, IndexSpec::new("Idx_User_username", "User").column(USERNAME))]
#[case(TestProgram::IndexMultiColumn, IndexSpec::new("Idx_Session_user_time", "UserSession").column(USER_ID).column("start_time"))]
#[case(TestProgram::IndexNestedFunction, IndexSpec::new("Idx_lower_username", "User").column("lower(username)"))]
#[case(TestProgram::IndexWhitespaceVariations, IndexSpec::new("Idx_User_ws", "User").column(USERNAME))]
fn index_parsing(#[case] prog: TestProgram, #[case] spec: IndexSpec) {
    let parsed = parse(prog.source());
    let idx = parsed.root().indexes().first().expect("index missing");
    spec.assert(idx);
}

#[rstest]
#[case(TestProgram::IndexInvalidMissingOn)]
#[case(TestProgram::IndexUnbalancedParentheses)]
fn index_errors(#[case] prog: TestProgram) {
    let parsed = parse(prog.source());
    assert_parse_has_errors(&parsed);
    assert!(parsed.root().indexes().is_empty());
}

#[rstest]
#[case(TestProgram::SimpleRule)]
#[case(TestProgram::MultiLiteralRule)]
#[case(TestProgram::FactRule)]
fn rule_parsing(#[case] prog: TestProgram) {
    let src = prog.source();
    let parsed = parse(src);
    let rule = parsed.root().rules().first().expect("rule missing");
    assert_eq!(pretty_print(rule.syntax()), src);
}

#[rstest]
#[case(concat!(":- User(", USER_ID, ", ", USERNAME, ", _)."))]
#[case("UserLogin(username, session_id) :- .")]
#[case("UserLogin(username, session_id) User(user_id, username, _).")]
#[case(concat!("UserLogin(", USERNAME, ", session_id) :- User(", USER_ID, ", ", USERNAME, ", _)"))]
fn rule_errors(#[case] src: &str) {
    let parsed = parse(src);
    assert_parse_has_errors(&parsed);
}

#[rstest]
#[case("import standard_library", vec![("standard_library", None)])]
#[case("import collections::vector as vec", vec![("collections::vector", Some("vec"))])]
#[case("import a\nimport b as c", vec![("a", None), ("b", Some("c"))])]
fn import_parsing(#[case] src: &str, #[case] expected: Vec<(&str, Option<&str>)>) {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
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
    let parsed = parse(src);
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
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let def = parsed
        .root()
        .type_defs()
        .first()
        .expect("typedef not found");
    assert_eq!(def.name(), Some(expect.0.into()));
    assert_eq!(def.is_extern(), expect.1);
}

#[rstest]
#[case(TestProgram::ExternFunction, FnSpec::new("hash").extern_().param("data", "string").ret("u64"))]
#[case(TestProgram::FunctionWithBody, FnSpec::new("to_uppercase").param("s", "string").ret("string"))]
#[case(TestProgram::FunctionNoReturn, FnSpec::new("log_message").param("msg", "string"))]
#[case(TestProgram::FunctionNoParams, FnSpec::new("greet").ret("string"))]
#[case(TestProgram::FunctionMultiParams, FnSpec::new("concat").param("a", "string").param("b", "string").ret("string"))]
#[case(TestProgram::FunctionComplexParams, FnSpec::new("complex").param("p", "(u32,(u8,string))").ret("bool"))]
#[case(TestProgram::FunctionWsComments, FnSpec::new("spaced").param("x", "string").ret("u8"))]
#[case(TestProgram::FunctionGenericParams, FnSpec::new("example").param("arg", "Vec<(u32,string)>").param("map", "Map<string,u64>").ret("bool"))]
#[case(TestProgram::FunctionNestedGenerics, FnSpec::new("test").param("p", "Vec<Map<string,Vec<u8>>>").param("arr", "[Vec<u32>]").ret("bool"))]
#[case(TestProgram::FunctionShiftParam, FnSpec::new("shift").param("x", "Vec<<u8>>").ret("bool"))]
fn function_parsing(#[case] prog: TestProgram, #[case] spec: FnSpec) {
    let parsed = parse(prog.source());
    let func = parsed.root().functions().first().expect("function missing");
    spec.assert(func);
}

#[rstest]
#[case(TestProgram::ExternFunctionMissingColon)]
#[case(TestProgram::FunctionUnterminatedBody)]
#[case(TestProgram::FunctionUnclosedParams)]
fn function_errors(#[case] prog: TestProgram) {
    let parsed = parse(prog.source());
    assert_parse_has_errors(&parsed);
    assert!(parsed.root().functions().is_empty());
}

#[rstest]
#[case(TestProgram::TransformerSingleIo, TransformerSpec::new("normalize").input("input", "UnnormalizedData").output("NormalizedData"))]
#[case(TestProgram::TransformerMultiIo, TransformerSpec::new("correlate").input("users", "User").input("sessions", "UserSession").output("UserActivity").output("SessionAlerts"))]
#[case(TestProgram::TransformerNoInputs, TransformerSpec::new("no_inputs").output("OutputType"))]
#[case(TestProgram::TransformerExtraWs, TransformerSpec::new("spaced").input("foo", "Bar").input("baz", "Qux").output("Out1").output("Out2"))]
#[case(TestProgram::TransformerDupInputs, TransformerSpec::new("dup_inputs").input("foo", "Bar").input("foo", "Baz").output("Out"))]
#[case(TestProgram::TransformerReservedNames, TransformerSpec::new("reserved").input("transformer", "Type").input("extern", "Type").output("out"))]
fn transformer_parsing(#[case] prog: TestProgram, #[case] spec: TransformerSpec) {
    let parsed = parse(prog.source());
    let t = parsed
        .root()
        .transformers()
        .first()
        .expect("transformer missing");
    spec.assert(t);
}

#[rstest]
#[case(TestProgram::TransformerInvalid)]
#[case(TestProgram::TransformerNoOutputs)]
fn transformer_errors(#[case] prog: TestProgram) {
    let parsed = parse(prog.source());
    assert_parse_has_errors(&parsed);
    assert!(parsed.root().transformers().is_empty());
}
