//! Predefined DDL programs used across parser tests.
//!
//! Each enum groups programs by domain to keep the main test file concise.

#[derive(Copy, Clone)]
pub enum BasicProgram {
    Simple,
    Complex,
    Empty,
    MultilineRelation,
}

impl BasicProgram {
    pub fn source(self) -> &'static str {
        match self {
            Self::Simple => "input relation R(x: u32);",
            Self::Complex => "input relation R(x: u32);\noutput relation S(y: string);",
            Self::Empty => "",
            Self::MultilineRelation => "input relation Log(\n    id: u32,\n    message: string\n) primary key (id)\n",
        }
    }
}

#[derive(Copy, Clone)]
pub enum RelationProgram {
    InputRelationPk,
    OutputRelationNoPk,
    InternalRelationCompoundPk,
    RelationUnbalancedParentheses,
    RelationEmptyColumns,
    RelationWhitespaceColumns,
    RelationInvalidPk,
    RelationPkEmpty,
    RelationPkTrailingComma,
}

impl RelationProgram {
    pub fn source(self) -> &'static str {
        match self {
            Self::InputRelationPk => {
                "input relation User(user_id: u32, username: string) primary key (user_id)"
            }
            Self::OutputRelationNoPk => {
                "output relation Alert(message: string, timestamp: u64)"
            }
            Self::InternalRelationCompoundPk => {
                "relation UserSession(user_id: u32, session_id: string, start_time: u64) primary key (user_id, session_id)"
            }
            Self::RelationUnbalancedParentheses => "relation Foo(x: u32",
            Self::RelationEmptyColumns => "relation Foo()",
            Self::RelationWhitespaceColumns => "relation Foo(   )",
            Self::RelationInvalidPk => "relation Foo(x: u32) primary key x",
            Self::RelationPkEmpty => "relation Foo(x: u32) primary key ()",
            Self::RelationPkTrailingComma => "relation Foo(x: u32) primary key (x,)",
        }
    }
}

#[derive(Copy, Clone)]
pub enum IndexProgram {
    IndexSingleColumn,
    IndexMultiColumn,
    IndexInvalidMissingOn,
    IndexNestedFunction,
    IndexUnbalancedParentheses,
    IndexWhitespaceVariations,
}

impl IndexProgram {
    pub fn source(self) -> &'static str {
        match self {
            Self::IndexSingleColumn => "index Idx_User_username on User(username)",
            Self::IndexMultiColumn => {
                "index Idx_Session_user_time on UserSession(user_id, start_time)"
            }
            Self::IndexInvalidMissingOn => "index Idx_Invalid User(username)",
            Self::IndexNestedFunction => {
                "index Idx_lower_username on User(lower(username))"
            }
            Self::IndexUnbalancedParentheses => {
                "index Idx_Unbalanced on User(lower(username)"
            }
            Self::IndexWhitespaceVariations => {
                "  index  Idx_User_ws \t on\n  User (\n    username  )  "
            }
        }
    }
}

#[derive(Copy, Clone)]
pub enum RuleProgram {
    SimpleRule,
    MultiLiteralRule,
    FactRule,
}

impl RuleProgram {
    pub fn source(self) -> &'static str {
        match self {
            Self::SimpleRule => {
                "ActiveUser(user_id) :- User(user_id, _, true)."
            }
            Self::MultiLiteralRule => {
                "UserLogin(username, session_id) :- User(user_id, username, _), UserSession(user_id, session_id, _)."
            }
            Self::FactRule => "SystemAlert(\"System is now online.\").",
        }
    }
}

#[derive(Copy, Clone)]
pub enum FunctionProgram {
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
}

impl FunctionProgram {
    pub fn source(self) -> &'static str {
        match self {
            Self::ExternFunction => "extern function hash(data: string): u64\n",
            Self::FunctionWithBody => "function to_uppercase(s: string): string {\n}\n",
            Self::FunctionNoReturn => "function log_message(msg: string) {\n}\n",
            Self::ExternFunctionMissingColon => {
                "extern function missing_colon u32\n"
            }
            Self::FunctionUnterminatedBody => "function foo() {",
            Self::FunctionNoParams => "function greet(): string {\n}\n",
            Self::FunctionMultiParams => {
                "function concat(a: string, b: string): string {\n}\n"
            }
            Self::FunctionComplexParams => {
                "function complex(p: (u32,(u8,string))): bool {\n}\n"
            }
            Self::FunctionWsComments => {
                "function  spaced  (  x : string )  :  u8 { /*empty*/ }\n"
            }
            Self::FunctionUnclosedParams => "function bad(a: string { }\n",
            Self::FunctionGenericParams => {
                "function example(arg: Vec<(u32,string)>, map: Map<string,u64>): bool {\n}\n"
            }
            Self::FunctionNestedGenerics => {
                "function test(p: Vec<Map<string,Vec<u8>>>, arr: [Vec<u32>]): bool {}\n"
            }
            Self::FunctionShiftParam => "function shift(x: Vec<<u8>>): bool {}\n",
        }
    }
}

#[derive(Copy, Clone)]
pub enum TransformerProgram {
    TransformerSingleIo,
    TransformerMultiIo,
    TransformerInvalid,
    TransformerNoInputs,
    TransformerNoOutputs,
    TransformerExtraWs,
    TransformerDupInputs,
    TransformerReservedNames,
}

impl TransformerProgram {
    pub fn source(self) -> &'static str {
        match self {
            Self::TransformerSingleIo => {
                "extern transformer normalize(input: UnnormalizedData): NormalizedData"
            }
            Self::TransformerMultiIo => {
                "extern transformer correlate(users: User, sessions: UserSession): UserActivity, SessionAlerts"
            }
            Self::TransformerInvalid => {
                "extern transformer incomplete_transformer(input: SomeData):"
            }
            Self::TransformerNoInputs => {
                "extern transformer no_inputs(): OutputType"
            }
            Self::TransformerNoOutputs => {
                "extern transformer no_outputs(input: InputType):"
            }
            Self::TransformerExtraWs => {
                " extern   transformer   spaced  (  foo  :  Bar  ,  baz : Qux )  :  Out1 , Out2 "
            }
            Self::TransformerDupInputs => {
                "extern transformer dup_inputs(foo: Bar, foo: Baz): Out"
            }
            Self::TransformerReservedNames => {
                "extern transformer reserved(transformer: Type, extern: Type): out"
            }
        }
    }
}

