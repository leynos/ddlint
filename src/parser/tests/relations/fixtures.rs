//! Relation declaration sources shared by the relation parsing tests.
//!
//! Each fixture returns one declaration the tests parse without further setup,
//! so the test module itself stays within the module size budget.

use fixture_lints::allow_fixture_expansion_lints;
use rstest::fixture;

#[allow_fixture_expansion_lints]
#[fixture]
pub fn input_relation_pk() -> &'static str {
    "input relation User(user_id: u32, username: string) primary key (user_id)"
}

#[allow_fixture_expansion_lints]
#[fixture]
pub fn output_relation_no_pk() -> &'static str {
    "output relation Alert(message: string, timestamp: u64)"
}

#[allow_fixture_expansion_lints]
#[fixture]
pub fn internal_relation_no_pk() -> &'static str {
    "relation UserSession(user_id: u32, session_id: string, start_time: u64)"
}

#[allow_fixture_expansion_lints]
#[fixture]
pub fn multiline_relation() -> &'static str {
    "input relation Log(\n    id: u32,\n    message: string\n) primary key (id)\n"
}

#[allow_fixture_expansion_lints]
#[fixture]
pub fn relation_unbalanced_parentheses() -> &'static str {
    "relation Foo(x: u32"
}
