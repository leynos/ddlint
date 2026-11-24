//! Strategies for typed primary-key access property tests.
//!
//! The generated source text is the oracle for `INV-AGREE`, `INV-INDEP`, and
//! `INV-SHIFT`: properties compare the accessor's output with the names and
//! expression text recorded here, never with values read back from the same
//! CST tokens. Each value also carries a coverage class so the class-coverage
//! test can prove the properties are not exercised by one shape only.

use std::ops::RangeInclusive;

use proptest::prelude::*;

/// Coverage class of one generated key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum KeyClass {
    /// A key with a well-formed expression.
    WithExpression,
    /// A key whose binder list is the whole clause.
    WithoutExpression,
    /// A key with more than one binder.
    MultiBinder,
    /// An expression followed by a trailing line comment.
    TrailingComment,
    /// An expression that spans lines inside open delimiters.
    MultilineDelimiter,
    /// A balanced but syntactically invalid expression.
    InvalidMutation,
}

/// Relation prefix shared by a keyed declaration and its keyless twin.
#[derive(Debug, Clone)]
struct Prefix {
    /// Prefix that introduces the key clause, including separating trivia.
    keyed: String,
    /// Prefix used when the key clause is removed.
    keyless: String,
}

/// Binder names together with the exact text written between the parentheses.
#[derive(Debug, Clone)]
struct BinderList {
    /// Names in declaration order.
    names: Vec<String>,
    /// Text between the parentheses, including any inline trivia.
    text: String,
}

impl BinderList {
    /// Join `names`, placing `separators[i]` before `names[i + 1]`.
    fn new(names: &[&'static str], separators: &[&'static str]) -> Self {
        let mut text = String::new();
        for (index, name) in names.iter().enumerate() {
            if index > 0 {
                text.push_str(separators.get(index - 1).copied().unwrap_or(", "));
            }
            text.push_str(name);
        }
        Self {
            names: names.iter().map(|name| (*name).to_owned()).collect(),
            text,
        }
    }
}

/// The parts of a generated key apart from its relation prefix.
struct KeyParts {
    /// Binder names written in the clause.
    binders: Vec<String>,
    /// Exact text between the binder list parentheses.
    binder_list: String,
    /// Expression text written after the binder list.
    expression: Option<String>,
    /// Coverage class used by the class-coverage test.
    class: KeyClass,
}

impl KeyParts {
    /// Parts for a key whose expression, if any, must parse.
    fn valid(binder_list: &BinderList, expression: Option<String>, class: KeyClass) -> Self {
        Self {
            binders: binder_list.names.clone(),
            binder_list: binder_list.text.clone(),
            expression,
            class,
        }
    }

    /// Parts for a key whose balanced expression must fail to parse.
    fn invalid(binder_list: &BinderList, expression: String) -> Self {
        Self {
            binders: binder_list.names.clone(),
            binder_list: binder_list.text.clone(),
            expression: Some(expression),
            class: KeyClass::InvalidMutation,
        }
    }
}

/// One generated relation declaration and the values it must expose.
#[derive(Debug, Clone)]
pub(super) struct GeneratedKey {
    /// Source text handed to the parser.
    pub(super) source: String,
    /// The same declaration with its key clause removed, for `INV-TERM`.
    pub(super) keyless_source: String,
    /// Binder names written in the clause.
    pub(super) binders: Vec<String>,
    /// Expression text the accessor must report, if one is written.
    pub(super) expression: Option<String>,
    /// Coverage class used by the class-coverage test.
    pub(super) class: KeyClass,
}

impl GeneratedKey {
    /// The generated source followed by `suffix`.
    pub(super) fn source_with_suffix(&self, suffix: &str) -> String {
        format!("{}{suffix}", self.source)
    }

    /// Append a line comment after the keyed source.
    fn with_trailing_comment(mut self) -> Self {
        self.source.push_str(" // trailing note");
        self
    }
}

/// A relation name whose length varies the expression's start offset.
fn relation_name_strategy() -> impl Strategy<Value = String> {
    prop::sample::select(vec!["R", "Rel", "Relation", "BookRowSource"]).prop_map(str::to_owned)
}

/// Column list spellings that keep the same columns but vary layout.
fn columns_strategy() -> impl Strategy<Value = &'static str> {
    prop::sample::select(vec![
        "a: u32, b: u32, c: u32",
        "a: u32,b: u32,c: u32",
        "a: u32 ,b: u32 ,c: u32",
    ])
}

/// Trivia between the column list and the `primary` keyword.
fn clause_gap_strategy() -> impl Strategy<Value = &'static str> {
    prop::sample::select(vec![" ", "  ", "\t", " /* gap */ "])
}

/// Prefix shared by both arms of a keyed and keyless declaration.
fn prefix_strategy() -> impl Strategy<Value = Prefix> {
    (
        relation_name_strategy(),
        columns_strategy(),
        clause_gap_strategy(),
    )
        .prop_map(|(name, columns, gap)| Prefix {
            keyed: format!("input relation {name}({columns}){gap}"),
            keyless: format!("input relation {name}({columns})"),
        })
}

/// A leaf expression: a bare variable or a field access on a variable.
fn leaf_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        prop::sample::select(vec!["a", "b", "c"]).prop_map(str::to_owned),
        (
            prop::sample::select(vec!["a", "b", "c"]),
            prop::sample::select(vec!["id", "name", "value"]),
        )
            .prop_map(|(binder, field)| format!("{binder}.{field}")),
    ]
}

/// A well-formed expression from the `DDlog` expression core.
///
/// Depth is bounded at three and every form closes its delimiters, so the
/// scanner can continue across interior line breaks.
fn expression_strategy() -> impl Strategy<Value = String> {
    leaf_strategy().prop_recursive(3, 16, 4, |inner| {
        prop_oneof![
            (leaf_strategy(), leaf_strategy())
                .prop_map(|(left, right)| format!("({left}, {right})")),
            (leaf_strategy(), inner.clone()).prop_map(|(left, right)| format!("({left}, {right})")),
            (inner.clone(), leaf_strategy()).prop_map(|(left, right)| format!("({left}, {right})")),
            (leaf_strategy(), leaf_strategy())
                .prop_map(|(function, argument)| format!("{function}({argument})")),
        ]
    })
}

/// Trivia written between binder names, all inside open parentheses.
fn binder_separator_strategy() -> impl Strategy<Value = &'static str> {
    prop::sample::select(vec![", ", ",", " , ", ",\t", ",\n    "])
}

/// Binder lists drawn with the given name count.
fn binder_list_from(names: RangeInclusive<usize>) -> impl Strategy<Value = BinderList> {
    (
        prop::collection::vec(prop::sample::select(vec!["a", "b", "c"]), names),
        prop::collection::vec(binder_separator_strategy(), 0..=2),
    )
        .prop_map(|(identifiers, separators)| BinderList::new(&identifiers, &separators))
}

/// Binder lists of one to three identifiers.
fn binder_list_strategy() -> impl Strategy<Value = BinderList> { binder_list_from(1..=3) }

/// Binder lists of two to three identifiers.
fn multi_binder_list_strategy() -> impl Strategy<Value = BinderList> { binder_list_from(2..=3) }

/// Build a generated key, including its keyless twin.
fn build_key(prefix: &Prefix, parts: KeyParts) -> GeneratedKey {
    let keyed = &prefix.keyed;
    let binder_list = &parts.binder_list;
    let source = parts.expression.as_ref().map_or_else(
        || format!("{keyed}primary key ({binder_list})"),
        |expression| format!("{keyed}primary key ({binder_list}) {expression}"),
    );
    GeneratedKey {
        source,
        keyless_source: prefix.keyless.clone(),
        binders: parts.binders,
        expression: parts.expression,
        class: parts.class,
    }
}

/// A key carrying a well-formed expression.
fn with_expression_strategy() -> impl Strategy<Value = GeneratedKey> {
    (
        prefix_strategy(),
        binder_list_strategy(),
        expression_strategy(),
    )
        .prop_map(|(prefix, binders, expression)| {
            build_key(
                &prefix,
                KeyParts::valid(&binders, Some(expression), KeyClass::WithExpression),
            )
        })
}

/// A list-form key with no expression.
fn without_expression_strategy() -> impl Strategy<Value = GeneratedKey> {
    (prefix_strategy(), binder_list_strategy()).prop_map(|(prefix, binders)| {
        build_key(
            &prefix,
            KeyParts::valid(&binders, None, KeyClass::WithoutExpression),
        )
    })
}

/// A key with several binders and no expression.
fn multi_binder_strategy() -> impl Strategy<Value = GeneratedKey> {
    (prefix_strategy(), multi_binder_list_strategy()).prop_map(|(prefix, binders)| {
        build_key(
            &prefix,
            KeyParts::valid(&binders, None, KeyClass::MultiBinder),
        )
    })
}

/// An expression followed by a trailing line comment.
fn trailing_comment_strategy() -> impl Strategy<Value = GeneratedKey> {
    (
        prefix_strategy(),
        binder_list_strategy(),
        expression_strategy(),
    )
        .prop_map(|(prefix, binders, expression)| {
            build_key(
                &prefix,
                KeyParts::valid(&binders, Some(expression), KeyClass::TrailingComment),
            )
            .with_trailing_comment()
        })
}

/// An expression that spans lines inside open delimiters.
fn multiline_strategy() -> impl Strategy<Value = GeneratedKey> {
    (
        prefix_strategy(),
        binder_list_strategy(),
        leaf_strategy(),
        leaf_strategy(),
    )
        .prop_map(|(prefix, binders, left, right)| {
            let expression = format!("({left},\n {right})");
            build_key(
                &prefix,
                KeyParts::valid(&binders, Some(expression), KeyClass::MultilineDelimiter),
            )
        })
}

/// A balanced expression that the expression parser rejects.
fn invalid_expression_strategy() -> impl Strategy<Value = String> {
    prop_oneof![Just("a + * 1".to_owned()), Just("(a.id,, b.id)".to_owned()),]
}

/// A balanced expression that the expression parser rejects.
fn invalid_mutation_strategy() -> impl Strategy<Value = GeneratedKey> {
    (
        prefix_strategy(),
        binder_list_strategy(),
        invalid_expression_strategy(),
    )
        .prop_map(|(prefix, binders, expression)| {
            build_key(&prefix, KeyParts::invalid(&binders, expression))
        })
}

/// One pair of sources that share binders but differ in expression validity.
#[derive(Debug, Clone)]
pub(super) struct IndependencePair {
    /// Binder names shared by both sources.
    pub(super) binders: Vec<String>,
    /// Source whose expression parses.
    pub(super) valid_source: String,
    /// Source whose expression is balanced but does not parse.
    pub(super) invalid_source: String,
}

/// Draw a valid expression and an invalid mutation for the same binders.
pub(super) fn independence_pair_strategy() -> impl Strategy<Value = IndependencePair> {
    (
        prefix_strategy(),
        binder_list_strategy(),
        expression_strategy(),
        invalid_expression_strategy(),
    )
        .prop_map(|(prefix, binders, valid, invalid)| {
            let valid_key = build_key(
                &prefix,
                KeyParts::valid(&binders, Some(valid), KeyClass::WithExpression),
            );
            let invalid_key = build_key(&prefix, KeyParts::invalid(&binders, invalid));
            IndependencePair {
                binders: binders.names,
                valid_source: valid_key.source,
                invalid_source: invalid_key.source,
            }
        })
}

/// Every valid coverage class, used by the class-coverage test.
pub(super) const VALID_CLASSES: [KeyClass; 5] = [
    KeyClass::WithExpression,
    KeyClass::WithoutExpression,
    KeyClass::MultiBinder,
    KeyClass::TrailingComment,
    KeyClass::MultilineDelimiter,
];

/// Keys whose expressions must parse.
pub(super) fn valid_key_strategy() -> impl Strategy<Value = GeneratedKey> {
    prop_oneof![
        with_expression_strategy(),
        without_expression_strategy(),
        multi_binder_strategy(),
        trailing_comment_strategy(),
        multiline_strategy(),
    ]
}

/// Keys with a balanced but invalid expression.
pub(super) fn invalid_key_strategy() -> impl Strategy<Value = GeneratedKey> {
    invalid_mutation_strategy()
}

/// Keys that can be followed directly by a statement terminator.
///
/// A trailing line comment would swallow the `;`, so that class is excluded
/// from `INV-TERM`.
pub(super) fn terminator_key_strategy() -> impl Strategy<Value = GeneratedKey> {
    prop_oneof![
        with_expression_strategy(),
        without_expression_strategy(),
        multi_binder_strategy(),
        multiline_strategy(),
    ]
}

/// Every generated class.
pub(super) fn generated_key_strategy() -> impl Strategy<Value = GeneratedKey> {
    prop_oneof![valid_key_strategy(), invalid_key_strategy()]
}
