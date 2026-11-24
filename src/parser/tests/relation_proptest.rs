//! Property tests for relation declaration forms.
//!
//! The handwritten matrix pins representative cases. These generated cases
//! cover the role, kind, body, ref, and primary-key space more densely while
//! staying within the accepted grammar.

use proptest::prelude::*;

use crate::parser::ast::{RelationBody, RelationKind, RelationRole};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GeneratedRole {
    Absent,
    Input,
    Output,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GeneratedKind {
    Absent,
    Relation,
    Stream,
    Multiset,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GeneratedBody {
    Record,
    Bracket,
}

#[derive(Debug, Clone, Copy)]
enum GeneratedPrimaryKey {
    Single,
    Compound,
}

#[derive(Debug, Clone)]
struct GeneratedRelation {
    role: GeneratedRole,
    kind: GeneratedKind,
    body: GeneratedBody,
    is_ref: bool,
    primary_key: Option<GeneratedPrimaryKey>,
}

impl GeneratedRelation {
    /// Renders the declaration in the order the grammar expects: role, kind,
    /// ref marker, body, then the optional key clause.
    fn source(&self) -> String {
        let mut parts = Vec::new();
        if let Some(role) = self.role.keyword() {
            parts.push(role);
        }
        if let Some(kind) = self.kind.keyword() {
            parts.push(kind);
        }
        if self.is_ref {
            parts.push("&");
        }
        parts.push(match self.body {
            GeneratedBody::Record => "R(id: u32, status: string)",
            GeneratedBody::Bracket => "R[Vec<u32>]",
        });

        let mut source = parts.join(" ");
        if let Some(primary_key) = self.primary_key {
            source.push_str(primary_key.source());
        }
        source
    }
}

impl GeneratedPrimaryKey {
    /// The clause text, including its leading space so it appends directly to
    /// the rendered body.
    fn source(self) -> &'static str {
        match self {
            Self::Single => " primary key (id)",
            Self::Compound => " primary key (id, status)",
        }
    }

    /// The binder names the accessor should report for this key form.
    fn expected(self) -> Vec<&'static str> {
        match self {
            Self::Single => vec!["id"],
            Self::Compound => vec!["id", "status"],
        }
    }
}

impl GeneratedRole {
    /// The `RelationRole` the AST should report for this generated role.
    fn expected(self) -> RelationRole {
        match self {
            Self::Absent => RelationRole::Internal,
            Self::Input => RelationRole::Input,
            Self::Output => RelationRole::Output,
        }
    }

    /// The source keyword for this role, or `None` when the role is implicit.
    fn keyword(self) -> Option<&'static str> {
        match self {
            Self::Absent => None,
            Self::Input => Some("input"),
            Self::Output => Some("output"),
        }
    }
}

impl GeneratedKind {
    /// The `RelationKind` the AST should report; an absent keyword defaults to
    /// `relation`.
    fn expected(self) -> RelationKind {
        match self {
            Self::Absent | Self::Relation => RelationKind::Relation,
            Self::Stream => RelationKind::Stream,
            Self::Multiset => RelationKind::Multiset,
        }
    }

    /// The source keyword for this kind, or `None` when the kind is implicit.
    fn keyword(self) -> Option<&'static str> {
        match self {
            Self::Absent => None,
            Self::Relation => Some("relation"),
            Self::Stream => Some("stream"),
            Self::Multiset => Some("multiset"),
        }
    }
}

/// Builds a source-accurate declaration; a primary key is retained only on an
/// `input` record relation, the sole form that accepts one.
fn generated_relation_strategy() -> impl Strategy<Value = GeneratedRelation> {
    (
        prop_oneof![
            Just(GeneratedRole::Absent),
            Just(GeneratedRole::Input),
            Just(GeneratedRole::Output),
        ],
        prop_oneof![
            Just(GeneratedKind::Absent),
            Just(GeneratedKind::Relation),
            Just(GeneratedKind::Stream),
            Just(GeneratedKind::Multiset),
        ],
        prop_oneof![Just(GeneratedBody::Record), Just(GeneratedBody::Bracket)],
        any::<bool>(),
        prop_oneof![
            Just(GeneratedPrimaryKey::Single),
            Just(GeneratedPrimaryKey::Compound),
        ],
    )
        .prop_map(|(role, kind, body, is_ref, primary_key)| {
            let effective_primary_key = (role == GeneratedRole::Input
                && body == GeneratedBody::Record)
                .then_some(primary_key);
            GeneratedRelation {
                role,
                kind,
                body,
                is_ref,
                primary_key: effective_primary_key,
            }
        })
}

proptest! {
    /// Pins that every generated declaration round-trips: the CST text matches
    /// the source, and each typed accessor returns the role, kind, body, ref
    /// flag, and key the generator intended.
    #[test]
    fn relation_forms_preserve_ast_accessors(generated in generated_relation_strategy()) {
        let source = generated.source();
        let parsed = crate::parse(&source);
        prop_assert!(parsed.errors().is_empty(), "source: {source}, errors: {:?}", parsed.errors());
        prop_assert_eq!(parsed.root().syntax().text().to_string(), source.as_str());

        let mut relations = parsed.root().relations();
        prop_assert_eq!(relations.len(), 1, "source: {}", source);
        let relation = relations.remove(0);

        let name = relation.name();
        prop_assert_eq!(name.as_deref(), Some("R"));
        prop_assert_eq!(relation.role(), generated.role.expected());
        prop_assert_eq!(relation.role_keyword_present(), generated.role != GeneratedRole::Absent);
        prop_assert_eq!(relation.kind(), generated.kind.expected());
        prop_assert_eq!(relation.kind_keyword_present(), generated.kind != GeneratedKind::Absent);
        prop_assert_eq!(relation.is_ref(), generated.is_ref);

        match generated.body {
            GeneratedBody::Record => {
                prop_assert_eq!(
                    relation.body(),
                    Ok(RelationBody::Fields(vec![
                        ("id".into(), "u32".into()),
                        ("status".into(), "string".into()),
                    ]))
                );
                prop_assert_eq!(relation.element_type(), Ok(None));
            }
            GeneratedBody::Bracket => {
                prop_assert_eq!(
                    relation.body(),
                    Ok(RelationBody::ElementType("Vec<u32>".into()))
                );
                prop_assert_eq!(relation.columns(), Ok(Vec::<(String, String)>::new()));
                prop_assert_eq!(relation.element_type(), Ok(Some("Vec<u32>".to_owned())));
            }
        }

        let expected_primary_key = generated.primary_key.map(|key| {
            key.expected()
                .into_iter()
                .map(str::to_owned)
                .collect::<Vec<_>>()
        });
        let actual_primary_key = relation
            .primary_key()
            .map(|clause| clause.map(|key| super::helpers::owned_binder_names(&key)));
        prop_assert_eq!(actual_primary_key, Ok(expected_primary_key));
    }
}
