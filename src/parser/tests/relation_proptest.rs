//! Property tests for relation declaration forms.
//!
//! The hand-written matrix pins representative cases. These generated cases
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

#[derive(Debug, Clone)]
struct GeneratedRelation {
    role: GeneratedRole,
    kind: GeneratedKind,
    body: GeneratedBody,
    is_ref: bool,
    has_primary_key: bool,
}

impl GeneratedRelation {
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
            GeneratedBody::Record => "R(id: u32)",
            GeneratedBody::Bracket => "R[Vec<u32>]",
        });

        let mut source = parts.join(" ");
        if self.has_primary_key {
            source.push_str(" primary key (id)");
        }
        source
    }
}

impl GeneratedRole {
    fn expected(self) -> RelationRole {
        match self {
            Self::Absent => RelationRole::Internal,
            Self::Input => RelationRole::Input,
            Self::Output => RelationRole::Output,
        }
    }

    fn keyword(self) -> Option<&'static str> {
        match self {
            Self::Absent => None,
            Self::Input => Some("input"),
            Self::Output => Some("output"),
        }
    }
}

impl GeneratedKind {
    fn expected(self) -> RelationKind {
        match self {
            Self::Absent | Self::Relation => RelationKind::Relation,
            Self::Stream => RelationKind::Stream,
            Self::Multiset => RelationKind::Multiset,
        }
    }

    fn keyword(self) -> Option<&'static str> {
        match self {
            Self::Absent => None,
            Self::Relation => Some("relation"),
            Self::Stream => Some("stream"),
            Self::Multiset => Some("multiset"),
        }
    }
}

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
        any::<bool>(),
    )
        .prop_map(|(role, kind, body, is_ref, wants_primary_key)| {
            let has_primary_key =
                wants_primary_key && role == GeneratedRole::Input && body == GeneratedBody::Record;
            GeneratedRelation {
                role,
                kind,
                body,
                is_ref,
                has_primary_key,
            }
        })
}

proptest! {
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
                    RelationBody::Fields(vec![("id".into(), "u32".into())])
                );
                prop_assert_eq!(relation.element_type(), None);
            }
            GeneratedBody::Bracket => {
                prop_assert_eq!(relation.body(), RelationBody::ElementType("Vec<u32>".into()));
                prop_assert_eq!(relation.columns(), Vec::<(String, String)>::new());
                let element_type = relation.element_type();
                prop_assert_eq!(element_type.as_deref(), Some("Vec<u32>"));
            }
        }

        let expected_primary_key = generated
            .has_primary_key
            .then(|| vec!["id".to_string()]);
        prop_assert_eq!(relation.primary_key(), expected_primary_key);
    }
}
