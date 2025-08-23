//! Assertion builders for parser tests.

use crate::ast::{Function, Index, Relation, Transformer};

fn pair_vec(items: &[(&str, &str)]) -> Vec<(String, String)> {
    items
        .iter()
        .map(|(n, t)| ((*n).into(), (*t).into()))
        .collect()
}

fn str_vec(items: &[&str]) -> Vec<String> {
    items.iter().map(|s| (*s).to_string()).collect()
}

macro_rules! assert_entity {
    ($node:expr, $spec:expr, $( $actual:expr => $expected:expr ),* $(,)?) => {{
        assert_eq!($node.name(), Some($spec.name.into()));
        $( assert_eq!($actual, $expected); )*
    }};
}
pub(crate) use assert_entity;

pub struct RelationSpec<'a> {
    name: &'a str,
    input: bool,
    output: bool,
    columns: Vec<(&'a str, &'a str)>,
    pk: Option<Vec<&'a str>>,
}
impl<'a> RelationSpec<'a> {
    pub fn new(name: &'a str) -> Self {
        Self { name, input: false, output: false, columns: vec![], pk: None }
    }
    pub fn input(mut self) -> Self {
        self.input = true;
        self
    }
    pub fn output(mut self) -> Self {
        self.output = true;
        self
    }
    pub fn column(mut self, name: &'a str, ty: &'a str) -> Self {
        self.columns.push((name, ty));
        self
    }
    pub fn pk(mut self, cols: Vec<&'a str>) -> Self {
        self.pk = Some(cols);
        self
    }
    pub fn assert(self, r: &Relation) {
        assert_entity!(
            r,
            self,
            r.is_input() => self.input,
            r.is_output() => self.output,
            r.columns() => pair_vec(&self.columns),
            r.primary_key() => self.pk.map(|v| str_vec(&v))
        );
    }
}

pub struct IndexSpec<'a> {
    name: &'a str,
    relation: &'a str,
    columns: Vec<&'a str>,
}
impl<'a> IndexSpec<'a> {
    pub fn new(name: &'a str, relation: &'a str) -> Self {
        Self { name, relation, columns: vec![] }
    }
    pub fn column(mut self, col: &'a str) -> Self {
        self.columns.push(col);
        self
    }
    pub fn assert(self, i: &Index) {
        assert_entity!(
            i,
            self,
            i.relation() => Some(self.relation.into()),
            i.columns() => str_vec(&self.columns)
        );
    }
}

pub struct FnSpec<'a> {
    name: &'a str,
    ext: bool,
    params: Vec<(&'a str, &'a str)>,
    ret: Option<&'a str>,
}
impl<'a> FnSpec<'a> {
    pub fn new(name: &'a str) -> Self {
        Self { name, ext: false, params: vec![], ret: None }
    }
    pub fn extern_(mut self) -> Self {
        self.ext = true;
        self
    }
    pub fn param(mut self, name: &'a str, ty: &'a str) -> Self {
        self.params.push((name, ty));
        self
    }
    pub fn ret(mut self, ty: &'a str) -> Self {
        self.ret = Some(ty);
        self
    }
    pub fn assert(self, f: &Function) {
        assert_entity!(
            f,
            self,
            f.is_extern() => self.ext,
            f.parameters() => pair_vec(&self.params),
            f.return_type() => self.ret.map(str::to_string)
        );
    }
}

pub struct TransformerSpec<'a> {
    name: &'a str,
    inputs: Vec<(&'a str, &'a str)>,
    outputs: Vec<&'a str>,
}
impl<'a> TransformerSpec<'a> {
    pub fn new(name: &'a str) -> Self {
        Self { name, inputs: vec![], outputs: vec![] }
    }
    pub fn input(mut self, name: &'a str, ty: &'a str) -> Self {
        self.inputs.push((name, ty));
        self
    }
    pub fn output(mut self, ty: &'a str) -> Self {
        self.outputs.push(ty);
        self
    }
    pub fn assert(self, t: &Transformer) {
        assert_entity!(
            t,
            self,
            t.inputs() => pair_vec(&self.inputs),
            t.outputs() => str_vec(&self.outputs)
        );
    }
}

