#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// ^^^^^^^^^^^^^^^^^^^^^
// Because our naming conventions in C are not necessarily
// the same as in Rust.
// For our binding, it should be ok.

#![allow(dead_code)]
// ^^^^^^^^^^^^^^^^
// This one is needed because there are a few type aliases
// unused in the generated code.

pub use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter, Error};

include!(concat!(env!("OUT_DIR"), "/gnc-core.rs"));

pub type VExpr = VExpr_s;
pub type VPattern = VPattern_s;

#[derive(Clone)]
pub struct Environment<'a> {
    pub values: BTreeMap<&'a str, Value<'a>>,
    pub cons: BTreeSet<&'a str>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        Environment { values: BTreeMap::new(), cons: BTreeSet::new() }
    }

    pub fn with_bindings<T, F>(&mut self, new_vals: &BTreeMap<&'a str, Value<'a>>, action: F) -> T
      where F: Fn(&mut Environment<'a>) -> T {
        let mut dups: BTreeMap<&str, Value> = BTreeMap::new();
        for (name, val) in new_vals.into_iter()  {
            self.values.insert(name, val.clone())
                       .map(|v| dups.insert(name, v));
        }

        let result = action(self);

        for name in new_vals.keys().into_iter() {
            self.values.remove(name);
        }
        self.values.append(&mut dups);

        result
    }
}

#[derive(Clone)]
pub enum Value<'a> {
    VInteger(i64),
    VDouble(f64),
    VCharacter(char),
    VLambda(VPattern, VExpr, Environment<'a>),
    VTuple(Vec<Value<'a>>),
    VPrim(fn(Value<'a>, &mut Environment<'a>) -> Value<'a>),
    VConstructor(&'a str, Vec<Value<'a>>),
    VUnevaluated(VExpr),
}

impl <'a> Display for Value<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::VInteger(i) => write!(f, "{}", i),
            Value::VDouble(d) => write!(f, "{}", d),
            Value::VCharacter(c) => write!(f, "{}", c),
            Value::VTuple(vs) => {
                let mut comma_separated = String::new();
                for value in vs.into_iter()
                               .take(vs.len() - 1) {
                    comma_separated += format!("{}, ", value).as_str();
                }
                vs.into_iter()
                  .last()
                  .map(|v| { comma_separated += format!("{}", v).as_str(); });

                write!(f, "({})", comma_separated)
            },
            Value::VConstructor(name, vals) => {
                let mut pprint = name.to_string();
                vals.into_iter()
                    .map(|v| match v {
                        Value::VConstructor(_, vals)
                            if vals.len() > 0 => format!("({}) ", v),
                        _ => format!("{} ", v),
                    })
                    .for_each(|pretty_val| { pprint += pretty_val.as_str(); });

                write!(f, "{}", pprint)
            },
            _ => Err(Error),
        }
    }
}

pub const EId: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrEId;
pub const EInteger: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrEInteger;
pub const EDouble: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrEDouble;
pub const ECharacter: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrECharacter;
pub const ELambda: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrELambda;
pub const EApplication: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrEApplication;
pub const ETuple: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrETuple;
pub const ETypeHole: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrETypeHole;
pub const EMatch: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrEMatch;
pub const ELet: VExpr_s_VExpr_Cons = VExpr_s_VExpr_Cons_CrELet;

pub const PInteger: VPattern_s_VPattern_Cons = VPattern_s_VPattern_Cons_CrPInteger;
pub const PDouble: VPattern_s_VPattern_Cons = VPattern_s_VPattern_Cons_CrPDouble;
pub const PCharacter: VPattern_s_VPattern_Cons = VPattern_s_VPattern_Cons_CrPCharacter;
pub const PId: VPattern_s_VPattern_Cons = VPattern_s_VPattern_Cons_CrPId;
pub const PWildcard: VPattern_s_VPattern_Cons = VPattern_s_VPattern_Cons_CrPWildcard;
pub const PTuple: VPattern_s_VPattern_Cons = VPattern_s_VPattern_Cons_CrPTuple;
pub const PConstructor: VPattern_s_VPattern_Cons = VPattern_s_VPattern_Cons_CrPConstructor;