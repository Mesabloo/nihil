pub use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Error, Formatter};

#[derive(Clone)]
pub enum VPattern<'a> {
    PInteger(i64),
    PDouble(f64),
    PCharacter(char),
    PId(&'a str),
    PWildcard,
    PTuple(Vec<VPattern<'a>>),
    PConstructor(&'a str, Vec<VPattern<'a>>),
}

#[derive(Clone)]
pub enum VExpr<'a> {
    EInteger(i64),
    EDouble(f64),
    ECharacter(char),
    EId(&'a str),
    ELambda(VPattern<'a>, Box<VExpr<'a>>),
    EApplication(Box<VExpr<'a>>, Box<VExpr<'a>>),
    ETuple(Vec<VExpr<'a>>),
    ETypeHole,
    EMatch(Box<VExpr<'a>>, Vec<(VPattern<'a>, VExpr<'a>)>),
    ELet(Vec<(&'a str, VExpr<'a>)>, Box<VExpr<'a>>),
}

#[derive(Clone)]
pub struct Environment<'a> {
    pub values: BTreeMap<&'a str, Value<'a>>,
    pub cons: BTreeSet<&'a str>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        Environment {
            values: BTreeMap::new(),
            cons: BTreeSet::new(),
        }
    }

    pub fn with_bindings<T, F>(&mut self, new_vals: &BTreeMap<&'a str, Value<'a>>, action: F) -> T
    where
        F: Fn(&mut Environment<'a>) -> T,
    {
        let mut dups: BTreeMap<&str, Value> = BTreeMap::new();
        for (name, val) in new_vals.into_iter() {
            self.values
                .insert(name, val.clone())
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
    VLambda(VPattern<'a>, VExpr<'a>, Environment<'a>),
    VTuple(Vec<Value<'a>>),
    VPrim(fn(Value<'a>, &mut Environment<'a>) -> Result<Value<'a>, RuntimeError<'a>>),
    VConstructor(&'a str, Vec<Value<'a>>),
    VUnevaluated(VExpr<'a>),
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::VInteger(i) => write!(f, "{}", i),
            Value::VDouble(d) => write!(f, "{}", d),
            Value::VCharacter(c) => write!(f, "{}", c),
            Value::VTuple(vs) => {
                let mut comma_separated = String::new();
                for value in vs.into_iter().take(vs.len() - 1) {
                    comma_separated += format!("{}, ", value).as_str();
                }
                vs.into_iter().last().map(|v| {
                    comma_separated += format!("{}", v).as_str();
                });

                write!(f, "({})", comma_separated)
            }
            Value::VConstructor(name, vals) => {
                let mut pprint = name.to_string();
                vals.into_iter()
                    .map(|v| match v {
                        Value::VConstructor(_, vals) if vals.len() > 0 => format!("({}) ", v),
                        _ => format!("{} ", v),
                    })
                    .for_each(|pretty_val| {
                        pprint += pretty_val.as_str();
                    });

                write!(f, "{}", pprint)
            }
            _ => Err(Error),
        }
    }
}

pub enum RuntimeError<'a> {
    UnboundName(&'a str),
    NonExhaustivePatternsInMatch,
    NonExhaustivePatternsInLambda,
    IncorrectFunction,
    InvalidTypeHole,
}

impl<'a> Display for RuntimeError<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use RuntimeError::*;

        match self {
            UnboundName(name) => write!(f, "Name '{}' is unbound", name),
            NonExhaustivePatternsInMatch => {
                write!(f, "Non exhaustive patterns in pattern matching")
            }
            NonExhaustivePatternsInLambda => write!(f, "Non exhaustive pattern in lambda function"),
            IncorrectFunction => write!(
                f,
                "Only a function or a constructor can be applied to arguments"
            ),
            InvalidTypeHole => write!(f, "Uncaught typed hole"),
        }
    }
}
