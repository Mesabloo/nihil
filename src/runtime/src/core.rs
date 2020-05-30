pub use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Error, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub enum VPattern {
    PInteger(i64),
    PDouble(f64),
    PCharacter(char),
    PId(String),
    PWildcard,
    PTuple(Vec<VPattern>),
    PConstructor(String, Vec<VPattern>),
}

#[derive(Clone)]
pub enum VExpr {
    EInteger(i64),
    EDouble(f64),
    ECharacter(char),
    EId(String),
    ELambda(VPattern, Box<VExpr>),
    EApplication(Box<VExpr>, Box<VExpr>),
    ETuple(Vec<VExpr>),
    ETypeHole,
    EMatch(Box<VExpr>, Vec<(VPattern, VExpr)>),
    ELet(Vec<(String, VExpr)>, Box<VExpr>),
}

#[derive(Clone)]
pub struct Environment {
    pub values: BTreeMap<String, Value>,
    pub cons: BTreeSet<String>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: BTreeMap::new(),
            cons: BTreeSet::new(),
        }
    }

    pub fn with_bindings<T, F>(&mut self, new_vals: BTreeMap<String, Value>, action: F) -> T
    where
        F: FnOnce(&mut Environment) -> T,
    {
        let mut dups: BTreeMap<String, Value> = BTreeMap::new();
        for (name, val) in new_vals.iter() {
            self.values
                .insert(name.to_string(), val.clone())
                .map(|v| dups.insert(name.to_string(), v));
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
pub enum Value {
    VInteger(i64),
    VDouble(f64),
    VCharacter(char),
    VLambda(VPattern, VExpr, Environment),
    VTuple(Vec<Value>),
    VPrim(Rc<dyn Fn(Value, &mut Environment) -> Result<Value, RuntimeError>>),
    VConstructor(String, Vec<Value>),
    VUnevaluated(VExpr),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::VInteger(i) => write!(f, "{}", i),
            Value::VDouble(d) => write!(f, "{}", d),
            Value::VCharacter(c) => write!(f, "{}", c),
            Value::VTuple(vs) => {
                let mut comma_separated = String::new();
                for value in vs.iter().take(vs.len() - 1) {
                    comma_separated += format!("{}, ", value).as_str();
                }
                if let Some(v) = vs.iter().last() {
                    comma_separated += format!("{}", v).as_str();
                }

                write!(f, "({})", comma_separated)
            }
            Value::VConstructor(name, vals) => {
                let mut pprint = name.to_string();
                vals.iter()
                    .map(|v| match v {
                        Value::VConstructor(_, vals) if !vals.is_empty() => format!("({})", v),
                        _ => format!("{}", v),
                    })
                    .for_each(|pretty_val| {
                        pprint += format!(" {}", pretty_val).as_str();
                    });

                write!(f, "{}", pprint)
            }
            _ => Err(Error),
        }
    }
}

#[derive(Clone)]
pub enum RuntimeError {
    UnboundName(String),
    NonExhaustivePatternsInMatch,
    NonExhaustivePatternsInLambda,
    IncorrectFunction,
    InvalidTypeHole,
    IncorrectArguments,
}

impl Display for RuntimeError {
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
            IncorrectArguments => {
                write!(f, "Incorrectly typed arguments given to primitive function")
            }
        }
    }
}
