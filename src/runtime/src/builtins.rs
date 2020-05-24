use crate::core::*;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

pub fn default_bindings() -> BTreeMap<String, Value> {
    let mut builtins = BTreeMap::new();
    builtins.insert(String::from("+"), Value::VPrim(Rc::new(add)));

    builtins
}

pub fn default_constructors() -> BTreeSet<String> {
    vec!["()", "Nil", "Cons"]
        .into_iter()
        .map(String::from)
        .collect()
}

//////////////////////////////////////////////////////////////////////////

fn add(v1: Value, _env: &mut Environment) -> Result<Value, RuntimeError> {
    use Value::*;

    let add_inner = move |v2: Value, _env: &mut Environment| match (&v1, v2) {
        (VInteger(x), VInteger(y)) => Ok(VInteger(x + y)),
        (VDouble(x), VDouble(y)) => Ok(VDouble(x + y)),
        (VDouble(x), VInteger(y)) => Ok(VDouble(x + (y as f64))),
        (VInteger(x), VDouble(y)) => Ok(VDouble((*x as f64) + y)),
        _ => Err(RuntimeError::IncorrectArguments),
    };

    Ok(VPrim(Rc::new(add_inner)))
}
