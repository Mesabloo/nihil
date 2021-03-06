#![allow(non_upper_case_globals)]

use crate::builtins;
use crate::core::*;
use crate::unsafe_layer::{coerce_bindings, coerce_to_vexpr, Binding_s, VExpr_s};

#[no_mangle]
pub extern "C" fn evaluate(
    ex: *const VExpr_s,
    nb_defs: u64,
    nb_cons: u64,
    defs: *const *const Binding_s,
    cons: *const *const i8,
) {
    // The process here is really simple.
    //
    // We are receiving an expression, and a complete environment.
    // - We firstly create a new Rust-land environment, to which we add the default bindings. (all the native functions)
    // - We then unmarshal the bindings unsafely, and put them in the environment as well.
    //
    // And finally we simply evaluate the expression received with the environment we created.

    let mut default_env = Environment::new();
    default_env.values.append(&mut builtins::default_bindings());
    default_env
        .cons
        .append(&mut builtins::default_constructors());

    let (mut defs, mut cons) = coerce_bindings(nb_defs as usize, defs, nb_cons as usize, cons);
    default_env.values.append(&mut defs);
    default_env.cons.append(&mut cons);

    let _ = evaluate_inner(coerce_to_vexpr(ex), &mut default_env)
        .map_err(|err| println!("[!] {}", err))
        .map(|res| println!("==> {}", res));
}

fn evaluate_inner(ex: VExpr, env: &mut Environment) -> Result<Value, RuntimeError> {
    match ex {
        VExpr::EId(name) => {
            // If our identifier does not refer to a variable in the scoped,
            // then it must be a constructor.
            // But if the identifier doesn't either refer to a constructor,
            // then something's wrong: our typechecking probably messed up.

            match env.values.get(&name).as_deref() {
                None => env
                    .cons
                    .get(&name)
                    .ok_or_else(move || RuntimeError::UnboundName(name))
                    .map(|name| Value::VConstructor(name.to_string(), vec![])),
                Some(Value::VUnevaluated(ex)) => evaluate_inner(ex.clone(), env),
                Some(e) => Ok(e.clone()),
            }
        }
        VExpr::EInteger(i) => Ok(Value::VInteger(i)),
        VExpr::EDouble(d) => Ok(Value::VDouble(d)),
        VExpr::ECharacter(c) => Ok(Value::VCharacter(c)),
        VExpr::ELambda(arg, box body) => Ok(Value::VLambda(arg, body, env.values.clone())),
        VExpr::EApplication(box fun, box arg) => {
            let arg = evaluate_inner(arg, env)?;
            let fun = evaluate_inner(fun, env)?;

            // Our evaluated function MUST BE a function (either a primitive, or a constructor, or a lambda).
            match fun {
                Value::VPrim(f) => {
                    let arg = match arg {
                        Value::VUnevaluated(e) => evaluate_inner(e, env)?,
                        v => v,
                    };
                    f(arg, env)
                }
                Value::VConstructor(name, mut es) => {
                    es.push(arg);
                    Ok(Value::VConstructor(name, es))
                }
                Value::VLambda(pat, ex, ctx) => env
                    .with_bindings(ctx, move |e| evaluate_case(&arg, &pat, ex, e))
                    .map_err(|_| RuntimeError::NonExhaustivePatternsInLambda),

                _ => Err(RuntimeError::IncorrectFunction),
            }
        }
        VExpr::ETuple(exprs) => {
            // Simply enough: evaluate all the expressions, accumulate the results.
            // Fails whenever any evaluation fails.

            let values = exprs.into_iter().try_fold(vec![], |mut acc, e| {
                evaluate_inner(e, env).map(|v| {
                    acc.push(v);
                    acc
                })
            })?;
            Ok(Value::VTuple(values))
        }
        VExpr::EMatch(box expr, branches) => {
            let expr = evaluate_inner(expr, env)?;

            // We try each branch, from the first to the last.
            // Stops whenever a pattern matches, and returns the corresponding value.
            //
            // Fails if no pattern has been matched.

            branches
                .into_iter()
                .filter_map(|(pattern, branch)| evaluate_case(&expr, &pattern, branch, env).ok())
                .next()
                .ok_or(RuntimeError::NonExhaustivePatternsInMatch)
        }
        VExpr::ELet(decls, box expr) => {
            // We create a shadowing environment, which will be used to introduce our let-bindings to
            // the expression.

            let mut new_decls: BTreeMap<String, Value> = BTreeMap::new();
            decls.into_iter().for_each(|(name, bind)| {
                new_decls.insert(name, Value::VUnevaluated(bind));
            });

            env.with_bindings(new_decls, move |e| evaluate_inner(expr, e))
        }
        VExpr::ETypeHole => Err(RuntimeError::InvalidTypeHole),
        VExpr::ERecord(decls) => Ok(Value::VRecord(
            decls
                .into_iter()
                .map(|(name, ex)| (name, Value::VUnevaluated(ex)))
                .collect(),
        )),
        VExpr::ERecordAccess(box record, field) => match evaluate_inner(record, env)? {
            Value::VRecord(rec) => rec
                .get(&field)
                .ok_or_else(|| RuntimeError::NoSuchField(field))
                .map(|v| v.clone()),
            _ => Err(RuntimeError::IncorrectRecord),
        },
    }
}

fn evaluate_case(
    expr: &Value,
    pat: &VPattern,
    branch: VExpr,
    env: &mut Environment,
) -> Result<Value, RuntimeError> {
    unpack_pattern(expr, pat)
        .ok_or(RuntimeError::NonExhaustivePatternsInMatch)
        .and_then(move |new_env| env.with_bindings(new_env, move |e| evaluate_inner(branch, e)))
}

fn unpack_pattern(expr: &Value, pat: &VPattern) -> Option<BTreeMap<String, Value>> {
    let mut env: BTreeMap<String, Value> = BTreeMap::new();

    match (expr, pat) {
        (_, VPattern::PWildcard) => Some(env),
        (Value::VInteger(i), VPattern::PInteger(j)) if i == j => Some(env),
        (Value::VDouble(d), VPattern::PDouble(e)) if (d - e).abs() < 0.000000001 => Some(env),
        (v, VPattern::PId(name)) => {
            env.insert(name.to_string(), v.clone());
            Some(env)
        }
        (Value::VConstructor(name, args), VPattern::PConstructor(namf, argt)) if name == namf => {
            args.iter()
                .zip(argt.iter())
                .try_fold(env, |mut new_env, (v, p)| {
                    new_env.append(&mut unpack_pattern(&v, p)?);
                    Some(new_env)
                })
        }
        (Value::VTuple(vals), VPattern::PTuple(pats)) => {
            vals.iter()
                .zip(pats.iter())
                .try_fold(env, |mut new_env, (v, p)| {
                    new_env.append(&mut unpack_pattern(&v, p)?);
                    Some(new_env)
                })
        }
        (Value::VUnevaluated(_), _) => unreachable!(),
        //                             ^^^^^^^^^^^^^^
        //     This should never happen as our `evaluate_innter` function takes
        //     care of evaluating values.
        _ => None,
    }
}
