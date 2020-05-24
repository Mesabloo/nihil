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
        VExpr::EId(ref name) => match env.values.get(name) {
            None => env
                .cons
                .get(name)
                .ok_or_else(|| RuntimeError::UnboundName(name.clone()))
                .map(|_| Value::VConstructor(name.clone(), vec![])),
            Some(Value::VUnevaluated(ex)) => evaluate_inner(ex.clone(), env),
            Some(e) => Ok(e.clone()),
        },
        VExpr::EInteger(i) => Ok(Value::VInteger(i)),
        VExpr::EDouble(d) => Ok(Value::VDouble(d)),
        VExpr::ECharacter(c) => Ok(Value::VCharacter(c)),
        VExpr::ELambda(arg, box body) => Ok(Value::VLambda(arg, body, env.clone())),
        VExpr::EApplication(box fun, box arg) => {
            let arg = evaluate_inner(arg, env)?;
            let fun = evaluate_inner(fun, env)?;

            match fun {
                Value::VPrim(f) => f(arg, env),
                Value::VConstructor(name, mut es) => {
                    es.push(arg);
                    Ok(Value::VConstructor(name, es))
                }
                Value::VLambda(pat, ex, ctx) => env
                    .with_bindings(ctx.values, move |e| {
                        evaluate_case(&arg, pat.clone(), ex.clone(), e)
                    })
                    .map_err(|_| RuntimeError::NonExhaustivePatternsInLambda),

                _ => Err(RuntimeError::IncorrectFunction),
            }
        }
        VExpr::ETuple(exprs) => {
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

            branches
                .into_iter()
                .filter_map(|(pattern, branch)| evaluate_case(&expr, pattern, branch, env).ok())
                .next()
                .ok_or(RuntimeError::NonExhaustivePatternsInMatch)
        }
        VExpr::ELet(decls, box expr) => {
            let mut new_decls: BTreeMap<String, Value> = BTreeMap::new();
            decls.into_iter().for_each(|(name, bind)| {
                new_decls.insert(name, Value::VUnevaluated(bind));
            });

            env.with_bindings(new_decls, move |e| evaluate_inner(expr.clone(), e))
        }
        VExpr::ETypeHole => Err(RuntimeError::InvalidTypeHole),
    }
}

fn evaluate_case(
    expr: &Value,
    pat: VPattern,
    branch: VExpr,
    env: &mut Environment,
) -> Result<Value, RuntimeError> {
    unpack_pattern(expr, &pat)
        .ok_or(RuntimeError::NonExhaustivePatternsInMatch)
        .and_then(move |new_env| {
            env.with_bindings(new_env, move |e| evaluate_inner(branch.clone(), e))
        })
}

fn unpack_pattern(expr: &Value, pat: &VPattern) -> Option<BTreeMap<String, Value>> {
    let mut env: BTreeMap<String, Value> = BTreeMap::new();

    match (expr, pat) {
        (_, VPattern::PWildcard) => Some(env),
        (Value::VInteger(i), VPattern::PInteger(j)) if i == j => Some(env),
        (Value::VDouble(d), VPattern::PDouble(e)) if d == e => Some(env),
        (v, VPattern::PId(name)) => {
            env.insert(name.to_string(), v.clone());
            Some(env)
        }
        (Value::VConstructor(name, args), VPattern::PConstructor(namf, argt)) if name == namf => {
            args.into_iter()
                .zip(argt.into_iter())
                .try_fold(env, |mut new_env, (v, p)| {
                    new_env.append(&mut unpack_pattern(&v, p)?);
                    Some(new_env)
                })
        }
        (Value::VTuple(vals), VPattern::PTuple(pats)) => vals
            .into_iter()
            .zip(pats.into_iter())
            .try_fold(env, |mut new_env, (v, p)| {
                new_env.append(&mut unpack_pattern(&v, p)?);
                Some(new_env)
            }),
        (Value::VUnevaluated(_), _) => unreachable!(),
        //                             ^^^^^^^^^^^^^^
        //     This should never happen as our `evaluate_innter` function takes
        //     care of evaluating values.
        _ => None,
    }
}
