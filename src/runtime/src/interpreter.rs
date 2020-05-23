#![allow(non_upper_case_globals)]

use crate::core::*;
use crate::unsafe_layer::{VExpr_s, coerce_to_vexpr};

#[no_mangle]
pub extern "C" fn evaluate(ex: *const VExpr_s) -> () {
    let mut default_env = Environment::new();

    let _ = evaluate_inner(coerce_to_vexpr(ex), &mut default_env)
        .map_err(|err| println!("{}", err));
}

fn evaluate_inner<'a>(ex: VExpr<'a>, env: &mut Environment<'a>) -> Result<Value<'a>, RuntimeError<'a>> {
    match ex {
        VExpr::EId(name) => {
            match env.values.get(name).as_deref() {
                None => {
                    env.cons.get(name).ok_or_else(|| RuntimeError::UnboundName(name))?;
                    Ok(Value::VConstructor(name, vec![]))
                },
                Some(Value::VUnevaluated(ex)) => evaluate_inner(ex.clone(), env),
                Some(e)                       => Ok(e.clone())
            }
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
                },
                Value::VLambda(pat, ex, ctx) => env.with_bindings(&ctx.values, move |e|
                    evaluate_case(&arg, pat.clone(), ex.clone(), e)).map_err(|_| RuntimeError::NonExhaustivePatternsInLambda),

                _ => Err(RuntimeError::IncorrectFunction),
            }
        },
        VExpr::ETuple(exprs) => {
            let values = exprs.into_iter().try_fold(vec![], |mut acc, e| evaluate_inner(e, env).map(|v| { acc.push(v); acc }))?;
            Ok(Value::VTuple(values))
        },
        VExpr::EMatch(box expr, branches) => {
            let expr = evaluate_inner(expr, env)?;

            branches.into_iter()
                .filter_map(|(pattern, branch)| evaluate_case(&expr, pattern, branch, env).ok())
                .next()
                .ok_or(RuntimeError::NonExhaustivePatternsInMatch)
        },
        VExpr::ELet(decls, box expr) => {
            let mut new_decls: BTreeMap<&'a str, Value<'a>> = BTreeMap::new();
            decls.into_iter()
                 .for_each(|(name, bind)| { new_decls.insert(name, Value::VUnevaluated(bind)); });

            env.with_bindings(&new_decls, move |e| evaluate_inner(expr.clone(), e))
        },
        VExpr::ETypeHole => Err(RuntimeError::InvalidTypeHole),
    }
}

fn evaluate_case<'a>(expr: &Value<'a>, pat: VPattern<'a>, branch: VExpr<'a>, env: &mut Environment<'a>) -> Result<Value<'a>, RuntimeError<'a>> {
    unpack_pattern(expr, pat).ok_or(RuntimeError::NonExhaustivePatternsInMatch)
                             .and_then(move |new_env| env.with_bindings(&new_env, move |e| evaluate_inner(branch.clone(), e)))
}

fn unpack_pattern<'a>(expr: &Value<'a>, pat: VPattern<'a>) -> Option<BTreeMap<&'a str, Value<'a>>> {
    let mut env: BTreeMap<&'a str, Value<'a>> = BTreeMap::new();

    match (expr.clone(), pat) {
        (_, VPattern::PWildcard) => Some(env),
        (Value::VInteger(i), VPattern::PInteger(j)) if i == j => Some(env),
        (Value::VDouble(d), VPattern::PDouble(e)) if d == e => Some(env),
        (v, VPattern::PId(name)) => {
            env.insert(name, v.clone());
            Some(env)
        },
        (Value::VConstructor(name, args), VPattern::PConstructor(namf, argt)) if name == namf => {
            args.into_iter()
                .zip(argt.into_iter())
                .try_fold(env, |mut new_env, (v, p)| { new_env.append(&mut unpack_pattern(&v, p)?); Some(new_env) })
        }
        (Value::VTuple(vals), VPattern::PTuple(pats)) => {
            vals.into_iter()
                .zip(pats.into_iter())
                .try_fold(env, |mut new_env, (v, p)| { new_env.append(&mut unpack_pattern(&v, p)?); Some(new_env) })
        },
        (Value::VUnevaluated(_), _) => unreachable!(),
        //                             ^^^^^^^^^^^^^^
        //     This should never happen as our `evaluate_innter` function takes
        //     care of evaluating values.
        _ => None,
    }
}