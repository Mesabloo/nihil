#![allow(non_upper_case_globals)]

use crate::core::*;

use std::ffi::{CStr};

#[no_mangle]
pub extern "C" fn evaluate(ex: *const VExpr) -> () {
    let mut default_env = Environment::new();

    let result = evaluate_inner(unsafe { *ex }, &mut default_env);
    println!("{}", result);
}

fn evaluate_inner<'a>(ex: VExpr, env: &mut Environment<'a>) -> Value<'a> {
    match ex.ctor {
        EId => {
            let name = unsafe { CStr::from_ptr(ex.value.eId.v_name) }
                .to_str()
                .expect("Cannot parse UTF8 string");
            match env.values.get(name) {
                None => {
                    if let None = env.cons.get(name) {
                        panic!(format!("Name '{}' unbound!", name))
                    }
                    Value::VConstructor(name, vec!())
                },
                Some(Value::VUnevaluated(ex)) => evaluate_inner(*ex, env),
                Some(e)                       => e.clone()
            }
        },
        EInteger => Value::VInteger(unsafe { ex.value.eInteger.v_i }),
        EDouble => Value::VDouble(unsafe { ex.value.eDouble.v_d }),
        ECharacter => Value::VCharacter(unsafe { ex.value.eCharacter.v_c } as u8 as char),
                                    //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                    // This seems somewhat disgusting.
        ELambda => Value::VLambda(unsafe { *ex.value.eLambda.v_arg }, unsafe { *ex.value.eLambda.v_ex }, env.clone()),
        EApplication => {
            let fun = unsafe { *ex.value.eApplication.v_fun };
            let arg = evaluate_inner(unsafe { *ex.value.eApplication.v_x }, env);

            let fun = evaluate_inner(fun, env);
            match fun {
                Value::VPrim(f) => f(arg, env),
                Value::VConstructor(name, mut es) => {
                    es.push(arg);
                    Value::VConstructor(name, es)
                },
                Value::VLambda(pat, ex, ctx) => env.with_bindings(&ctx.values, |e|
                    evaluate_case(&arg, pat, ex, e).expect("Non exhaustive patterns in lambda.")),

                _ => panic!("First argument of applications must be a function."),
            }
        },
        ETuple => {
            let mut values: Vec<Value> = vec![];
            let vec_size = unsafe { ex.value.eTuple.n } as usize;
            values.reserve_exact(vec_size);
            for idx in 0..vec_size {
                values.insert(idx, Value::VUnevaluated(unsafe { **ex.value.eTuple.v_vals.add(idx) }));
            }
            Value::VTuple(values)
        },
        EMatch => {
            let expr = evaluate_inner(unsafe { *ex.value.eMatch.v_expr }, env);

            let mut branches: Vec<(VPattern, VExpr)> = vec![];
            let branches_size = unsafe { ex.value.eMatch.n } as usize;
            branches.reserve_exact(branches_size);
            for idx in 0..branches_size {
                let branch = unsafe {
                    let branch = **ex.value.eMatch.v_branches.add(idx);
                    (*branch.b_pattern, *branch.b_expr)
                };
                branches.insert(idx, branch);
            }

            let mut result: Option<Value<'a>> = None;
            let mut branch_iterator = branches.into_iter();
            while result.is_none() {
                match branch_iterator.next() {
                    Some((pattern, branch)) => {
                        result = evaluate_case(&expr, pattern, branch, env);
                    },
                    None => break,
                }
            }

            result.expect("Unexhaustive patterns in pattern matching")
        },
        ELet => {
            let expr = unsafe { *ex.value.eLet.v_expr };
            let mut decls: BTreeMap<&'a str, Value<'a>> = BTreeMap::new();
            let decls_number = unsafe { ex.value.eLet.n } as usize;
            for idx in 0..decls_number {
                let (name, def) = unsafe {
                    let decl = **ex.value.eLet.v_decls.add(idx);
                    (CStr::from_ptr(decl.d_name)
                        .to_str()
                        .expect("Unable to decode UTF8 string"), *decl.d_val)
                };
                decls.insert(name, Value::VUnevaluated(def));
            }

            env.with_bindings(&decls, move |e| evaluate_inner(expr, e))
        },
        ETypeHole => panic!("Unhandled type hole."),

        _ => unreachable!(),
    }
}

fn evaluate_case<'a>(expr: &Value<'a>, pat: VPattern, branch: VExpr, env: &mut Environment<'a>) -> Option<Value<'a>> {
    unpack_pattern(expr, pat).map(move |new_env| env.with_bindings(&new_env, move |e| evaluate_inner(branch, e)))
}

fn unpack_pattern<'a>(expr: &Value<'a>, pat: VPattern) -> Option<BTreeMap<&'a str, Value<'a>>> {
    let mut env: BTreeMap<&'a str, Value<'a>> = BTreeMap::new();

    match (expr, pat.ctor) {
        (_, PWildcard) => Some(env),
        (Value::VInteger(i), PInteger)
            if *i == unsafe { pat.value.pInteger.p_i } => Some(env),
        (Value::VDouble(d), PDouble)
            if *d == unsafe { pat.value.pDouble.p_d } => Some(env),
        (v, PId) => {
            let name = unsafe { CStr::from_ptr(pat.value.pId.p_name) }
                .to_str()
                .expect("Cannot decode UTF8 string");
            env.insert(name, v.clone());
            Some(env)
        },
        (Value::VConstructor(name, args), PConstructor)
            if *name == unsafe { CStr::from_ptr(pat.value.pConstructor.p_name) }
                .to_str()
                .expect("Cannot decode UTF8 string") => {
            let mut p_args: Vec<VPattern> = vec![];
            let args_size = unsafe { pat.value.pConstructor.n } as usize;
            p_args.reserve_exact(args_size);
            for idx in 0..args_size {
                p_args.insert(idx, unsafe { **pat.value.pConstructor.p_args.add(idx) });
            }

            for (v, p) in args.into_iter().zip(p_args.into_iter()) {
                match unpack_pattern(v, p) {
                    Some(mut new_env) => env.append(&mut new_env),
                    None => return None
                }
            }

            Some(env)
        }
        (Value::VTuple(vals), PTuple) => {
            let mut p_args: Vec<VPattern> = vec![];
            let args_size = unsafe { pat.value.pTuple.n } as usize;
            p_args.reserve_exact(args_size);
            for idx in 0..args_size {
                p_args.insert(idx, unsafe { **pat.value.pTuple.p_patterns.add(idx) });
            }

            for (v, p) in vals.into_iter().zip(p_args.into_iter()) {
                match unpack_pattern(v, p) {
                    Some(mut new_env) => env.append(&mut new_env),
                    None => return None
                }
            }

            Some(env)
        },
        (Value::VUnevaluated(_), _) => unreachable!(),
        //                             ^^^^^^^^^^^^^^
        //     This should never happen as our `evaluate_innter` function takes
        //     care of evaluating values.
        _ => None,
    }
}