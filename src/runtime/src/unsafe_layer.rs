use std::ffi::CStr;

#[allow(non_upper_case_globals)]
#[allow(non_camel_case_types)]
#[allow(non_snake_case)]
// ^^^^^^^^^^^^^^^^^^^^^
// Because our naming conventions in C are not necessarily
// the same as in Rust.
// For our binding, it should be ok.
#[allow(dead_code)]
// ^^^^^^^^^^^^^^^^
// This one is needed because there are a few type aliases
// unused in the generated code.
mod core_bindings {
    include!(concat!(env!("OUT_DIR"), "/gnc-core.rs"));
}
pub use core_bindings::Binding_s;
pub use core_bindings::VExpr_s;
use core_bindings::*;

use crate::core::{BTreeMap, BTreeSet, VExpr, VPattern, Value};

#[allow(non_upper_case_globals)]
pub fn coerce_to_vexpr(ex: *const VExpr_s) -> VExpr {
    let ex = unsafe { *ex };

    match ex.ctor {
        VExpr_s_VExpr_Cons_CrEInteger => VExpr::EInteger(unsafe { ex.value.eInteger.v_i } as i64),
        VExpr_s_VExpr_Cons_CrEDouble => VExpr::EDouble(unsafe { ex.value.eDouble.v_d }),
        VExpr_s_VExpr_Cons_CrECharacter => {
            VExpr::ECharacter(unsafe { ex.value.eCharacter.v_c } as u8 as char)
        }
        VExpr_s_VExpr_Cons_CrEId => {
            let name = unsafe { CStr::from_ptr(ex.value.eId.v_name) }
                .to_str()
                .expect("Cannot decode UTF8 string")
                .to_owned();
            VExpr::EId(name)
        }
        VExpr_s_VExpr_Cons_CrELambda => {
            let arg = coerce_to_vpattern(unsafe { ex.value.eLambda.v_arg });
            let body = coerce_to_vexpr(unsafe { ex.value.eLambda.v_ex });
            VExpr::ELambda(arg, box body)
        }
        VExpr_s_VExpr_Cons_CrEApplication => {
            let fun = coerce_to_vexpr(unsafe { ex.value.eApplication.v_fun });
            let arg = coerce_to_vexpr(unsafe { ex.value.eApplication.v_x });
            VExpr::EApplication(box fun, box arg)
        }
        VExpr_s_VExpr_Cons_CrETuple => {
            let exprs = vec_from_ptr(
                unsafe { ex.value.eTuple.v_vals },
                unsafe { ex.value.eTuple.n } as usize,
            )
            .into_iter()
            .map(coerce_to_vexpr)
            .collect();
            VExpr::ETuple(exprs)
        }
        VExpr_s_VExpr_Cons_CrEMatch => {
            let expr = coerce_to_vexpr(unsafe { ex.value.eMatch.v_expr });
            let exprs =
                vec_from_ptr(
                    unsafe { ex.value.eMatch.v_branches },
                    unsafe { ex.value.eMatch.n } as usize,
                )
                .into_iter()
                .map(coerce_to_vbranch)
                .collect();
            VExpr::EMatch(box expr, exprs)
        }
        VExpr_s_VExpr_Cons_CrELet => {
            let decls = vec_from_ptr(unsafe { ex.value.eLet.v_decls }, unsafe { ex.value.eLet.n }
                as usize)
            .into_iter()
            .map(coerce_to_vdecl)
            .collect();
            let expr = coerce_to_vexpr(unsafe { ex.value.eLet.v_expr });
            VExpr::ELet(decls, box expr)
        }
        VExpr_s_VExpr_Cons_CrETypeHole => VExpr::ETypeHole,
        VExpr_s_VExpr_Cons_CrERecord => {
            let decls = vec_from_ptr(unsafe { ex.value.eLet.v_decls }, unsafe { ex.value.eLet.n }
                as usize)
            .into_iter()
            .map(coerce_to_vdecl)
            .collect();
            VExpr::ERecord(decls)
        }
        _ => unreachable!(),
    }
}

pub fn coerce_bindings(
    nb_defs: usize,
    defs: *const *const Binding_s,
    nb_cons: usize,
    cons: *const *const i8,
) -> (BTreeMap<String, Value>, BTreeSet<String>) {
    let mut new_defs = BTreeMap::new();
    let mut new_cons = BTreeSet::new();

    for idx in 0..nb_defs {
        let (name, bind) = unsafe {
            let ptr = **defs.add(idx);

            let name = CStr::from_ptr(ptr.b_name);
            let bind = coerce_to_vexpr(ptr.b_val);
            (name, Value::VUnevaluated(bind))
        };
        new_defs.insert(
            name.to_str().expect("Cannot decode UTF8 string").to_owned(),
            bind,
        );
    }
    for idx in 0..nb_cons {
        let name = unsafe { CStr::from_ptr(*cons.add(idx)) }
            .to_str()
            .expect("Cannot decode UTF8 string")
            .to_owned();
        new_cons.insert(name);
    }

    (new_defs, new_cons)
}

#[allow(non_upper_case_globals)]
fn coerce_to_vpattern(pat: *const VPattern_s) -> VPattern {
    let pat = unsafe { *pat };

    match pat.ctor {
        VPattern_s_VPattern_Cons_CrPInteger => {
            VPattern::PInteger(unsafe { pat.value.pInteger.p_i } as i64)
        }
        VPattern_s_VPattern_Cons_CrPDouble => VPattern::PDouble(unsafe { pat.value.pDouble.p_d }),
        VPattern_s_VPattern_Cons_CrPCharacter => {
            VPattern::PCharacter(unsafe { pat.value.pCharacter.p_c } as u8 as char)
        }
        VPattern_s_VPattern_Cons_CrPId => {
            let name = unsafe { CStr::from_ptr(pat.value.pId.p_name) }
                .to_str()
                .expect("Cannot decode UTF8 string")
                .to_owned();
            VPattern::PId(name)
        }
        VPattern_s_VPattern_Cons_CrPTuple => {
            let pats =
                vec_from_ptr(
                    unsafe { pat.value.pTuple.p_patterns },
                    unsafe { pat.value.pTuple.n } as usize,
                )
                .into_iter()
                .map(coerce_to_vpattern)
                .collect();
            VPattern::PTuple(pats)
        }
        VPattern_s_VPattern_Cons_CrPConstructor => {
            let name = unsafe { CStr::from_ptr(pat.value.pConstructor.p_name) }
                .to_str()
                .expect("Cannot decode UTF8 string")
                .to_owned();
            let args = vec_from_ptr(unsafe { pat.value.pConstructor.p_args }, unsafe {
                pat.value.pConstructor.n
            }
                as usize)
            .into_iter()
            .map(coerce_to_vpattern)
            .collect();
            VPattern::PConstructor(name, args)
        }
        VPattern_s_VPattern_Cons_CrPWildcard => VPattern::PWildcard,
        _ => unreachable!(),
    }
}

fn coerce_to_vbranch(ptr: *const VBranch_s) -> (VPattern, VExpr) {
    let (pat, ex) = unsafe { ((*ptr).b_pattern, (*ptr).b_expr) };
    (coerce_to_vpattern(pat), coerce_to_vexpr(ex))
}

fn coerce_to_vdecl(ptr: *const VDecl_s) -> (String, VExpr) {
    let (name, ex) = unsafe { (CStr::from_ptr((*ptr).d_name), (*ptr).d_val) };
    (
        name.to_str().expect("Cannot decode UTF8 string").to_owned(),
        coerce_to_vexpr(ex),
    )
}

fn vec_from_ptr<T>(ptr: *const T, size: usize) -> Vec<T>
where
    T: Copy,
{
    let mut vec = vec![];
    vec.reserve_exact(size);
    for idx in 0..size {
        vec.push(unsafe { *ptr.add(idx) });
    }
    vec

    // Please keep this pretty ugly code...
    // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    //
    // (0..size).map(|idx| unsafe { *ptr.add(idx) })
    //          .collect()
}
