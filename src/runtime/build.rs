extern crate bindgen;

use std::env;
use std::path::{Path, PathBuf};

fn main() {
    let gnc_core_dir = "../core";
    //                 ^^^^^^^^^
    //  Modify this if you ever change the path of the runtime Haskell core.

    let path = Path::new(env!("PWD"))
        .join(gnc_core_dir)
        .join("Nihil/Runtime/core.h");
    let path = path.to_str().unwrap();

    println!("cargo:rerun-if-changed={}", path);

    let bindings = bindgen::Builder::default()
        .header(path)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings for GNC's core");

    let out = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out.join("gnc-core.rs"))
        .expect("Unable to write bidings to file");
}
