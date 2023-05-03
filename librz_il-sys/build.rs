use std::env;
use std::path::{Path, PathBuf};

/// Generates Rust FFI from a wrapping `.h` file
fn gen_bindings(
    // *.h
    wrapper: impl AsRef<Path>,
    // *.rs
    dst: impl AsRef<Path>,
    clang_args: impl IntoIterator<Item = impl AsRef<str>>,
    docstring: &str,
    mut setup_builder: impl FnMut(bindgen::Builder) -> bindgen::Builder,
) {
    let gen = bindgen::Builder::default()
        .header(format!("{}", wrapper.as_ref().display()))
        .clang_args(clang_args)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks));

    let gen = setup_builder(gen);

    // Add docstring to output file
    let gen = gen
        .raw_line(docstring)
        .raw_line("")
        .raw_line(r"#![allow(warnings)]");

    // Generate FFi
    let gen = gen.generate().unwrap_or_else(|err| {
        panic!(
            "Unable to generate bindings for `{}`. Original error {:?}",
            dst.as_ref().display(),
            err
        )
    });

    // Output FFi. Allow to fail because crates.io has no write permission for 'src' dir
    gen.write_to_file(dst).ok();
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rustc-link-lib=rz_il");

    let library = pkg_config::Config::new()
        .probe("rz_il")
        .expect("pkg-config: rz_il not found");
    let root = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let args = library
        .include_paths
        .iter()
        .map(|path| format!("-I{}", path.to_string_lossy()));

    gen_bindings(
        root.join("src/wrapper.h"),
        root.join("src/bindings.rs"),
        args,
        "//! Rust FFI to 'rz_il.h'",
        |b| {
            b.derive_default(true)
                .derive_eq(true)
                .derive_partialeq(true)
                .default_enum_style(bindgen::EnumVariation::Rust {
                    non_exhaustive: false,
                })
        },
    );
}
