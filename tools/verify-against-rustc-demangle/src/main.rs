#![warn(
    explicit_outlives_requirements,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    // missing_docs,
    noop_method_call,
    pointer_structural_match,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    variant_size_differences,
    // clippy::cargo_common_metadata,
    clippy::clone_on_ref_ptr,
    clippy::cognitive_complexity,
    clippy::create_dir,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::empty_line_after_outer_attr,
    clippy::fallible_impl_from,
    clippy::filetype_is_file,
    clippy::float_cmp_const,
    clippy::get_unwrap,
    clippy::if_then_some_else_none,
    clippy::imprecise_flops,
    clippy::let_underscore_must_use,
    clippy::lossy_float_literal,
    clippy::multiple_inherent_impl,
    clippy::mutex_integer,
    clippy::nonstandard_macro_braces,
    clippy::panic_in_result_fn,
    clippy::path_buf_push_overwrite,
    clippy::pedantic,
    clippy::print_stderr,
    // clippy::print_stdout,
    clippy::rc_buffer,
    clippy::rc_mutex,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::suspicious_operation_groupings,
    clippy::todo,
    clippy::trivial_regex,
    clippy::unimplemented,
    clippy::unnecessary_self_imports,
    clippy::unneeded_field_pattern,
    // clippy::use_debug,
    clippy::use_self,
    clippy::useless_let_if_seq,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    // clippy::wildcard_dependencies,
)]
#![allow(clippy::non_ascii_literal)]

use ast_demangle::rust_v0::Symbol;
use std::env;
use std::fmt::Write;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[allow(clippy::if_then_some_else_none)] // See <https://github.com/rust-lang/rust-clippy/issues/7870>.
fn demangle_ast_demangle<'a>(name: &str, buffer: &'a mut String) -> Option<(&'a str, &'a str)> {
    let (symbol, rest) = Symbol::parse_from_str(name).ok()?;

    if rest.is_empty() || rest.starts_with('.') {
        let suffix = rest.find(".llvm.").map_or(rest, |i| &rest[..i]);

        write!(buffer, "{}{}", symbol, suffix).ok()?;

        let split = buffer.len();

        write!(buffer, "{:#}{}", symbol, suffix).ok()?;

        Some(buffer.split_at(split))
    } else {
        None
    }
}

fn demangle_rustc_demangle<'a>(name: &str, buffer: &'a mut String) -> Option<(&'a str, &'a str)> {
    let demangle = rustc_demangle::try_demangle(name).ok()?;

    write!(buffer, "{}", demangle).ok()?;

    let split = buffer.len();

    write!(buffer, "{:#}", demangle).ok()?;

    Some(buffer.split_at(split))
}

fn main() -> Result<(), &'static str> {
    let test_data_path = env::args_os().nth(1).unwrap();
    let mut reader = BufReader::new(File::open(test_data_path).unwrap());
    let mut line_buffer = String::new();
    let mut ast_buffer = String::new();
    let mut rustc_buffer = String::new();
    let mut failed = false;

    while let Ok(length) = reader.read_line(&mut line_buffer) {
        if length == 0 {
            break;
        }

        let line = line_buffer.trim_end();

        if line.starts_with("_R") {
            let ast_result = demangle_ast_demangle(line, &mut ast_buffer);
            let rustc_result = demangle_rustc_demangle(line, &mut rustc_buffer);

            if ast_result != rustc_result {
                failed = true;

                println!("{}", line);
                println!("    Actual = {:?}", ast_result);
                println!("    Expected = {:?}", rustc_result);
            }

            rustc_buffer.clear();
            ast_buffer.clear();
        } else {
            break;
        }

        line_buffer.clear();
    }

    if failed {
        Err("Some test cases failed.")
    } else {
        Ok(())
    }
}
