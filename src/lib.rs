//! # ast-demangle
//!
//! Parses mangled names and produces structured results.
//!
//! Example:
//!
//! ```rust
//! use ast_demangle::rust_v0::{DisplayStyle, Identifier, Path, Symbol, UndisambiguatedIdentifier};
//! use std::borrow::Cow;
//!
//! let mangled_name = "_RNvNtCs6GSVXm7oiwY_5regex4utf811decode_utf8.llvm.1119170478327948870";
//! let (symbol, suffix) = Symbol::parse_from_str(mangled_name).unwrap();
//!
//! // The suffix is returned.
//! assert_eq!(suffix, ".llvm.1119170478327948870");
//!
//! // The default style for displaying is the long format.
//! assert_eq!(format!("{}", symbol), "regex[4df147058689a776]::utf8::decode_utf8");
//!
//! // To omit the crate hash, use the alternate display format.
//! assert_eq!(format!("{:#}", symbol), "regex::utf8::decode_utf8");
//!
//! // Use `Symbol::display` and `DisplayStyle` to specify the display style explicitly.
//!
//! assert_eq!(format!("{}", symbol.display(DisplayStyle::Short)), "decode_utf8");
//! assert_eq!(format!("{}", symbol.display(DisplayStyle::Normal)), "regex::utf8::decode_utf8");
//!
//! assert_eq!(
//!     format!("{}", symbol.display(DisplayStyle::Long)),
//!     "regex[4df147058689a776]::utf8::decode_utf8"
//! );
//!
//! // You can access the structure of the demangled symbol.
//!
//! assert_eq!(
//!     symbol,
//!     Symbol {
//!         version: None,
//!         path: Path::Nested {
//!             namespace: b'v',
//!             path: Path::Nested {
//!                 namespace: b't',
//!                 path: Path::CrateRoot(Identifier {
//!                     disambiguator: 0x4df1_4705_8689_a776,
//!                     name: UndisambiguatedIdentifier(Cow::Borrowed("regex"))
//!                 })
//!                 .into(),
//!                 name: Identifier {
//!                     disambiguator: 0,
//!                     name: UndisambiguatedIdentifier(Cow::Borrowed("utf8"))
//!                 }
//!             }
//!             .into(),
//!             name: Identifier {
//!                 disambiguator: 0,
//!                 name: UndisambiguatedIdentifier(Cow::Borrowed("decode_utf8"))
//!             }
//!         }
//!         .into(),
//!         instantiating_crate: None
//!     }
//! );
//! ```

#![warn(
    explicit_outlives_requirements,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    // missing_docs,
    noop_method_call,
    pointer_structural_match,
    // single_use_lifetimes, // See <https://github.com/rust-lang/rust/issues/69952>.
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    // unused_crate_dependencies, // False positive.
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
    clippy::print_stdout,
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
    clippy::use_debug,
    clippy::use_self,
    clippy::useless_let_if_seq,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    // clippy::wildcard_dependencies,
)]
#![allow(clippy::module_name_repetitions, clippy::non_ascii_literal)]

mod mini_parser;
pub mod rust_v0;
