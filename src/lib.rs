//! # ast-demangle
//!
//! Parses mangled names and produces structured results.
//!
//! Example:
//!
//! ```rust
//! use ast_demangle::rust_v0::display::Style;
//! use ast_demangle::rust_v0::{Identifier, Path, Symbol};
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
//! // Use `Symbol::display` and `Style` to specify the display style explicitly.
//!
//! assert_eq!(format!("{}", symbol.display(Style::Short)), "decode_utf8");
//! assert_eq!(format!("{}", symbol.display(Style::Normal)), "regex::utf8::decode_utf8");
//!
//! assert_eq!(
//!     format!("{}", symbol.display(Style::Long)),
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
//!                     name: "regex".into()
//!                 })
//!                 .into(),
//!                 name: Identifier {
//!                     disambiguator: 0,
//!                     name: "utf8".into()
//!                 }
//!             }
//!             .into(),
//!             name: Identifier {
//!                 disambiguator: 0,
//!                 name: "decode_utf8".into()
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
    noop_method_call,
    pointer_structural_match,
    semicolon_in_expressions_from_macros,
    trivial_casts,
    trivial_numeric_casts,
    unaligned_references,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    variant_size_differences,
    clippy::clone_on_ref_ptr,
    clippy::cognitive_complexity,
    clippy::debug_assert_with_mut_call,
    clippy::empty_line_after_outer_attr,
    clippy::fallible_impl_from,
    clippy::get_unwrap,
    clippy::imprecise_flops,
    clippy::let_underscore_must_use,
    clippy::lossy_float_literal,
    clippy::multiple_inherent_impl,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::panic,
    clippy::panic_in_result_fn,
    clippy::path_buf_push_overwrite,
    clippy::pedantic,
    clippy::rc_buffer,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::semicolon_if_nothing_returned,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::todo,
    clippy::trivial_regex,
    clippy::unimplemented,
    clippy::unneeded_field_pattern,
    clippy::use_debug,
    clippy::use_self,
    clippy::useless_let_if_seq,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::wrong_pub_self_convention
)]
#![allow(clippy::non_ascii_literal)]

pub mod rust_v0;
