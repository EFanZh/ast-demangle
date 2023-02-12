//! # ast-demangle
//!
//! Parses mangled names and produces structured results.
//!
//! Example:
//!
//! ```rust
//! use ast_demangle::rust_v0::{DisplayStyle, Identifier, Path, Symbol};
//! use std::borrow::Cow;
//!
//! let mangled_name = "_RNvNtCs6GSVXm7oiwY_5regex4utf811decode_utf8.llvm.1119170478327948870";
//! let (symbol, suffix) = Symbol::parse_from_str(mangled_name).unwrap();
//!
//! // The suffix is returned.
//! assert_eq!(suffix, "");
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
//!                     name: Cow::Borrowed("regex"),
//!                     disambiguator: 0x4df1_4705_8689_a776,
//!                 })
//!                 .into(),
//!                 identifier: Identifier {
//!                     name: Cow::Borrowed("utf8"),
//!                     disambiguator: 0,
//!                 }
//!             }
//!             .into(),
//!             identifier: Identifier {
//!                 name: Cow::Borrowed("decode_utf8"),
//!                 disambiguator: 0,
//!             }
//!         }
//!         .into(),
//!         instantiating_crate: None,
//!         vendor_specific_suffix: Some(".llvm.1119170478327948870"),
//!     }
//! );
//! ```

#![warn(
    absolute_paths_not_starting_with_crate,
    explicit_outlives_requirements,
    let_underscore_drop,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    missing_docs,
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
    unused_macro_rules,
    unused_qualifications,
    unused_tuple_struct_fields,
    variant_size_differences,
    clippy::alloc_instead_of_core,
    clippy::allow_attributes_without_reason,
    clippy::as_ptr_cast_mut,
    clippy::branches_sharing_code,
    clippy::cargo_common_metadata,
    clippy::clone_on_ref_ptr,
    clippy::cognitive_complexity,
    clippy::create_dir,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::decimal_literal_representation,
    clippy::deref_by_slicing,
    clippy::derive_partial_eq_without_eq,
    clippy::empty_drop,
    clippy::empty_line_after_outer_attr,
    clippy::empty_structs_with_brackets,
    clippy::equatable_if_let,
    clippy::fallible_impl_from,
    clippy::filetype_is_file,
    clippy::float_cmp_const,
    clippy::format_push_string,
    clippy::get_unwrap,
    clippy::if_then_some_else_none,
    clippy::imprecise_flops,
    clippy::iter_on_empty_collections,
    clippy::iter_on_single_items,
    clippy::iter_with_drain,
    clippy::large_include_file,
    clippy::let_underscore_must_use,
    clippy::lossy_float_literal,
    clippy::manual_clamp,
    clippy::map_err_ignore,
    clippy::mixed_read_write_in_expression,
    clippy::multiple_inherent_impl,
    clippy::mutex_atomic,
    clippy::mutex_integer,
    clippy::needless_collect,
    clippy::negative_feature_names,
    clippy::non_send_fields_in_send_ty,
    clippy::nonstandard_macro_braces,
    clippy::option_if_let_else,
    clippy::or_fun_call,
    clippy::panic,
    clippy::panic_in_result_fn,
    clippy::partial_pub_fields,
    clippy::path_buf_push_overwrite,
    clippy::pedantic,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::rc_buffer,
    clippy::rc_mutex,
    clippy::redundant_feature_names,
    clippy::redundant_pub_crate,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::self_named_module_files,
    clippy::significant_drop_in_scrutinee,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::suspicious_operation_groupings,
    clippy::todo,
    clippy::trailing_empty_array,
    clippy::trait_duplication_in_bounds,
    clippy::transmute_undefined_repr,
    clippy::trivial_regex,
    clippy::try_err,
    clippy::type_repetition_in_bounds,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unnecessary_safety_comment,
    clippy::unnecessary_safety_doc,
    clippy::unnecessary_self_imports,
    clippy::unneeded_field_pattern,
    clippy::unused_peekable,
    clippy::unused_rounding,
    clippy::use_debug,
    clippy::use_self,
    clippy::useless_let_if_seq,
    clippy::verbose_file_reads,
    clippy::wildcard_dependencies
)]
#![allow(
    missing_docs,
    unused_crate_dependencies,
    clippy::cargo_common_metadata,
    clippy::module_name_repetitions,
    clippy::use_debug
)]

#[cfg(test)]
use rustc_demangle as _;

mod mini_parser;
pub mod rust_v0;
