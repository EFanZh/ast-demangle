#![cfg_attr(test, expect(unused_crate_dependencies, reason = "false positive"))]

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
//!         encoding_version: None,
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

#[expect(missing_docs, reason = "too many work")]
pub mod rust_v0;
