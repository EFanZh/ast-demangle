# ast-demangle

[![crates.io](https://img.shields.io/crates/v/ast-demangle)](https://crates.io/crates/ast-demangle)
[![docs](https://docs.rs/ast-demangle/badge.svg)](https://docs.rs/ast-demangle)
[![CI](https://github.com/EFanZh/ast-demangle/actions/workflows/ci.yml/badge.svg)](https://github.com/EFanZh/ast-demangle/actions/workflows/ci.yml)
[![Codecov](https://codecov.io/gh/EFanZh/ast-demangle/branch/main/graph/badge.svg)](https://codecov.io/gh/EFanZh/ast-demangle)
[![Coveralls](https://coveralls.io/repos/github/EFanZh/ast-demangle/badge.svg?branch=main)](https://coveralls.io/github/EFanZh/ast-demangle)

Parses mangled names and produces structured results.

Example:

```rust
use ast_demangle::rust_v0::{DisplayStyle, Identifier, Path, Symbol, UndisambiguatedIdentifier};
use std::borrow::Cow;

let mangled_name = "_RNvNtCs6GSVXm7oiwY_5regex4utf811decode_utf8.llvm.1119170478327948870";
let (symbol, suffix) = Symbol::parse_from_str(mangled_name).unwrap();

// The suffix is returned.
assert_eq!(suffix, ".llvm.1119170478327948870");

// The default style for displaying is the long format.
assert_eq!(format!("{}", symbol), "regex[4df147058689a776]::utf8::decode_utf8");

// To omit the crate hash, use the alternate display format.
assert_eq!(format!("{:#}", symbol), "regex::utf8::decode_utf8");

// Use `Symbol::display` and `DisplayStyle` to specify the display style explicitly.

assert_eq!(format!("{}", symbol.display(DisplayStyle::Short)), "decode_utf8");
assert_eq!(format!("{}", symbol.display(DisplayStyle::Normal)), "regex::utf8::decode_utf8");

assert_eq!(
    format!("{}", symbol.display(DisplayStyle::Long)),
    "regex[4df147058689a776]::utf8::decode_utf8"
);

// You can access the structure of the demangled symbol.

assert_eq!(
    symbol,
    Symbol {
        version: None,
        path: Path::Nested {
            namespace: b'v',
            path: Path::Nested {
                namespace: b't',
                path: Path::CrateRoot(Identifier {
                    disambiguator: 0x4df1_4705_8689_a776,
                    name: Cow::Borrowed("regex")
                })
                .into(),
                identifier: Identifier {
                    disambiguator: 0,
                    name: Cow::Borrowed("utf8")
                }
            }
            .into(),
            identifier: Identifier {
                disambiguator: 0,
                name: Cow::Borrowed("decode_utf8")
            }
        }
        .into(),
        instantiating_crate: None
    }
);
```
