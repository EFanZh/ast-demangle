#![no_main]

use ast_demangle::rust_v0::Symbol;

fn fuzz_with(data: &str) {
    let _ = Symbol::parse_from_str(data);
}

libfuzzer_sys::fuzz_target!(|data: &str| {
    fuzz_with(data);
});
