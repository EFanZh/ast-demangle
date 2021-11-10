#![no_main]

use ast_demangle::rust_v0::Symbol;

fn fuzz_with(data: &str) {
    if data
        .strip_prefix("_R")
        .map_or(false, |rest| rest.bytes().all(|c| c.is_ascii_graphic()))
    {
        let _ = Symbol::parse_from_str(data);
    }
}

libfuzzer_sys::fuzz_target!(|data: &str| {
    fuzz_with(data);
});
