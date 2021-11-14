#![no_main]

use ast_demangle::rust_v0::Symbol;
use std::io::{self, Write};
use test_utilities::BoundedWriter;

fn fuzz_with(data: &str) {
    if data
        .strip_prefix("_R")
        .map_or(false, |rest| rest.bytes().all(|c| c.is_ascii_graphic()))
    {
        let mut sink = BoundedWriter::new(io::sink(), 65536);

        if let Ok((symbol, _)) = Symbol::parse_from_str(data) {
            let _ = write!(sink, "{}", symbol);
            let _ = write!(sink, "{:#}", symbol);
        }
    }
}

libfuzzer_sys::fuzz_target!(|data: &str| {
    fuzz_with(data);
});
