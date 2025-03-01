//! Fuzz target for the `ast-demangle`.

#![no_main]

use ast_demangle::rust_v0::Symbol;
use std::io::{self, Sink, Write};
use test_utilities::BoundedWriter;

const fn bounded_writer() -> BoundedWriter<Sink> {
    BoundedWriter::new(io::sink(), 65536)
}

fn fuzz_with(data: &str) {
    if data
        .strip_prefix("_R")
        .is_some_and(|rest| rest.bytes().all(|c| c.is_ascii_graphic()))
    {
        let mut sink_1 = bounded_writer();

        if let Ok((symbol, _)) = Symbol::parse_from_str(data) {
            write!(sink_1, "{symbol}").ok();
            write!(sink_1, "{symbol:#}").ok();
        }

        let mut sink_2 = bounded_writer();

        if let Ok(symbol) = rustc_demangle::try_demangle(data) {
            write!(sink_2, "{symbol}").ok();
            write!(sink_2, "{symbol:#}").ok();
        }
    }
}

libfuzzer_sys::fuzz_target!(|data: &str| {
    fuzz_with(data);
});
