use ast_demangle::rust_v0::Symbol;
use std::fmt::{self, Write};

const TEST_DATA: &str = include_str!("test-against-rustc-demangle-data.txt");

struct StopIfFull<'a>(&'a mut String);

impl Write for StopIfFull<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.0.len() < 65536 {
            self.0.push_str(s);

            Ok(())
        } else {
            Err(fmt::Error)
        }
    }
}

#[allow(clippy::if_then_some_else_none)] // See <https://github.com/rust-lang/rust-clippy/issues/7870>.
fn demangle_ast_demangle<'a>(name: &str, buffer: &'a mut String) -> Option<(&'a str, &'a str)> {
    let mut buffer = StopIfFull(buffer);
    let (symbol, rest) = Symbol::parse_from_str(name).ok()?;

    if rest.is_empty() || rest.starts_with('.') {
        let suffix = rest.find(".llvm.").map_or(rest, |i| &rest[..i]);

        write!(buffer, "{}{}", symbol, suffix).ok()?;

        let split = buffer.0.len();

        write!(buffer, "{:#}{}", symbol, suffix).ok()?;

        Some(buffer.0.split_at(split))
    } else {
        None
    }
}

fn demangle_rustc_demangle<'a>(name: &str, buffer: &'a mut String) -> Option<(&'a str, &'a str)> {
    fn has_error(buffer: &str) -> bool {
        [
            "<?>",
            "<? as ?>",
            "{invalid syntax}",
            "{recursion limit reached}",
            "{size limit reached}",
        ]
        .into_iter()
        .any(|pattern| buffer.contains(pattern))
    }

    let mut buffer = StopIfFull(buffer);
    let demangle = rustc_demangle::try_demangle(name).ok()?;

    write!(buffer, "{}", demangle).ok()?;

    if has_error(buffer.0) {
        return None;
    }

    let split = buffer.0.len();

    write!(buffer, "{:#}", demangle).ok()?;

    if has_error(buffer.0) {
        return None;
    }

    Some(buffer.0.split_at(split))
}

#[test]
fn test_against_rustc_demangle() {
    let mut ast_demangle_buffer = String::new();
    let mut rustc_demangle_buffer = String::new();

    for line in TEST_DATA.lines() {
        if !line.is_empty() && !line.starts_with('#') {
            let ast_demangle_result = demangle_ast_demangle(line, &mut ast_demangle_buffer);
            let rustc_demangle_result = demangle_rustc_demangle(line, &mut rustc_demangle_buffer);

            assert_eq!(ast_demangle_result, rustc_demangle_result, "Failed: {}", line);

            ast_demangle_buffer.clear();
            rustc_demangle_buffer.clear();
        }
    }
}
