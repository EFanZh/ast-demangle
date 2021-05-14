use ast_demangle::rust_v0::Symbol;
use std::env;
use std::fmt::Write;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn demangle_ast_demangle<'a>(name: &str, buffer: &'a mut String) -> Option<(&'a str, &'a str)> {
    let (symbol, rest) = Symbol::parse_from_str(name).ok()?;

    if rest.is_empty() || rest.starts_with('.') {
        let suffix = if let Some(i) = rest.find(".llvm.") {
            &rest[..i]
        } else {
            rest
        };

        write!(buffer, "{}{}", symbol, suffix).ok()?;

        let split = buffer.len();

        write!(buffer, "{:#}{}", symbol, suffix).ok()?;

        Some(buffer.split_at(split))
    } else {
        None
    }
}

fn demangle_rustc_demangle<'a>(name: &str, buffer: &'a mut String) -> Option<(&'a str, &'a str)> {
    let demangle = rustc_demangle::try_demangle(name).ok()?;

    write!(buffer, "{}", demangle).ok()?;

    let split = buffer.len();

    write!(buffer, "{:#}", demangle).ok()?;

    Some(buffer.split_at(split))
}

fn main() -> Result<(), &'static str> {
    let test_data_path = env::args_os().nth(1).unwrap();
    let mut reader = BufReader::new(File::open(test_data_path).unwrap());
    let mut line_buffer = String::new();
    let mut ast_buffer = String::new();
    let mut rustc_buffer = String::new();
    let mut failed = false;

    while let Ok(length) = reader.read_line(&mut line_buffer) {
        if length == 0 {
            break;
        }

        let line = line_buffer.trim_end();

        if line.starts_with("_R") {
            let ast_result = demangle_ast_demangle(line, &mut ast_buffer);
            let rustc_result = demangle_rustc_demangle(line, &mut rustc_buffer);

            if ast_result != rustc_result {
                failed = true;

                println!("{}", line);
                println!("    Actual = {:?}", ast_result);
                println!("    Expected = {:?}", rustc_result);
            }

            rustc_buffer.clear();
            ast_buffer.clear();
        } else {
            break;
        }

        line_buffer.clear();
    }

    if failed {
        Err("Some test cases failed.")
    } else {
        Ok(())
    }
}
