//! Tools for demangling symbols using
//! [Rust v0 syntax](https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html#syntax-of-mangled-names).

pub use self::display::Style as DisplayStyle;

mod display;
mod parsers;

pub mod ast;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseSymbolError;
