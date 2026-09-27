//! AST nodes.

use crate::rust_v0::display;
use std::borrow::Cow;
use std::fmt::{self, Display, Formatter};

pub mod unsync;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier<'a> {
    pub disambiguator: u64,
    pub name: Cow<'a, str>,
}

impl Identifier<'_> {
    /// Returns an object that implements [`Display`] for printing the identifier.
    #[must_use]
    pub fn display(&self) -> impl Display {
        self.name.as_ref()
    }
}

impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display().fmt(f)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BasicType {
    I8,
    Bool,
    Char,
    F64,
    Str,
    F32,
    U8,
    Isize,
    Usize,
    I32,
    U32,
    I128,
    U128,
    I16,
    U16,
    Unit,
    Ellipsis,
    I64,
    U64,
    Never,
    Placeholder,
}

impl BasicType {
    /// Returns an object that implements [`Display`] for printing the basic type.
    #[must_use]
    pub fn display(self) -> impl Display {
        display::display_basic_type(self)
    }
}

impl Display for BasicType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display().fmt(f)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Abi<'a> {
    C,
    Named(Cow<'a, str>),
}
