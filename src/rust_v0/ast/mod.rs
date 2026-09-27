//! AST nodes.

use crate::rust_v0::display;
use std::borrow::Cow;
use std::fmt::{self, Display, Formatter};

pub mod traits;
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

impl traits::Identifier for Identifier<'_> {
    fn disambiguator(&self) -> u64 {
        self.disambiguator
    }

    fn name(&self) -> &str {
        &self.name
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

impl traits::BasicType for BasicType {
    fn visit<'a, V>(&self, visitor: &'a mut V) -> V::Result<'a>
    where
        V: traits::BasicTypeVisitor + ?Sized,
    {
        match self {
            Self::I8 => visitor.visit_i8(),
            Self::Bool => visitor.visit_bool(),
            Self::Char => visitor.visit_char(),
            Self::F64 => visitor.visit_f64(),
            Self::Str => visitor.visit_str(),
            Self::F32 => visitor.visit_f32(),
            Self::U8 => visitor.visit_u8(),
            Self::Isize => visitor.visit_isize(),
            Self::Usize => visitor.visit_usize(),
            Self::I32 => visitor.visit_i32(),
            Self::U32 => visitor.visit_u32(),
            Self::I128 => visitor.visit_i128(),
            Self::U128 => visitor.visit_u128(),
            Self::I16 => visitor.visit_i16(),
            Self::U16 => visitor.visit_u16(),
            Self::Unit => visitor.visit_unit(),
            Self::Ellipsis => visitor.visit_ellipsis(),
            Self::I64 => visitor.visit_i64(),
            Self::U64 => visitor.visit_u64(),
            Self::Never => visitor.visit_never(),
            Self::Placeholder => visitor.visit_placeholder(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Abi<'a> {
    C,
    Named(Cow<'a, str>),
}

impl traits::Abi for Abi<'_> {
    fn name(&self) -> &str {
        match self {
            Abi::C => "C",
            Abi::Named(name) => name,
        }
    }
}
