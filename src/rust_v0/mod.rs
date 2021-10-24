//! Tools for demangling symbols using
//! [Rust v0 syntax]((https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html#syntax-of-mangled-names)).

use crate::rust_v0::context::Context;
use crate::rust_v0::display::Style;
use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

mod context;
pub mod display;
mod parsers;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseSymbolError;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Symbol<'a> {
    pub version: Option<u64>,
    pub path: Rc<Path<'a>>,
    pub instantiating_crate: Option<Rc<Path<'a>>>,
}

impl<'a> Symbol<'a> {
    /// Returns an object that implements [`Display`] for printing the symbol.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display + '_ {
        display::display_path(&self.path, style, 0, true)
    }

    /// Parses `input` with Rust
    /// [v0 syntax](https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html#syntax-of-mangled-names),
    /// returns a tuple that contains a [`Symbol`] object and an [`&str`] object containing the suffix that is
    /// not part of the the Rust v0 syntax.
    ///
    /// # Errors
    ///
    /// Returns [`ParseSymbolError`] if `input` does not start with a valid prefix with Rust v0 syntax.
    pub fn parse_from_str(input: &'a str) -> Result<(Self, &'a str), ParseSymbolError> {
        let input = input
            .strip_prefix("_R")
            .or_else(|| input.strip_prefix('R'))
            .or_else(|| input.strip_prefix("__R"))
            .ok_or(ParseSymbolError)?;

        parsers::parse_symbol(Context::new(input, &RefCell::default()))
            .map(|(context, result)| (result, context.data))
            .map_err(|_| ParseSymbolError)
    }
}

impl Display for Symbol<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Path<'a> {
    CrateRoot(Identifier<'a>),
    InherentImpl {
        impl_path: ImplPath<'a>,
        type_: Rc<Type<'a>>,
    },
    TraitImpl {
        impl_path: ImplPath<'a>,
        type_: Rc<Type<'a>>,
        trait_: Rc<Path<'a>>,
    },
    TraitDefinition {
        type_: Rc<Type<'a>>,
        trait_: Rc<Path<'a>>,
    },
    Nested {
        namespace: u8,
        path: Rc<Path<'a>>,
        name: Identifier<'a>,
    },
    Generic {
        path: Rc<Path<'a>>,
        generic_args: Vec<GenericArg<'a>>,
    },
}

impl Path<'_> {
    /// Returns an object that implements [`Display`] for printing the path.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display + '_ {
        display::display_path(self, style, 0, false)
    }
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ImplPath<'a> {
    pub disambiguator: u64,
    pub path: Rc<Path<'a>>,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier<'a> {
    pub disambiguator: u64,
    pub name: Cow<'a, str>,
}

impl Identifier<'_> {
    /// Returns an object that implements [`Display`] for printing the identifier.
    #[must_use]
    pub fn display(&self) -> impl Display + '_ {
        self.name.as_ref()
    }
}

impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display().fmt(f)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum GenericArg<'a> {
    Lifetime(u64),
    Type(Rc<Type<'a>>),
    Const(Rc<Const<'a>>),
}

impl GenericArg<'_> {
    /// Returns an object that implements [`Display`] for printing the generic argument.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display + '_ {
        display::display_generic_arg(self, style, 0)
    }
}

impl Display for GenericArg<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type<'a> {
    Basic(BasicType),
    Named(Rc<Path<'a>>),
    Array(Rc<Type<'a>>, Rc<Const<'a>>),
    Slice(Rc<Type<'a>>),
    Tuple(Vec<Rc<Type<'a>>>),
    Ref { lifetime: u64, type_: Rc<Type<'a>> },
    RefMut { lifetime: u64, type_: Rc<Type<'a>> },
    PtrConst(Rc<Type<'a>>),
    PtrMut(Rc<Type<'a>>),
    Fn(FnSig<'a>),
    DynTrait { dyn_bounds: DynBounds<'a>, lifetime: u64 },
}

impl Type<'_> {
    /// Returns an object that implements [`Display`] for printing the type.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display + '_ {
        display::display_type(self, style, 0)
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
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
pub struct FnSig<'a> {
    pub bound_lifetimes: u64,
    pub is_unsafe: bool,
    pub abi: Option<Abi<'a>>,
    pub argument_types: Vec<Rc<Type<'a>>>,
    pub return_type: Rc<Type<'a>>,
}

impl FnSig<'_> {
    /// Returns an object that implements [`Display`] for printing the function signature.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display + '_ {
        display::display_fn_sig(self, style, 0)
    }
}

impl Display for FnSig<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Abi<'a> {
    C,
    Named(Cow<'a, str>),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynBounds<'a> {
    pub bound_lifetimes: u64,
    pub dyn_traits: Vec<DynTrait<'a>>,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynTrait<'a> {
    pub path: Rc<Path<'a>>,
    pub dyn_trait_assoc_bindings: Vec<DynTraitAssocBinding<'a>>,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynTraitAssocBinding<'a> {
    pub name: Cow<'a, str>,
    pub type_: Rc<Type<'a>>,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Const<'a> {
    I8(i8),
    U8(u8),
    Isize(isize),
    Usize(usize),
    I32(i32),
    U32(u32),
    I128(i128),
    U128(u128),
    I16(i16),
    U16(u16),
    I64(i64),
    U64(u64),
    Bool(bool),
    Char(char),
    Str(ConstStr<'a>),
    Ref(Rc<Const<'a>>),
    RefMut(Rc<Const<'a>>),
    Array(Vec<Rc<Const<'a>>>),
    Tuple(Vec<Rc<Const<'a>>>),
    NamedStruct {
        path: Rc<Path<'a>>,
        fields: ConstFields<'a>,
    },
    Placeholder,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ConstFields<'a> {
    Unit,
    Tuple(Vec<Rc<Const<'a>>>),
    Struct(Vec<(Identifier<'a>, Rc<Const<'a>>)>),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ConstStr<'a>(&'a str);

impl Const<'_> {
    /// Returns an object that implements [`Display`] for printing the constant value.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display + '_ {
        display::display_const(self, style, 0)
    }
}

impl Display for Const<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}
