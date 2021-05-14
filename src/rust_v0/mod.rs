use self::context::Context;
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use std::str;

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
    pub fn parse_from_str(input: &'a str) -> Result<(Self, &'a str), ParseSymbolError> {
        let input = input
            .strip_prefix("_R")
            .or_else(|| input.strip_prefix("R"))
            .or_else(|| input.strip_prefix("__R"))
            .ok_or(ParseSymbolError)?;

        parsers::parse_symbol(Context::new(input, &RefCell::default()))
            .map(|(context, result)| (result, context.data))
            .map_err(|_| ParseSymbolError)
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

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum GenericArg<'a> {
    Lifetme(u64),
    Type(Rc<Type<'a>>),
    Const(Rc<Const<'a>>),
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

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FnSig<'a> {
    pub bound_lifetimes: u64,
    pub is_unsafe: bool,
    pub abi: Option<Abi<'a>>,
    pub argument_types: Vec<Rc<Type<'a>>>,
    pub return_type: Rc<Type<'a>>,
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
    Data { type_: Rc<Type<'a>>, data: &'a str },
    Placeholder,
}
