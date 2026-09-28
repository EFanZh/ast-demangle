//! AST nodes using `Rc` for shared nodes.

use crate::rust_v0::ast::{Abi, BasicType, Identifier, traits};
use crate::rust_v0::display::{self, Style};
use crate::rust_v0::parsers;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseSymbolError;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Symbol<'a> {
    pub encoding_version: Option<u64>,
    pub path: Rc<Path<'a>>,
    pub instantiating_crate: Option<Rc<Path<'a>>>,
    pub vendor_specific_suffix: Option<&'a str>,
}

impl<'a> Symbol<'a> {
    /// Returns an object that implements [`Display`] for printing the symbol.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display {
        display::display_path(self.path.as_ref(), style, 0, true)
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

        parsers::parse_symbol(input, Builder::default()).map_err(|()| ParseSymbolError)
    }
}

impl Display for Symbol<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

impl<'a> traits::Symbol for Symbol<'a> {
    type Path = Path<'a>;

    fn encoding_version(&self) -> Option<u64> {
        self.encoding_version
    }

    fn path(&self) -> &Self::Path {
        &self.path
    }

    fn instantiating_crate(&self) -> Option<&Self::Path> {
        self.instantiating_crate.as_deref()
    }

    fn vendor_specific_suffix(&self) -> Option<&str> {
        self.vendor_specific_suffix
    }
}

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Path<'a> {
    CrateRoot(Identifier<'a>),
    InherentImpl {
        impl_path: ImplPath<'a>,
        r#type: Rc<Type<'a>>,
    },
    TraitImpl {
        impl_path: ImplPath<'a>,
        r#type: Rc<Type<'a>>,
        r#trait: Rc<Self>,
    },
    TraitDefinition {
        r#type: Rc<Type<'a>>,
        r#trait: Rc<Self>,
    },
    Nested {
        namespace: u8,
        parent: Rc<Self>,
        identifier: Identifier<'a>,
    },
    Generic {
        path: Rc<Self>,
        generic_args: Vec<GenericArg<'a>>,
    },
}

impl Path<'_> {
    /// Returns an object that implements [`Display`] for printing the path.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display {
        display::display_path(self, style, 0, false)
    }
}

impl Debug for Path<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        struct DebugNamespace(u8);

        impl Debug for DebugNamespace {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.write_char('b')?;
                Debug::fmt(&char::from(self.0), f)
            }
        }

        match self {
            Self::CrateRoot(identifier) => f.debug_tuple("CrateRoot").field(identifier).finish(),
            Self::InherentImpl { impl_path, r#type } => f
                .debug_struct("InherentImpl")
                .field("impl_path", impl_path)
                .field("type", r#type.as_ref())
                .finish(),
            Self::TraitImpl {
                impl_path,
                r#type,
                r#trait,
            } => f
                .debug_struct("TraitImpl")
                .field("impl_path", impl_path)
                .field("type", r#type.as_ref())
                .field("trait", r#trait.as_ref())
                .finish(),
            Self::TraitDefinition { r#type, r#trait } => f
                .debug_struct("TraitDefinition")
                .field("type", r#type.as_ref())
                .field("trait", r#trait.as_ref())
                .finish(),
            Self::Nested {
                namespace,
                parent: path,
                identifier,
            } => f
                .debug_struct("Nested")
                .field("namespace", &DebugNamespace(*namespace))
                .field("path", path.as_ref())
                .field("identifier", identifier)
                .finish(),
            Self::Generic { path, generic_args } => f
                .debug_struct("Generic")
                .field("path", path.as_ref())
                .field("generic_args", &generic_args.as_slice())
                .finish(),
        }
    }
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

impl<'a> traits::Path for Path<'a> {
    type ImplPath = ImplPath<'a>;
    type Identifier = Identifier<'a>;
    type GenericArg = GenericArg<'a>;
    type Type = Type<'a>;

    type GenericArgs<'b>
        = &'b [GenericArg<'a>]
    where
        Self: 'b;

    fn visit<'b, 'c, V>(&'b self, visitor: &'c mut V) -> V::Result<'c>
    where
        V: traits::PathVisitor<
                'b,
                Self,
                Self::ImplPath,
                Self::Identifier,
                Self::GenericArg,
                Self::Type,
                Self::GenericArgs<'b>,
            > + ?Sized,
    {
        match self {
            Path::CrateRoot(identifier) => visitor.visit_crate_root(identifier),
            Path::InherentImpl { impl_path, r#type } => visitor.visit_inherent_impl(impl_path, r#type),
            Path::TraitImpl {
                impl_path,
                r#type,
                r#trait,
            } => visitor.visit_trait_impl(impl_path, r#type, r#trait),
            Path::TraitDefinition { r#type, r#trait } => visitor.visit_trait_definition(r#type, r#trait),
            Path::Nested {
                namespace,
                parent,
                identifier,
            } => visitor.visit_nested(*namespace, parent, identifier),
            Path::Generic { path, generic_args } => visitor.visit_generic(path, generic_args),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ImplPath<'a> {
    pub disambiguator: u64,
    pub path: Rc<Path<'a>>,
}

impl<'a> traits::ImplPath for ImplPath<'a> {
    type Path = Path<'a>;

    fn disambiguator(&self) -> u64 {
        self.disambiguator
    }

    fn path(&self) -> &Self::Path {
        &self.path
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
    pub fn display(&self, style: Style) -> impl Display {
        display::display_generic_arg(self, style, 0)
    }
}

impl Display for GenericArg<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

impl<'a> traits::GenericArg for GenericArg<'a> {
    type Type = Type<'a>;
    type Const = Const<'a>;

    fn visit<'b, 'c, V>(&'b self, visitor: &'c mut V) -> V::Result<'c>
    where
        V: traits::GenericArgVisitor<'b, <Self as traits::GenericArg>::Type, <Self as traits::GenericArg>::Const>
            + ?Sized,
    {
        match self {
            GenericArg::Lifetime(lifetime) => visitor.visit_lifetime(*lifetime),
            GenericArg::Type(r#type) => visitor.visit_type(r#type),
            GenericArg::Const(value) => visitor.visit_const(value),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type<'a> {
    Basic(BasicType),
    Named(Rc<Path<'a>>),
    Array(Rc<Self>, Rc<Const<'a>>),
    Slice(Rc<Self>),
    Tuple(Vec<Rc<Self>>),
    Ref { lifetime: u64, r#type: Rc<Self> },
    RefMut { lifetime: u64, r#type: Rc<Self> },
    PtrConst(Rc<Self>),
    PtrMut(Rc<Self>),
    Fn(FnSig<'a>),
    DynTrait { dyn_bounds: DynBounds<'a>, lifetime: u64 },
}

impl Type<'_> {
    /// Returns an object that implements [`Display`] for printing the type.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display {
        display::display_type(self, style, 0)
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

impl<'a> traits::Type for Type<'a> {
    type Path = Path<'a>;
    type BasicType = BasicType;
    type FnSig = FnSig<'a>;
    type DynBounds = DynBounds<'a>;
    type Const = Const<'a>;

    fn visit<'b, 'c, V>(&'b self, visitor: &'c mut V) -> V::Result<'c>
    where
        V: traits::TypeVisitor<'b, Self::Path, Self, Self::BasicType, Self::FnSig, Self::DynBounds, Self::Const>
            + ?Sized,
    {
        match self {
            Type::Basic(basic_type) => visitor.visit_basic(basic_type),
            Type::Named(path) => visitor.visit_named(path),
            Type::Array(r#type, length) => visitor.visit_array(r#type, length),
            Type::Slice(r#type) => visitor.visit_slice(r#type),
            Type::Tuple(types) => visitor.visit_tuple(types.iter().map(Rc::as_ref)),
            Type::Ref { lifetime, r#type } => visitor.visit_ref(*lifetime, r#type),
            Type::RefMut { lifetime, r#type } => visitor.visit_ref_mut(*lifetime, r#type),
            Type::PtrConst(r#type) => visitor.visit_ptr_const(r#type),
            Type::PtrMut(r#type) => visitor.visit_ptr_mut(r#type),
            Type::Fn(fn_sig) => visitor.visit_fn(fn_sig),
            Type::DynTrait { dyn_bounds, lifetime } => visitor.visit_dyn_trait(dyn_bounds, *lifetime),
        }
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
    pub fn display(&self, style: Style) -> impl Display {
        display::display_fn_sig(self, style, 0)
    }
}

impl Display for FnSig<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

impl<'a> traits::FnSig for FnSig<'a> {
    type Type = Type<'a>;
    type Abi = Abi<'a>;

    fn bound_lifetimes(&self) -> u64 {
        self.bound_lifetimes
    }

    fn is_unsafe(&self) -> bool {
        self.is_unsafe
    }

    fn abi(&self) -> Option<&Self::Abi> {
        self.abi.as_ref()
    }

    fn argument_types(&self) -> impl IntoIterator<Item = &Self::Type> {
        self.argument_types.iter().map(Rc::as_ref)
    }

    fn return_type(&self) -> &Self::Type {
        &self.return_type
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynBounds<'a> {
    pub bound_lifetimes: u64,
    pub dyn_traits: Vec<DynTrait<'a>>,
}

impl<'a> traits::DynBounds for DynBounds<'a> {
    type DynTrait = DynTrait<'a>;

    fn bound_lifetimes(&self) -> u64 {
        self.bound_lifetimes
    }

    fn dyn_traits(&self) -> impl IntoIterator<Item = &Self::DynTrait> {
        &self.dyn_traits
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynTrait<'a> {
    pub path: Rc<Path<'a>>,
    pub assoc_bindings: Vec<(Cow<'a, str>, Rc<Type<'a>>)>,
}

impl<'a> traits::DynTrait for DynTrait<'a> {
    type Path = Path<'a>;
    type Type = Type<'a>;

    fn path(&self) -> &Self::Path {
        &self.path
    }

    fn assoc_bindings(&self) -> impl IntoIterator<Item = (&str, &Self::Type)> {
        self.assoc_bindings
            .iter()
            .map(|(name, value)| (name.as_ref(), value.as_ref()))
    }
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
    Str(String),
    Ref(Rc<Self>),
    RefMut(Rc<Self>),
    Array(Vec<Rc<Self>>),
    Tuple(Vec<Rc<Self>>),
    NamedStruct {
        path: Rc<Path<'a>>,
        fields: ConstFields<'a>,
    },
    Placeholder,
}

impl Const<'_> {
    /// Returns an object that implements [`Display`] for printing the constant value.
    #[must_use]
    pub fn display(&self, style: Style) -> impl Display {
        display::display_const(self, style, 0, true)
    }
}

impl Display for Const<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display(if f.alternate() { Style::Normal } else { Style::Long })
            .fmt(f)
    }
}

impl<'a> traits::Const for Const<'a> {
    type Path = Path<'a>;
    type ConstFields = ConstFields<'a>;

    fn visit<'b, 'c, V>(&'b self, visitor: &'c mut V) -> V::Result<'c>
    where
        V: traits::ConstVisitor<'b, Self::Path, Self, Self::ConstFields> + ?Sized,
    {
        match self {
            Const::I8(value) => visitor.visit_i8(*value),
            Const::U8(value) => visitor.visit_u8(*value),
            Const::Isize(value) => visitor.visit_isize(*value),
            Const::Usize(value) => visitor.visit_usize(*value),
            Const::I32(value) => visitor.visit_i32(*value),
            Const::U32(value) => visitor.visit_u32(*value),
            Const::I128(value) => visitor.visit_i128(*value),
            Const::U128(value) => visitor.visit_u128(*value),
            Const::I16(value) => visitor.visit_i16(*value),
            Const::U16(value) => visitor.visit_u16(*value),
            Const::I64(value) => visitor.visit_i64(*value),
            Const::U64(value) => visitor.visit_u64(*value),
            Const::Bool(value) => visitor.visit_bool(*value),
            Const::Char(value) => visitor.visit_char(*value),
            Const::Str(value) => visitor.visit_str(value),
            Const::Ref(value) => visitor.visit_ref(value),
            Const::RefMut(value) => visitor.visit_ref_mut(value),
            Const::Array(values) => visitor.visit_array(values.iter().map(Rc::as_ref)),
            Const::Tuple(values) => visitor.visit_tuple(values.iter().map(Rc::as_ref)),
            Const::NamedStruct { path, fields } => visitor.visit_named_struct(path, fields),
            Const::Placeholder => visitor.visit_placeholder(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ConstFields<'a> {
    Unit,
    Tuple(Vec<Rc<Const<'a>>>),
    Struct(Vec<(Identifier<'a>, Rc<Const<'a>>)>),
}

impl<'a> traits::ConstFields for ConstFields<'a> {
    type Identifier = Identifier<'a>;
    type Const = Const<'a>;

    fn visit<'b, 'c, V>(&'b self, visitor: &'c mut V) -> V::Result<'c>
    where
        V: traits::ConstFieldsVisitor<'b, Self::Identifier, Self::Const> + ?Sized,
    {
        match self {
            ConstFields::Unit => visitor.visit_unit(),
            ConstFields::Tuple(fields) => visitor.visit_tuple(fields.iter().map(Rc::as_ref)),
            ConstFields::Struct(fields) => {
                visitor.visit_struct(fields.iter().map(|(name, value)| (name, value.as_ref())))
            }
        }
    }
}

#[derive(Default)]
pub struct Builder<'a> {
    paths: HashMap<usize, Rc<Path<'a>>>,
    types: HashMap<usize, Rc<Type<'a>>>,
    consts: HashMap<usize, Rc<Const<'a>>>,
}

impl<'a> parsers::Builder<'a> for Builder<'a> {
    type Identifier = Identifier<'a>;
    type GenericArg = GenericArg<'a>;
    type Symbol = Symbol<'a>;
    type Path = Rc<Path<'a>>;
    type Type = Rc<Type<'a>>;
    type Const = Rc<Const<'a>>;
    type ImplPath = ImplPath<'a>;
    type BasicType = BasicType;
    type FnSig = FnSig<'a>;
    type DynBounds = DynBounds<'a>;
    type Abi = Abi<'a>;
    type DynTrait = DynTrait<'a>;
    type ConstFields = ConstFields<'a>;

    fn make_symbol(
        &mut self,
        encoding_version: Option<u64>,
        path: Self::Path,
        instantiating_crate: Option<Self::Path>,
        vendor_specific_suffix: Option<&'a str>,
    ) -> Self::Symbol {
        Symbol {
            encoding_version,
            path,
            instantiating_crate,
            vendor_specific_suffix,
        }
    }

    fn make_crate_root_path(&mut self, identifier: Self::Identifier) -> Self::Path {
        Rc::new(Path::CrateRoot(identifier))
    }

    fn make_inherent_impl_path(&mut self, impl_path: Self::ImplPath, r#type: Self::Type) -> Self::Path {
        Rc::new(Path::InherentImpl { impl_path, r#type })
    }

    fn make_trait_impl_path(
        &mut self,
        impl_path: Self::ImplPath,
        r#type: Self::Type,
        r#trait: Self::Path,
    ) -> Self::Path {
        Rc::new(Path::TraitImpl {
            impl_path,
            r#type,
            r#trait,
        })
    }

    fn make_trait_definition_path(&mut self, r#type: Self::Type, r#trait: Self::Path) -> Self::Path {
        Rc::new(Path::TraitDefinition { r#type, r#trait })
    }

    fn make_nested_path(&mut self, namespace: u8, path: Self::Path, identifier: Self::Identifier) -> Self::Path {
        Rc::new(Path::Nested {
            namespace,
            parent: path,
            identifier,
        })
    }

    fn make_generic_path(&mut self, path: Self::Path, generic_args: Vec<Self::GenericArg>) -> Self::Path {
        Rc::new(Path::Generic { path, generic_args })
    }

    fn make_impl_path(&mut self, disambiguator: u64, path: Self::Path) -> Self::ImplPath {
        ImplPath { disambiguator, path }
    }

    fn make_identifier(&mut self, disambiguator: u64, name: Cow<'a, str>) -> Self::Identifier {
        Identifier { disambiguator, name }
    }

    fn make_lifetime_generic_arg(&mut self, lifetime: u64) -> Self::GenericArg {
        GenericArg::Lifetime(lifetime)
    }

    fn make_type_generic_arg(&mut self, r#type: Self::Type) -> Self::GenericArg {
        GenericArg::Type(r#type)
    }

    fn make_const_generic_arg(&mut self, value: Self::Const) -> Self::GenericArg {
        GenericArg::Const(value)
    }

    fn make_basic_type_type(&mut self, basic_type: Self::BasicType) -> Self::Type {
        Rc::new(Type::Basic(basic_type))
    }

    fn make_named_type(&mut self, path: Self::Path) -> Self::Type {
        Rc::new(Type::Named(path))
    }

    fn make_array_type(&mut self, r#type: Self::Type, length: Self::Const) -> Self::Type {
        Rc::new(Type::Array(r#type, length))
    }

    fn make_slice_type(&mut self, r#type: Self::Type) -> Self::Type {
        Rc::new(Type::Slice(r#type))
    }

    fn make_tuple_type(&mut self, types: Vec<Self::Type>) -> Self::Type {
        Rc::new(Type::Tuple(types))
    }

    fn make_ref_type(&mut self, lifetime: u64, r#type: Self::Type) -> Self::Type {
        Rc::new(Type::Ref { lifetime, r#type })
    }

    fn make_ref_mut_type(&mut self, lifetime: u64, r#type: Self::Type) -> Self::Type {
        Rc::new(Type::RefMut { lifetime, r#type })
    }

    fn make_ptr_const_type(&mut self, r#type: Self::Type) -> Self::Type {
        Rc::new(Type::PtrConst(r#type))
    }

    fn make_ptr_mut_type(&mut self, r#type: Self::Type) -> Self::Type {
        Rc::new(Type::PtrMut(r#type))
    }

    fn make_fn_type(&mut self, fn_sig: Self::FnSig) -> Self::Type {
        Rc::new(Type::Fn(fn_sig))
    }

    fn make_dyn_trait_type(&mut self, dyn_bounds: Self::DynBounds, lifetime: u64) -> Self::Type {
        Rc::new(Type::DynTrait { dyn_bounds, lifetime })
    }

    fn make_i8_basic_type(&mut self) -> Self::BasicType {
        BasicType::I8
    }

    fn make_bool_basic_type(&mut self) -> Self::BasicType {
        BasicType::Bool
    }

    fn make_char_basic_type(&mut self) -> Self::BasicType {
        BasicType::Char
    }

    fn make_f64_basic_type(&mut self) -> Self::BasicType {
        BasicType::F64
    }

    fn make_str_basic_type(&mut self) -> Self::BasicType {
        BasicType::Str
    }

    fn make_f32_basic_type(&mut self) -> Self::BasicType {
        BasicType::F32
    }

    fn make_u8_basic_type(&mut self) -> Self::BasicType {
        BasicType::U8
    }

    fn make_isize_basic_type(&mut self) -> Self::BasicType {
        BasicType::Isize
    }

    fn make_usize_basic_type(&mut self) -> Self::BasicType {
        BasicType::Usize
    }

    fn make_i32_basic_type(&mut self) -> Self::BasicType {
        BasicType::I32
    }

    fn make_u32_basic_type(&mut self) -> Self::BasicType {
        BasicType::U32
    }

    fn make_i128_basic_type(&mut self) -> Self::BasicType {
        BasicType::I128
    }

    fn make_u128_basic_type(&mut self) -> Self::BasicType {
        BasicType::U128
    }

    fn make_i16_basic_type(&mut self) -> Self::BasicType {
        BasicType::I16
    }

    fn make_u16_basic_type(&mut self) -> Self::BasicType {
        BasicType::U16
    }

    fn make_unit_basic_type(&mut self) -> Self::BasicType {
        BasicType::Unit
    }

    fn make_ellipsis_basic_type(&mut self) -> Self::BasicType {
        BasicType::Ellipsis
    }

    fn make_i64_basic_type(&mut self) -> Self::BasicType {
        BasicType::I64
    }

    fn make_u64_basic_type(&mut self) -> Self::BasicType {
        BasicType::U64
    }

    fn make_never_basic_type(&mut self) -> Self::BasicType {
        BasicType::Never
    }

    fn make_placeholder_basic_type(&mut self) -> Self::BasicType {
        BasicType::Placeholder
    }

    fn make_fn_sig(
        &mut self,
        bound_lifetimes: u64,
        is_unsafe: bool,
        abi: Option<Self::Abi>,
        argument_types: Vec<Self::Type>,
        return_type: Self::Type,
    ) -> Self::FnSig {
        FnSig {
            bound_lifetimes,
            is_unsafe,
            abi,
            argument_types,
            return_type,
        }
    }

    fn make_c_abi(&mut self) -> Self::Abi {
        Abi::C
    }

    fn make_named_abi(&mut self, name: Cow<'a, str>) -> Self::Abi {
        Abi::Named(name)
    }

    fn make_dyn_bounds(&mut self, bound_lifetimes: u64, dyn_traits: Vec<Self::DynTrait>) -> Self::DynBounds {
        DynBounds {
            bound_lifetimes,
            dyn_traits,
        }
    }

    fn make_dyn_trait(&mut self, path: Self::Path, assoc_bindings: Vec<(Cow<'a, str>, Self::Type)>) -> Self::DynTrait {
        DynTrait { path, assoc_bindings }
    }

    fn make_i8_const(&mut self, value: i8) -> Self::Const {
        Rc::new(Const::I8(value))
    }

    fn make_u8_const(&mut self, value: u8) -> Self::Const {
        Rc::new(Const::U8(value))
    }

    fn make_isize_const(&mut self, value: isize) -> Self::Const {
        Rc::new(Const::Isize(value))
    }

    fn make_usize_const(&mut self, value: usize) -> Self::Const {
        Rc::new(Const::Usize(value))
    }

    fn make_i32_const(&mut self, value: i32) -> Self::Const {
        Rc::new(Const::I32(value))
    }

    fn make_u32_const(&mut self, value: u32) -> Self::Const {
        Rc::new(Const::U32(value))
    }

    fn make_i128_const(&mut self, value: i128) -> Self::Const {
        Rc::new(Const::I128(value))
    }

    fn make_u128_const(&mut self, value: u128) -> Self::Const {
        Rc::new(Const::U128(value))
    }

    fn make_i16_const(&mut self, value: i16) -> Self::Const {
        Rc::new(Const::I16(value))
    }

    fn make_u16_const(&mut self, value: u16) -> Self::Const {
        Rc::new(Const::U16(value))
    }

    fn make_i64_const(&mut self, value: i64) -> Self::Const {
        Rc::new(Const::I64(value))
    }

    fn make_u64_const(&mut self, value: u64) -> Self::Const {
        Rc::new(Const::U64(value))
    }

    fn make_bool_const(&mut self, value: bool) -> Self::Const {
        Rc::new(Const::Bool(value))
    }

    fn make_char_const(&mut self, value: char) -> Self::Const {
        Rc::new(Const::Char(value))
    }

    fn make_str_const(&mut self, value: String) -> Self::Const {
        Rc::new(Const::Str(value))
    }

    fn make_ref_const(&mut self, value: Self::Const) -> Self::Const {
        Rc::new(Const::Ref(value))
    }

    fn make_ref_mut_const(&mut self, value: Self::Const) -> Self::Const {
        Rc::new(Const::RefMut(value))
    }

    fn make_array_const(&mut self, values: Vec<Self::Const>) -> Self::Const {
        Rc::new(Const::Array(values))
    }

    fn make_tuple_const(&mut self, values: Vec<Self::Const>) -> Self::Const {
        Rc::new(Const::Tuple(values))
    }

    fn make_named_struct_const(&mut self, path: Self::Path, fields: Self::ConstFields) -> Self::Const {
        Rc::new(Const::NamedStruct { path, fields })
    }

    fn make_placeholder_const(&mut self) -> Self::Const {
        Rc::new(Const::Placeholder)
    }

    fn make_unit_const_fields(&mut self) -> Self::ConstFields {
        ConstFields::Unit
    }

    fn make_tuple_const_fields(&mut self, values: Vec<Self::Const>) -> Self::ConstFields {
        ConstFields::Tuple(values)
    }

    fn make_struct_const_fields(&mut self, values: Vec<(Self::Identifier, Self::Const)>) -> Self::ConstFields {
        ConstFields::Struct(values)
    }

    fn query_const(&mut self, index: usize) -> Option<Self::Const> {
        self.consts.get(&index).cloned()
    }

    fn save_const(&mut self, index: usize, value: &Self::Const) {
        self.consts.insert(index, Rc::clone(value));
    }

    fn query_path(&mut self, index: usize) -> Option<Self::Path> {
        self.paths.get(&index).cloned()
    }

    fn save_path(&mut self, index: usize, value: &Self::Path) {
        self.paths.insert(index, Rc::clone(value));
    }

    fn query_type(&mut self, index: usize) -> Option<Self::Type> {
        self.types.get(&index).cloned()
    }

    fn save_type(&mut self, index: usize, value: &Self::Type) {
        self.types.insert(index, Rc::clone(value));
    }
}
