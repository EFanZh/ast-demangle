pub trait Symbol {
    type Path: Path + ?Sized;

    fn encoding_version(&self) -> Option<u64>;
    fn path(&self) -> &Self::Path;
    fn instantiating_crate(&self) -> Option<&Self::Path>;
    fn vendor_specific_suffix(&self) -> Option<&str>;
}

pub trait PathVisitor<'a, P, IP, I, G, T>
where
    P: ?Sized,
    IP: ?Sized,
    I: ?Sized,
    G: ?Sized + 'a,
    T: ?Sized,
{
    type Result<'b>
    where
        Self: 'b;

    fn visit_crate_root(&mut self, identifier: &'a I) -> Self::Result<'_>;
    fn visit_inherent_impl(&mut self, impl_path: &'a IP, r#type: &'a T) -> Self::Result<'_>;
    fn visit_trait_impl(&mut self, impl_path: &'a IP, r#type: &'a T, r#trait: &'a P) -> Self::Result<'_>;
    fn visit_trait_definition(&mut self, r#type: &'a T, r#trait: &'a P) -> Self::Result<'_>;
    fn visit_nested(&mut self, namespace: u8, path: &'a P, identifier: &'a I) -> Self::Result<'_>;

    fn visit_generic(&mut self, path: &'a P, generic_args: impl IntoIterator<Item = &'a G>) -> Self::Result<'_>;
}

pub trait Path {
    type ImplPath: ImplPath + ?Sized;
    type Identifier: Identifier + ?Sized;
    type GenericArg: GenericArg + ?Sized;
    type Type: Type + ?Sized;

    fn visit<'a, 'b, V>(&'a self, visitor: &'b mut V) -> V::Result<'b>
    where
        V: PathVisitor<'a, Self, Self::ImplPath, Self::Identifier, Self::GenericArg, Self::Type> + ?Sized;
}

pub trait ImplPath {
    type Path: Path + ?Sized;

    fn disambiguator(&self) -> u64;
    fn path(&self) -> &Self::Path;
}

pub trait Identifier {
    fn disambiguator(&self) -> u64;
    fn name(&self) -> &str;
}

pub trait GenericArgVisitor<'a, T, C>
where
    T: ?Sized,
    C: ?Sized,
{
    type Result<'b>
    where
        Self: 'b;

    fn visit_lifetime(&mut self, lifetime: u64) -> Self::Result<'_>;
    fn visit_type(&mut self, r#type: &'a T) -> Self::Result<'_>;
    fn visit_const(&mut self, value: &'a C) -> Self::Result<'_>;
}

pub trait GenericArg {
    type Type: Type + ?Sized;
    type Const: Const + ?Sized;

    fn visit<'a, 'b, V>(&'a self, visitor: &'b mut V) -> V::Result<'b>
    where
        V: GenericArgVisitor<'a, Self::Type, Self::Const> + ?Sized;
}

pub trait TypeVisitor<'a, P, T, B, F, D, C>
where
    P: ?Sized,
    T: ?Sized + 'a,
    B: ?Sized,
    F: ?Sized,
    D: ?Sized,
    C: ?Sized,
{
    type Result<'b>
    where
        Self: 'b;

    fn visit_basic(&mut self, basic_type: &'a B) -> Self::Result<'_>;
    fn visit_named(&mut self, path: &'a P) -> Self::Result<'_>;
    fn visit_array(&mut self, r#type: &'a T, length: &'a C) -> Self::Result<'_>;
    fn visit_slice(&mut self, r#type: &'a T) -> Self::Result<'_>;
    fn visit_tuple(&mut self, types: impl IntoIterator<Item = &'a T>) -> Self::Result<'_>;
    fn visit_ref(&mut self, lifetime: u64, r#type: &'a T) -> Self::Result<'_>;
    fn visit_ref_mut(&mut self, lifetime: u64, r#type: &'a T) -> Self::Result<'_>;
    fn visit_ptr_const(&mut self, r#type: &'a T) -> Self::Result<'_>;
    fn visit_ptr_mut(&mut self, r#type: &'a T) -> Self::Result<'_>;
    fn visit_fn(&mut self, fn_sig: &'a F) -> Self::Result<'_>;
    fn visit_dyn_trait(&mut self, dyn_bounds: &'a D, lifetime: u64) -> Self::Result<'_>;
}

pub trait Type {
    type Path: Path + ?Sized;
    type BasicType: BasicType + ?Sized;
    type FnSig: FnSig + ?Sized;
    type DynBounds: DynBounds + ?Sized;
    type Const: Const + ?Sized;

    fn visit<'a, 'b, V>(&'a self, visitor: &'b mut V) -> V::Result<'b>
    where
        V: TypeVisitor<'a, Self::Path, Self, Self::BasicType, Self::FnSig, Self::DynBounds, Self::Const> + ?Sized;
}

pub trait BasicTypeVisitor {
    type Result<'b>
    where
        Self: 'b;

    fn visit_i8(&mut self) -> Self::Result<'_>;
    fn visit_bool(&mut self) -> Self::Result<'_>;
    fn visit_char(&mut self) -> Self::Result<'_>;
    fn visit_f64(&mut self) -> Self::Result<'_>;
    fn visit_str(&mut self) -> Self::Result<'_>;
    fn visit_f32(&mut self) -> Self::Result<'_>;
    fn visit_u8(&mut self) -> Self::Result<'_>;
    fn visit_isize(&mut self) -> Self::Result<'_>;
    fn visit_usize(&mut self) -> Self::Result<'_>;
    fn visit_i32(&mut self) -> Self::Result<'_>;
    fn visit_u32(&mut self) -> Self::Result<'_>;
    fn visit_i128(&mut self) -> Self::Result<'_>;
    fn visit_u128(&mut self) -> Self::Result<'_>;
    fn visit_i16(&mut self) -> Self::Result<'_>;
    fn visit_u16(&mut self) -> Self::Result<'_>;
    fn visit_unit(&mut self) -> Self::Result<'_>;
    fn visit_ellipsis(&mut self) -> Self::Result<'_>;
    fn visit_i64(&mut self) -> Self::Result<'_>;
    fn visit_u64(&mut self) -> Self::Result<'_>;
    fn visit_never(&mut self) -> Self::Result<'_>;
    fn visit_placeholder(&mut self) -> Self::Result<'_>;
}

pub trait BasicType {
    fn visit<'a, V>(&self, visitor: &'a mut V) -> V::Result<'a>
    where
        V: BasicTypeVisitor + ?Sized;
}

pub trait FnSig {
    type Type: Type + ?Sized;
    type Abi: Abi + ?Sized;

    fn bound_lifetimes(&self) -> u64;
    fn is_unsafe(&self) -> bool;
    fn abi(&self) -> Option<&Self::Abi>;
    fn argument_types(&self) -> impl IntoIterator<Item = &Self::Type>;
    fn return_type(&self) -> &Self::Type;
}

pub trait Abi {
    fn name(&self) -> &str;
}

pub trait DynBounds {
    type DynTrait: DynTrait + ?Sized;

    fn bound_lifetimes(&self) -> u64;
    fn dyn_traits(&self) -> impl IntoIterator<Item = &Self::DynTrait>;
}

pub trait DynTrait {
    type Path: Path + ?Sized;
    type Type: Type + ?Sized;

    fn path(&self) -> &Self::Path;
    fn dyn_trait_assoc_bindings(&self) -> impl IntoIterator<Item = (&str, &Self::Type)>;
}

pub trait ConstVisitor<'a, P, C, CF>
where
    P: ?Sized,
    C: ?Sized + 'a,
    CF: ?Sized,
{
    type Result<'b>
    where
        Self: 'b;

    fn visit_i8(&mut self, value: i8) -> Self::Result<'_>;
    fn visit_u8(&mut self, value: u8) -> Self::Result<'_>;
    fn visit_isize(&mut self, value: isize) -> Self::Result<'_>;
    fn visit_usize(&mut self, value: usize) -> Self::Result<'_>;
    fn visit_i32(&mut self, value: i32) -> Self::Result<'_>;
    fn visit_u32(&mut self, value: u32) -> Self::Result<'_>;
    fn visit_i128(&mut self, value: i128) -> Self::Result<'_>;
    fn visit_u128(&mut self, value: u128) -> Self::Result<'_>;
    fn visit_i16(&mut self, value: i16) -> Self::Result<'_>;
    fn visit_u16(&mut self, value: u16) -> Self::Result<'_>;
    fn visit_i64(&mut self, value: i64) -> Self::Result<'_>;
    fn visit_u64(&mut self, value: u64) -> Self::Result<'_>;
    fn visit_bool(&mut self, value: bool) -> Self::Result<'_>;
    fn visit_char(&mut self, value: char) -> Self::Result<'_>;
    fn visit_str(&mut self, value: &'a str) -> Self::Result<'_>;
    fn visit_ref(&mut self, value: &'a C) -> Self::Result<'_>;
    fn visit_ref_mut(&mut self, value: &'a C) -> Self::Result<'_>;
    fn visit_array(&mut self, values: impl IntoIterator<Item = &'a C>) -> Self::Result<'_>;
    fn visit_tuple(&mut self, values: impl IntoIterator<Item = &'a C>) -> Self::Result<'_>;
    fn visit_named_struct(&mut self, path: &'a P, fields: &'a CF) -> Self::Result<'_>;
    fn visit_placeholder(&mut self) -> Self::Result<'_>;
}

pub trait Const {
    type Path: Path + ?Sized;
    type ConstFields: ConstFields + ?Sized;

    fn visit<'a, 'b, V>(&'a self, visitor: &'b mut V) -> V::Result<'b>
    where
        V: ConstVisitor<'a, Self::Path, Self, Self::ConstFields> + ?Sized;
}

pub trait ConstFieldsVisitor<'a, I, C>
where
    I: ?Sized + 'a,
    C: ?Sized + 'a,
{
    type Result<'b>
    where
        Self: 'b;

    fn visit_unit(&mut self) -> Self::Result<'_>;
    fn visit_tuple(&mut self, values: impl IntoIterator<Item = &'a C>) -> Self::Result<'_>;
    fn visit_struct(&mut self, values: impl IntoIterator<Item = (&'a I, &'a C)>) -> Self::Result<'_>;
}

pub trait ConstFields {
    type Identifier: Identifier + ?Sized;
    type Const: Const + ?Sized;

    fn visit<'a, 'b, V>(&'a self, visitor: &'b mut V) -> V::Result<'b>
    where
        V: ConstFieldsVisitor<'a, Self::Identifier, Self::Const> + ?Sized;
}
