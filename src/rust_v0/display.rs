//! Pretty printing demangled symbol names.

use crate::rust_v0::ast::traits::{
    Abi, BasicType, BasicTypeVisitor, Const, ConstFields, ConstFieldsVisitor, ConstVisitor, DynBounds, DynTrait, FnSig,
    GenericArg, GenericArgVisitor, Identifier, ImplPath, Path, PathVisitor, Type, TypeVisitor,
};
use std::any;
use std::fmt::{self, Debug, Display, Formatter, LowerHex, Write};

/// Denote the style for displaying the symbol.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Style {
    /// Omit enclosing namespaces to get a shorter name.
    Short,
    /// Omit crate hashes and const value types. This matches rustc-demangle’s `{}` format.
    Normal,
    /// Show crate hashes and const value types. This matches rustc-demangle’s `{:#}` format. Note that even with this
    /// style, impl paths are still omitted.
    Long,
}

struct DisplayPathVisitor<'a, 'b> {
    style: Style,
    bound_lifetime_depth: u64,
    in_value: bool,
    formatter: &'a mut Formatter<'b>,
}

impl<'a, P, IP, I, G, T, GS> PathVisitor<'a, P, IP, I, G, T, GS> for DisplayPathVisitor<'_, '_>
where
    P: Path + ?Sized,
    IP: ImplPath + ?Sized,
    I: Identifier + ?Sized,
    G: GenericArg + ?Sized + 'a,
    T: Type + ?Sized,
    GS: IntoIterator<Item = &'a G>,
{
    type Result<'b>
        = fmt::Result
    where
        Self: 'b;

    fn visit_crate_root(&mut self, identifier: &'a I) -> Self::Result<'_> {
        self.formatter.write_str(identifier.name())?;

        let disambiguator = identifier.disambiguator();

        if matches!(self.style, Style::Long) && disambiguator != 0 {
            self.formatter.write_char('[')?;
            LowerHex::fmt(&disambiguator, self.formatter)?;
            self.formatter.write_char(']')?;
        }

        Ok(())
    }

    fn visit_inherent_impl(&mut self, _: &'a IP, r#type: &'a T) -> Self::Result<'_> {
        self.formatter.write_char('<')?;
        display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;
        self.formatter.write_char('>')
    }

    fn visit_trait_impl(&mut self, _: &'a IP, r#type: &'a T, r#trait: &'a P) -> Self::Result<'_> {
        self.formatter.write_char('<')?;
        display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;
        self.formatter.write_str(" as ")?;
        display_path(r#trait, self.style, self.bound_lifetime_depth, false).fmt(self.formatter)?;
        self.formatter.write_char('>')
    }

    fn visit_trait_definition(&mut self, r#type: &'a T, r#trait: &'a P) -> Self::Result<'_> {
        self.formatter.write_char('<')?;
        display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;
        self.formatter.write_str(" as ")?;
        display_path(r#trait, self.style, self.bound_lifetime_depth, false).fmt(self.formatter)?;
        self.formatter.write_char('>')
    }

    fn visit_nested(&mut self, namespace: u8, parent: &'a P, identifier: &'a I) -> Self::Result<'_> {
        match namespace {
            b'A'..=b'Z' => {
                display_path(parent, self.style, self.bound_lifetime_depth, self.in_value).fmt(self.formatter)?;

                self.formatter.write_str("::{")?;

                match namespace {
                    b'C' => self.formatter.write_str("closure")?,
                    b'S' => self.formatter.write_str("shim")?,
                    _ => self.formatter.write_char(char::from(namespace))?,
                }

                let identifier_name = identifier.name();

                if !identifier_name.is_empty() {
                    self.formatter.write_str(":")?;
                    self.formatter.write_str(identifier_name)?;
                }

                self.formatter.write_char('#')?;
                write!(self.formatter, "{}", identifier.disambiguator())?;
                self.formatter.write_char('}')
            }
            b'a'..=b'z' => {
                struct ShouldDisplayParent;

                impl<'a, P, IP, I, G, T, GS> PathVisitor<'a, P, IP, I, G, T, GS> for ShouldDisplayParent
                where
                    P: Path + ?Sized,
                    IP: ImplPath + ?Sized,
                    I: Identifier + ?Sized,
                    G: GenericArg + ?Sized + 'a,
                    T: Type + ?Sized,
                    GS: IntoIterator<Item = &'a G>,
                {
                    type Result<'b>
                        = bool
                    where
                        Self: 'b;

                    fn visit_crate_root(&mut self, _: &'a I) -> Self::Result<'_> {
                        false
                    }

                    fn visit_inherent_impl(&mut self, _: &'a IP, _: &'a T) -> Self::Result<'_> {
                        true
                    }

                    fn visit_trait_impl(&mut self, _: &'a IP, _: &'a T, _: &'a P) -> Self::Result<'_> {
                        true
                    }

                    fn visit_trait_definition(&mut self, _: &'a T, _: &'a P) -> Self::Result<'_> {
                        true
                    }

                    fn visit_nested(&mut self, _: u8, _: &'a P, _: &'a I) -> Self::Result<'_> {
                        false
                    }

                    fn visit_generic(&mut self, _: &'a P, _: GS) -> Self::Result<'_> {
                        true
                    }
                }

                let identifier_name = identifier.name();

                let should_display_parent =
                    matches!(self.style, Style::Normal | Style::Long) || parent.visit(&mut ShouldDisplayParent);

                if identifier_name.is_empty() || should_display_parent {
                    display_path(parent, self.style, self.bound_lifetime_depth, self.in_value).fmt(self.formatter)?;
                }

                if identifier_name.is_empty() {
                    Ok(())
                } else {
                    if should_display_parent {
                        self.formatter.write_str("::")?;
                    }

                    self.formatter.write_str(identifier_name)
                }
            }
            _ => Err(fmt::Error),
        }
    }

    fn visit_generic(&mut self, path: &'a P, generic_args: GS) -> Self::Result<'_> {
        display_path(path, self.style, self.bound_lifetime_depth, self.in_value).fmt(self.formatter)?;

        if self.in_value {
            self.formatter.write_str("::")?;
        }

        self.formatter.write_char('<')?;

        let mut generic_args_iter = generic_args.into_iter();

        if let Some(generic_arg) = generic_args_iter.next() {
            display_generic_arg(generic_arg, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;

            generic_args_iter.try_for_each(|generic_arg| {
                self.formatter.write_str(", ")?;
                display_generic_arg(generic_arg, self.style, self.bound_lifetime_depth).fmt(self.formatter)
            })?;
        }

        self.formatter.write_char('>')
    }
}

pub fn display_path(
    path: &(impl Path + ?Sized),
    style: Style,
    bound_lifetime_depth: u64,
    in_value: bool,
) -> impl Display {
    fmt::from_fn(move |f| {
        path.visit(&mut DisplayPathVisitor {
            style,
            bound_lifetime_depth,
            in_value,
            formatter: f,
        })
    })
}

fn display_lifetime(lifetime: u64, bound_lifetime_depth: u64) -> impl Display {
    fmt_tools::fmt_fn(move |f| {
        f.write_char('\'')?;

        if lifetime == 0 {
            f.write_char('_')
        } else if let Some(depth) = bound_lifetime_depth.checked_sub(lifetime) {
            if depth < 26 {
                f.write_char(char::from(b'a' + u8::try_from(depth).unwrap()))
            } else {
                f.write_char('_')?;
                write!(f, "{depth}")
            }
        } else {
            Err(fmt::Error)
        }
    })
}

pub fn display_generic_arg(
    generic_arg: &(impl GenericArg + ?Sized),
    style: Style,
    bound_lifetime_depth: u64,
) -> impl Display {
    struct Visitor<'a, 'b> {
        style: Style,
        bound_lifetime_depth: u64,
        formatter: &'a mut Formatter<'b>,
    }

    impl<'a, T, C> GenericArgVisitor<'a, T, C> for Visitor<'_, '_>
    where
        T: Type + ?Sized,
        C: Const + ?Sized,
    {
        type Result<'b>
            = fmt::Result
        where
            Self: 'b;

        fn visit_lifetime(&mut self, lifetime: u64) -> Self::Result<'_> {
            display_lifetime(lifetime, self.bound_lifetime_depth).fmt(self.formatter)
        }

        fn visit_type(&mut self, r#type: &'a T) -> Self::Result<'_> {
            display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)
        }

        fn visit_const(&mut self, value: &'a C) -> Self::Result<'_> {
            display_const(value, self.style, self.bound_lifetime_depth, false).fmt(self.formatter)
        }
    }

    fmt_tools::fmt_fn(move |f| {
        generic_arg.visit(&mut Visitor {
            style,
            bound_lifetime_depth,
            formatter: f,
        })
    })
}

fn display_binder(bound_lifetimes: u64, bound_lifetime_depth: u64) -> impl Display {
    fmt_tools::fmt_fn(move |f| {
        f.write_str("for<")?;

        Display::fmt(
            &fmt_tools::fmt_separated_display_list(
                || {
                    (1..=bound_lifetimes)
                        .rev()
                        .map(|i| display_lifetime(i, bound_lifetime_depth + bound_lifetimes))
                },
                ", ",
            ),
            f,
        )?;

        f.write_char('>')
    })
}

pub fn display_type(r#type: &(impl Type + ?Sized), style: Style, bound_lifetime_depth: u64) -> impl Display {
    struct Visitor<'a, 'b> {
        style: Style,
        bound_lifetime_depth: u64,
        formatter: &'a mut Formatter<'b>,
    }

    impl<'a, P, T, B, F, D, C> TypeVisitor<'a, P, T, B, F, D, C> for Visitor<'_, '_>
    where
        P: Path + ?Sized,
        T: Type + ?Sized + 'a,
        B: BasicType + ?Sized,
        F: FnSig + ?Sized,
        D: DynBounds + ?Sized,
        C: Const + ?Sized,
    {
        type Result<'b>
            = fmt::Result
        where
            Self: 'b;

        fn visit_basic(&mut self, basic_type: &'a B) -> Self::Result<'_> {
            display_basic_type(basic_type).fmt(self.formatter)
        }

        fn visit_named(&mut self, path: &'a P) -> Self::Result<'_> {
            display_path(path, self.style, self.bound_lifetime_depth, false).fmt(self.formatter)
        }

        fn visit_array(&mut self, r#type: &'a T, length: &'a C) -> Self::Result<'_> {
            self.formatter.write_char('[')?;
            display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;
            self.formatter.write_str("; ")?;
            display_const(length, self.style, self.bound_lifetime_depth, true).fmt(self.formatter)?;
            self.formatter.write_char(']')
        }

        fn visit_slice(&mut self, r#type: &'a T) -> Self::Result<'_> {
            self.formatter.write_char('[')?;
            display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;
            self.formatter.write_char(']')
        }

        fn visit_tuple(&mut self, types: impl IntoIterator<Item = &'a T>) -> Self::Result<'_> {
            self.formatter.write_char('(')?;

            let mut types_iter = types.into_iter();

            if let Some(r#type) = types_iter.next() {
                display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;

                if let Some(r#type) = types_iter.next() {
                    self.formatter.write_str(", ")?;
                    display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;

                    types_iter.try_for_each(|r#type| {
                        self.formatter.write_str(", ")?;
                        display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)
                    })?;
                } else {
                    self.formatter.write_char(',')?;
                }
            }

            self.formatter.write_char(')')
        }

        fn visit_ref(&mut self, lifetime: u64, r#type: &'a T) -> Self::Result<'_> {
            self.formatter.write_char('&')?;

            if lifetime != 0 {
                display_lifetime(lifetime, self.bound_lifetime_depth).fmt(self.formatter)?;
                self.formatter.write_char(' ')?;
            }

            display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)
        }

        fn visit_ref_mut(&mut self, lifetime: u64, r#type: &'a T) -> Self::Result<'_> {
            self.formatter.write_char('&')?;

            if lifetime != 0 {
                display_lifetime(lifetime, self.bound_lifetime_depth).fmt(self.formatter)?;
                self.formatter.write_char(' ')?;
            }

            self.formatter.write_str("mut ")?;
            display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)
        }

        fn visit_ptr_const(&mut self, r#type: &'a T) -> Self::Result<'_> {
            self.formatter.write_str("*const ")?;
            display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)
        }

        fn visit_ptr_mut(&mut self, r#type: &'a T) -> Self::Result<'_> {
            self.formatter.write_str("*mut ")?;
            display_type(r#type, self.style, self.bound_lifetime_depth).fmt(self.formatter)
        }

        fn visit_fn(&mut self, fn_sig: &'a F) -> Self::Result<'_> {
            display_fn_sig(fn_sig, self.style, self.bound_lifetime_depth).fmt(self.formatter)
        }

        fn visit_dyn_trait(&mut self, dyn_bounds: &'a D, lifetime: u64) -> Self::Result<'_> {
            display_dyn_bounds(dyn_bounds, self.style, self.bound_lifetime_depth).fmt(self.formatter)?;

            if lifetime == 0 {
                Ok(())
            } else {
                self.formatter.write_str(" + ")?;
                display_lifetime(lifetime, self.bound_lifetime_depth).fmt(self.formatter)
            }
        }
    }

    fmt_tools::fmt_fn(move |f| {
        r#type.visit(&mut Visitor {
            style,
            bound_lifetime_depth,
            formatter: f,
        })
    })
}

pub fn display_basic_type(basic_type: &(impl BasicType + ?Sized)) -> impl Display {
    struct Visitor;

    impl BasicTypeVisitor for Visitor {
        type Result<'b>
            = &'static str
        where
            Self: 'b;

        fn visit_i8(&mut self) -> Self::Result<'_> {
            "i8"
        }

        fn visit_bool(&mut self) -> Self::Result<'_> {
            "bool"
        }

        fn visit_char(&mut self) -> Self::Result<'_> {
            "char"
        }

        fn visit_f64(&mut self) -> Self::Result<'_> {
            "f64"
        }

        fn visit_str(&mut self) -> Self::Result<'_> {
            "str"
        }

        fn visit_f32(&mut self) -> Self::Result<'_> {
            "f32"
        }

        fn visit_u8(&mut self) -> Self::Result<'_> {
            "u8"
        }

        fn visit_isize(&mut self) -> Self::Result<'_> {
            "isize"
        }

        fn visit_usize(&mut self) -> Self::Result<'_> {
            "usize"
        }

        fn visit_i32(&mut self) -> Self::Result<'_> {
            "i32"
        }

        fn visit_u32(&mut self) -> Self::Result<'_> {
            "u32"
        }

        fn visit_i128(&mut self) -> Self::Result<'_> {
            "i128"
        }

        fn visit_u128(&mut self) -> Self::Result<'_> {
            "u128"
        }

        fn visit_i16(&mut self) -> Self::Result<'_> {
            "i16"
        }

        fn visit_u16(&mut self) -> Self::Result<'_> {
            "u16"
        }

        fn visit_unit(&mut self) -> Self::Result<'_> {
            "()"
        }

        fn visit_ellipsis(&mut self) -> Self::Result<'_> {
            "..."
        }

        fn visit_i64(&mut self) -> Self::Result<'_> {
            "i64"
        }

        fn visit_u64(&mut self) -> Self::Result<'_> {
            "u64"
        }

        fn visit_never(&mut self) -> Self::Result<'_> {
            "!"
        }

        fn visit_placeholder(&mut self) -> Self::Result<'_> {
            "_"
        }
    }

    fmt_tools::fmt_fn(move |f| f.write_str(basic_type.visit(&mut Visitor)))
}

pub fn display_fn_sig(fn_sig: &(impl FnSig + ?Sized), style: Style, bound_lifetime_depth: u64) -> impl Display {
    struct IsUnitType;

    impl<'a, P, T, B, F, D, C> TypeVisitor<'a, P, T, B, F, D, C> for IsUnitType
    where
        P: ?Sized,
        T: ?Sized + 'a,
        B: BasicType + ?Sized,
        F: ?Sized,
        D: ?Sized,
        C: ?Sized,
    {
        type Result<'b>
            = bool
        where
            Self: 'b;

        fn visit_basic(&mut self, basic_type: &'a B) -> Self::Result<'_> {
            struct IsUnitType;

            impl BasicTypeVisitor for IsUnitType {
                type Result<'b>
                    = bool
                where
                    Self: 'b;

                fn visit_i8(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_bool(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_char(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_f64(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_str(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_f32(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_u8(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_isize(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_usize(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_i32(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_u32(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_i128(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_u128(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_i16(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_u16(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_unit(&mut self) -> Self::Result<'_> {
                    true
                }

                fn visit_ellipsis(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_i64(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_u64(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_never(&mut self) -> Self::Result<'_> {
                    false
                }

                fn visit_placeholder(&mut self) -> Self::Result<'_> {
                    false
                }
            }

            basic_type.visit(&mut IsUnitType)
        }

        fn visit_named(&mut self, _: &'a P) -> Self::Result<'_> {
            false
        }

        fn visit_array(&mut self, _: &'a T, _: &'a C) -> Self::Result<'_> {
            false
        }

        fn visit_slice(&mut self, _: &'a T) -> Self::Result<'_> {
            false
        }

        fn visit_tuple(&mut self, _: impl IntoIterator<Item = &'a T>) -> Self::Result<'_> {
            false
        }

        fn visit_ref(&mut self, _: u64, _: &'a T) -> Self::Result<'_> {
            false
        }

        fn visit_ref_mut(&mut self, _: u64, _: &'a T) -> Self::Result<'_> {
            false
        }

        fn visit_ptr_const(&mut self, _: &'a T) -> Self::Result<'_> {
            false
        }

        fn visit_ptr_mut(&mut self, _: &'a T) -> Self::Result<'_> {
            false
        }

        fn visit_fn(&mut self, _: &'a F) -> Self::Result<'_> {
            false
        }

        fn visit_dyn_trait(&mut self, _: &'a D, _: u64) -> Self::Result<'_> {
            false
        }
    }

    fmt_tools::fmt_fn(move |f| {
        let bound_lifetimes = fn_sig.bound_lifetimes();

        if bound_lifetimes != 0 {
            display_binder(bound_lifetimes, bound_lifetime_depth).fmt(f)?;
            f.write_char(' ')?;
        }

        let bound_lifetime_depth = bound_lifetime_depth + bound_lifetimes;

        if fn_sig.is_unsafe() {
            f.write_str("unsafe ")?;
        }

        if let Some(abi) = fn_sig.abi() {
            f.write_str("extern ")?;
            display_abi(abi).fmt(f)?;
            f.write_char(' ')?;
        }

        f.write_str("fn(")?;

        Display::fmt(
            &fmt_tools::fmt_separated_display_list(
                || {
                    fn_sig
                        .argument_types()
                        .into_iter()
                        .map(|r#type| display_type(r#type, style, bound_lifetime_depth))
                },
                ", ",
            ),
            f,
        )?;

        f.write_char(')')?;

        let fn_sig_return_type = fn_sig.return_type();

        if fn_sig_return_type.visit(&mut IsUnitType) {
            Ok(())
        } else {
            f.write_str(" -> ")?;
            display_type(fn_sig_return_type, style, bound_lifetime_depth).fmt(f)
        }
    })
}

fn display_abi(abi: &(impl Abi + ?Sized)) -> impl Display {
    fmt_tools::fmt_fn(move |f| {
        f.write_char('"')?;

        let mut iter = abi.name().split('_');

        f.write_str(iter.next().unwrap())?;

        for item in iter {
            f.write_char('-')?;
            f.write_str(item)?;
        }

        f.write_char('"')
    })
}

fn display_dyn_bounds(dyn_bounds: &(impl DynBounds + ?Sized), style: Style, bound_lifetime_depth: u64) -> impl Display {
    fmt_tools::fmt_fn(move |f| {
        f.write_str("dyn ")?;

        let dyn_bounds_bound_lifetimes = dyn_bounds.bound_lifetimes();

        if dyn_bounds_bound_lifetimes != 0 {
            display_binder(dyn_bounds_bound_lifetimes, bound_lifetime_depth).fmt(f)?;
            f.write_char(' ')?;
        }

        let bound_lifetime_depth = bound_lifetime_depth + dyn_bounds_bound_lifetimes;

        Display::fmt(
            &fmt_tools::fmt_separated_display_list(
                || {
                    dyn_bounds
                        .dyn_traits()
                        .into_iter()
                        .map(move |dyn_trait| display_dyn_trait(dyn_trait, style, bound_lifetime_depth))
                },
                " + ",
            ),
            f,
        )
    })
}

fn display_dyn_trait(dyn_trait: &(impl DynTrait + ?Sized), style: Style, bound_lifetime_depth: u64) -> impl Display {
    struct GetParent;

    impl<'a, P, IP, I, G, T, GS> PathVisitor<'a, P, IP, I, G, T, GS> for GetParent
    where
        P: Path + ?Sized + 'a,
        IP: ImplPath + ?Sized,
        I: Identifier + ?Sized,
        G: GenericArg + ?Sized + 'a,
        T: Type + ?Sized,
        GS: IntoIterator<Item = &'a G>,
    {
        type Result<'b>
            = Option<(&'a P, GS)>
        where
            Self: 'b;

        fn visit_crate_root(&mut self, _: &'a I) -> Self::Result<'_> {
            None
        }

        fn visit_inherent_impl(&mut self, _: &'a IP, _: &'a T) -> Self::Result<'_> {
            None
        }

        fn visit_trait_impl(&mut self, _: &'a IP, _: &'a T, _: &'a P) -> Self::Result<'_> {
            None
        }

        fn visit_trait_definition(&mut self, _: &'a T, _: &'a P) -> Self::Result<'_> {
            None
        }

        fn visit_nested(&mut self, _: u8, _: &'a P, _: &'a I) -> Self::Result<'_> {
            None
        }

        fn visit_generic(&mut self, path: &'a P, generic_args: GS) -> Self::Result<'_> {
            Some((path, generic_args))
        }
    }

    fmt_tools::fmt_fn(move |f| {
        let path = dyn_trait.path();
        let mut assoc_bindings_iter = dyn_trait.assoc_bindings().into_iter();

        let mut opened = if let Some((parent, generic_args)) = path.visit(&mut GetParent) {
            display_path(parent, style, bound_lifetime_depth, false).fmt(f)?;

            let mut generic_args_iter = generic_args.into_iter();

            if let Some(generic_arg) = generic_args_iter.next() {
                f.write_char('<')?;
                display_generic_arg(generic_arg, style, bound_lifetime_depth).fmt(f)?;

                generic_args_iter.try_for_each(|generic_arg| {
                    f.write_str(", ")?;
                    display_generic_arg(generic_arg, style, bound_lifetime_depth).fmt(f)
                })?;

                true
            } else {
                false
            }
        } else {
            display_path(path, style, bound_lifetime_depth, false).fmt(f)?;

            false
        };

        if let Some((name, r#type)) = assoc_bindings_iter.next() {
            if opened {
                f.write_str(", ")
            } else {
                opened = true;
                f.write_char('<')
            }?;

            display_dyn_trait_assoc_binding(name, r#type, style, bound_lifetime_depth).fmt(f)?;

            assoc_bindings_iter.try_for_each(|(name, r#type)| {
                f.write_str(", ")?;
                display_dyn_trait_assoc_binding(name, r#type, style, bound_lifetime_depth).fmt(f)
            })?;
        }

        if opened { f.write_char('>') } else { Ok(()) }
    })
}

fn display_dyn_trait_assoc_binding(
    name: &str,
    r#type: &(impl Type + ?Sized),
    style: Style,
    bound_lifetime_depth: u64,
) -> impl Display {
    fmt_tools::fmt_fn(move |f| {
        f.write_str(name)?;
        f.write_str(" = ")?;
        display_type(r#type, style, bound_lifetime_depth).fmt(f)
    })
}

fn write_integer<T>(f: &mut Formatter, value: T, style: Style) -> fmt::Result
where
    T: Display,
{
    write!(f, "{value}")?;

    if matches!(style, Style::Long) {
        f.write_str(any::type_name::<T>())
    } else {
        Ok(())
    }
}

fn wrap_with_braces_if_needed(
    in_value: bool,
    formatter: &mut Formatter,
    f: impl FnOnce(&mut Formatter) -> fmt::Result,
) -> fmt::Result {
    if in_value {
        f(formatter)
    } else {
        formatter.write_char('{')?;
        f(formatter)?;
        formatter.write_char('}')
    }
}

pub fn display_const(
    value: &(impl Const + ?Sized),
    style: Style,
    bound_lifetime_depth: u64,
    in_value: bool,
) -> impl Display {
    struct Visitor<'a, 'b> {
        style: Style,
        bound_lifetime_depth: u64,
        in_value: bool,
        formatter: &'a mut Formatter<'b>,
    }

    impl<'a, P, C, CF> ConstVisitor<'a, P, C, CF> for Visitor<'_, '_>
    where
        P: Path + ?Sized,
        C: Const + ?Sized + 'a,
        CF: ConstFields + ?Sized,
    {
        type Result<'b>
            = fmt::Result
        where
            Self: 'b;

        fn visit_i8(&mut self, value: i8) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_u8(&mut self, value: u8) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_isize(&mut self, value: isize) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_usize(&mut self, value: usize) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_i32(&mut self, value: i32) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_u32(&mut self, value: u32) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_i128(&mut self, value: i128) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_u128(&mut self, value: u128) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_i16(&mut self, value: i16) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_u16(&mut self, value: u16) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_i64(&mut self, value: i64) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_u64(&mut self, value: u64) -> Self::Result<'_> {
            write_integer(self.formatter, value, self.style)
        }

        fn visit_bool(&mut self, value: bool) -> Self::Result<'_> {
            Debug::fmt(&value, self.formatter)
        }

        fn visit_char(&mut self, value: char) -> Self::Result<'_> {
            Debug::fmt(&value, self.formatter)
        }

        fn visit_str(&mut self, value: &'a str) -> Self::Result<'_> {
            wrap_with_braces_if_needed(self.in_value, self.formatter, |f| {
                f.write_char('*')?;
                Debug::fmt(value, f)
            })
        }

        fn visit_ref(&mut self, value: &'a C) -> Self::Result<'_> {
            struct GetStr;

            impl<'a, P, C, CF> ConstVisitor<'a, P, C, CF> for GetStr
            where
                P: ?Sized,
                C: ?Sized + 'a,
                CF: ?Sized,
            {
                type Result<'b>
                    = Option<&'a str>
                where
                    Self: 'b;

                fn visit_i8(&mut self, _: i8) -> Self::Result<'_> {
                    None
                }

                fn visit_u8(&mut self, _: u8) -> Self::Result<'_> {
                    None
                }

                fn visit_isize(&mut self, _: isize) -> Self::Result<'_> {
                    None
                }

                fn visit_usize(&mut self, _: usize) -> Self::Result<'_> {
                    None
                }

                fn visit_i32(&mut self, _: i32) -> Self::Result<'_> {
                    None
                }

                fn visit_u32(&mut self, _: u32) -> Self::Result<'_> {
                    None
                }

                fn visit_i128(&mut self, _: i128) -> Self::Result<'_> {
                    None
                }

                fn visit_u128(&mut self, _: u128) -> Self::Result<'_> {
                    None
                }

                fn visit_i16(&mut self, _: i16) -> Self::Result<'_> {
                    None
                }

                fn visit_u16(&mut self, _: u16) -> Self::Result<'_> {
                    None
                }

                fn visit_i64(&mut self, _: i64) -> Self::Result<'_> {
                    None
                }

                fn visit_u64(&mut self, _: u64) -> Self::Result<'_> {
                    None
                }

                fn visit_bool(&mut self, _: bool) -> Self::Result<'_> {
                    None
                }

                fn visit_char(&mut self, _: char) -> Self::Result<'_> {
                    None
                }

                fn visit_str(&mut self, value: &'a str) -> Self::Result<'_> {
                    Some(value)
                }

                fn visit_ref(&mut self, _: &'a C) -> Self::Result<'_> {
                    None
                }

                fn visit_ref_mut(&mut self, _: &'a C) -> Self::Result<'_> {
                    None
                }

                fn visit_array(&mut self, _: impl IntoIterator<Item = &'a C>) -> Self::Result<'_> {
                    None
                }

                fn visit_tuple(&mut self, _: impl IntoIterator<Item = &'a C>) -> Self::Result<'_> {
                    None
                }

                fn visit_named_struct(&mut self, _: &'a P, _: &'a CF) -> Self::Result<'_> {
                    None
                }

                fn visit_placeholder(&mut self) -> Self::Result<'_> {
                    None
                }
            }

            if let Some(value) = value.visit(&mut GetStr) {
                Debug::fmt(value, self.formatter)
            } else {
                wrap_with_braces_if_needed(self.in_value, self.formatter, |f| {
                    f.write_char('&')?;
                    Display::fmt(&display_const(value, self.style, self.bound_lifetime_depth, true), f)
                })
            }
        }

        fn visit_ref_mut(&mut self, value: &'a C) -> Self::Result<'_> {
            wrap_with_braces_if_needed(self.in_value, self.formatter, |f| {
                f.write_str("&mut ")?;
                Display::fmt(&display_const(value, self.style, self.bound_lifetime_depth, true), f)
            })
        }

        fn visit_array(&mut self, values: impl IntoIterator<Item = &'a C>) -> Self::Result<'_> {
            wrap_with_braces_if_needed(self.in_value, self.formatter, |f| {
                f.write_char('[')?;

                let mut iter = values.into_iter();

                if let Some(value) = iter.next() {
                    display_const(value, self.style, self.bound_lifetime_depth, true).fmt(f)?;

                    iter.try_for_each(|value| {
                        f.write_str(", ")?;
                        display_const(value, self.style, self.bound_lifetime_depth, true).fmt(f)
                    })?;
                }

                f.write_char(']')
            })
        }

        fn visit_tuple(&mut self, values: impl IntoIterator<Item = &'a C>) -> Self::Result<'_> {
            wrap_with_braces_if_needed(self.in_value, self.formatter, |f| {
                f.write_char('(')?;

                let mut iter = values.into_iter();

                if let Some(value) = iter.next() {
                    display_const(value, self.style, self.bound_lifetime_depth, true).fmt(f)?;

                    if let Some(value) = iter.next() {
                        f.write_str(", ")?;
                        display_const(value, self.style, self.bound_lifetime_depth, true).fmt(f)?;

                        iter.try_for_each(|value| {
                            f.write_str(", ")?;
                            display_const(value, self.style, self.bound_lifetime_depth, true).fmt(f)
                        })
                    } else {
                        f.write_char(',')
                    }?;
                }

                f.write_char(')')
            })
        }

        fn visit_named_struct(&mut self, path: &'a P, fields: &'a CF) -> Self::Result<'_> {
            wrap_with_braces_if_needed(self.in_value, self.formatter, |f| {
                display_path(path, self.style, self.bound_lifetime_depth, true).fmt(f)?;
                display_const_fields(fields, self.style, self.bound_lifetime_depth).fmt(f)
            })
        }

        fn visit_placeholder(&mut self) -> Self::Result<'_> {
            self.formatter.write_char('_')
        }
    }

    fmt_tools::fmt_fn(move |f| {
        value.visit(&mut Visitor {
            style,
            bound_lifetime_depth,
            in_value,
            formatter: f,
        })
    })
}

fn display_const_fields(fields: &(impl ConstFields + ?Sized), style: Style, bound_lifetime_depth: u64) -> impl Display {
    struct Visitor<'a, 'b> {
        style: Style,
        bound_lifetime_depth: u64,
        formatter: &'a mut Formatter<'b>,
    }

    impl<'a, I, C> ConstFieldsVisitor<'a, I, C> for Visitor<'_, '_>
    where
        I: Identifier + ?Sized + 'a,
        C: Const + ?Sized + 'a,
    {
        type Result<'b>
            = fmt::Result
        where
            Self: 'b;

        fn visit_unit(&mut self) -> Self::Result<'_> {
            Ok(())
        }

        fn visit_tuple(&mut self, fields: impl IntoIterator<Item = &'a C>) -> Self::Result<'_> {
            self.formatter.write_char('(')?;

            let mut iter = fields.into_iter();

            if let Some(value) = iter.next() {
                display_const(value, self.style, self.bound_lifetime_depth, true).fmt(self.formatter)?;

                iter.try_for_each(|value| {
                    self.formatter.write_str(", ")?;
                    display_const(value, self.style, self.bound_lifetime_depth, true).fmt(self.formatter)
                })?;
            }

            self.formatter.write_char(')')
        }

        fn visit_struct(&mut self, fields: impl IntoIterator<Item = (&'a I, &'a C)>) -> Self::Result<'_> {
            let mut iter = fields.into_iter();

            let suffix = if let Some((name, value)) = iter.next() {
                self.formatter.write_str(" { ")?;
                self.formatter.write_str(name.name())?;
                self.formatter.write_str(": ")?;
                display_const(value, self.style, self.bound_lifetime_depth, true).fmt(self.formatter)?;

                iter.try_for_each(|(name, value)| {
                    self.formatter.write_str(", ")?;
                    self.formatter.write_str(name.name())?;
                    self.formatter.write_str(": ")?;
                    display_const(value, self.style, self.bound_lifetime_depth, true).fmt(self.formatter)
                })?;

                " }"
            } else {
                // Matches the behavior of `rustc-demangle`.
                " {  }"
            };

            self.formatter.write_str(suffix)
        }
    }

    fmt_tools::fmt_fn(move |f| {
        fields.visit(&mut Visitor {
            style,
            bound_lifetime_depth,
            formatter: f,
        })
    })
}

#[cfg(test)]
mod tests {
    use super::Style;
    use crate::rust_v0::ast::unsync::Symbol;
    use std::fmt::Write;

    #[test]
    fn test_display_path() {
        let test_cases = [(
            "_RINvCsd5QWgxammnl_7example3fooNcNtINtNtCs454gRYH7d6L_4core6result6ResultllE2Ok0EB2_",
            (
                "foo::<Result<i32, i32>::Ok>",
                "example::foo::<core::result::Result<i32, i32>::Ok>",
                "example[9884ce86676676d1]::foo::<core[2f8af133219d6c11]::result::Result<i32, i32>::Ok>",
            ),
        )];

        let mut buffer = String::new();

        for (symbol, expected) in test_cases {
            let symbol = Symbol::parse_from_str(symbol).unwrap().0;

            write!(buffer, "{}", symbol.display(Style::Short)).unwrap();

            let length_1 = buffer.len();

            write!(buffer, "{}", symbol.display(Style::Normal)).unwrap();

            let length_2 = buffer.len();

            write!(buffer, "{}", symbol.display(Style::Long)).unwrap();

            assert_eq!(
                (&buffer[..length_1], &buffer[length_1..length_2], &buffer[length_2..]),
                expected
            );

            buffer.clear();
        }
    }

    #[test]
    fn test_display_lifetime() {
        #[track_caller]
        fn check(lifetime: u64, bound_lifetime_depth: u64, expected: &str) {
            assert_eq!(
                super::display_lifetime(lifetime, bound_lifetime_depth).to_string(),
                expected
            );
        }

        check(0, 0, "'_");
        check(0, 1, "'_");
        check(0, 2, "'_");

        check(1, 1, "'a");
        check(1, 2, "'b");
        check(1, 3, "'c");

        check(2, 2, "'a");
        check(2, 3, "'b");
        check(2, 4, "'c");
    }

    #[test]
    fn test_display_binder() {
        #[track_caller]
        fn check(bound_lifetimes: u64, bound_lifetime_depth: u64, expected: &str) {
            assert_eq!(
                super::display_binder(bound_lifetimes, bound_lifetime_depth).to_string(),
                expected
            );
        }

        check(0, 0, "for<>");
        check(0, 1, "for<>");
        check(0, 2, "for<>");

        check(1, 0, "for<'a>");
        check(1, 1, "for<'b>");
        check(1, 2, "for<'c>");

        check(2, 0, "for<'a, 'b>");
        check(2, 1, "for<'b, 'c>");
        check(2, 2, "for<'c, 'd>");
    }
}
