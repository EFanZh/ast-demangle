//! Pretty printing demangled symbol names.

use crate::rust_v0::{
    Abi, BasicType, Const, ConstFields, DynBounds, DynTrait, DynTraitAssocBinding, FnSig, GenericArg, Path, Type,
    UndisambiguatedIdentifier,
};
use std::any;
use std::cell::Cell;
use std::fmt::{self, Display, Formatter, Write};

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

fn display_fn(f: impl Fn(&mut Formatter) -> fmt::Result) -> impl Display {
    struct Wrapper<F>(F);

    impl<F: Fn(&mut Formatter) -> fmt::Result> Display for Wrapper<F> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            self.0(f)
        }
    }

    Wrapper(f)
}

fn display_separated_list(values: impl IntoIterator<Item = impl Display>, separator: impl Display) -> impl Display {
    let values = Cell::new(Some(values));

    display_fn(move |f| {
        let mut iter = values.take().unwrap().into_iter();

        if let Some(first) = iter.next() {
            first.fmt(f)?;

            for value in iter {
                separator.fmt(f)?;
                value.fmt(f)?;
            }
        }

        Ok(())
    })
}

pub fn display_path<'a>(path: &'a Path, style: Style, bound_lifetime_depth: u64, in_value: bool) -> impl Display + 'a {
    display_fn(move |f| match path {
        Path::CrateRoot(name) => match style {
            Style::Short | Style::Normal => write!(f, "{}", display_undisambiguated_identifier(&name.name)),
            Style::Long => {
                write!(
                    f,
                    "{}[{:x}]",
                    &display_undisambiguated_identifier(&name.name),
                    name.disambiguator
                )
            }
        },
        Path::InherentImpl { type_, .. } => {
            write!(f, "<{}>", display_type(type_, style, bound_lifetime_depth))
        }
        Path::TraitImpl { type_, trait_, .. } | Path::TraitDefinition { type_, trait_ } => {
            write!(
                f,
                "<{} as {}>",
                display_type(type_, style, bound_lifetime_depth),
                display_path(trait_, style, bound_lifetime_depth, false)
            )
        }
        Path::Nested { namespace, path, name } => match namespace {
            b'A'..=b'Z' => {
                display_path(path, style, bound_lifetime_depth, in_value).fmt(f)?;

                f.write_str("::{")?;

                match namespace {
                    b'C' => f.write_str("closure")?,
                    b'S' => f.write_str("shim")?,
                    _ => f.write_char(char::from(*namespace))?,
                }

                if !name.name.is_empty() {
                    write!(f, ":{}", display_undisambiguated_identifier(&name.name))?;
                }

                write!(f, "#{}}}", name.disambiguator)
            }
            b'a'..=b'z' => {
                if matches!(style, Style::Normal | Style::Long)
                    || matches!(
                        path.as_ref(),
                        Path::InherentImpl { .. }
                            | Path::TraitImpl { .. }
                            | Path::TraitDefinition { .. }
                            | Path::Generic { .. }
                    )
                {
                    display_path(path, style, bound_lifetime_depth, in_value).fmt(f)?;

                    if name.name.is_empty() {
                        Ok(())
                    } else {
                        write!(f, "::{}", display_undisambiguated_identifier(&name.name))
                    }
                } else {
                    write!(f, "{}", display_undisambiguated_identifier(&name.name))
                }
            }
            _ => Err(fmt::Error),
        },
        Path::Generic { path, generic_args } => {
            display_path(path, style, bound_lifetime_depth, in_value).fmt(f)?;

            if in_value {
                f.write_str("::")?;
            }

            write!(
                f,
                "<{}>",
                display_separated_list(
                    generic_args.iter().map(|generic_arg| display_generic_arg(
                        generic_arg,
                        style,
                        bound_lifetime_depth,
                    )),
                    ", "
                )
            )
        }
    })
}

pub fn display_undisambiguated_identifier<'a>(
    undisambiguated_identifier: &'a UndisambiguatedIdentifier,
) -> impl Display + 'a {
    display_fn(move |f| match undisambiguated_identifier {
        UndisambiguatedIdentifier::String(name) => f.write_str(name),
        UndisambiguatedIdentifier::PunyCode(name) => {
            f.write_str("punycode{")?;

            if let Some(i) = name.rfind('_') {
                if i != 0 {
                    f.write_str(&name[..i])?;
                    f.write_char('-')?;
                }

                f.write_str(&name[i + 1..])?;
            } else {
                f.write_str(name)?;
            }

            f.write_char('}')
        }
    })
}

fn display_lifetime(lifetime: u64, bound_lifetime_depth: u64) -> impl Display {
    display_fn(move |f| {
        f.write_char('\'')?;

        if lifetime == 0 {
            f.write_char('_')
        } else if let Some(depth) = bound_lifetime_depth.checked_sub(lifetime) {
            if depth < 26 {
                f.write_char(char::from(b'a' + u8::try_from(depth).unwrap()))
            } else {
                write!(f, "_{}", depth)
            }
        } else {
            f.write_str("{invalid syntax}")
        }
    })
}

pub fn display_generic_arg<'a>(
    generic_arg: &'a GenericArg,
    style: Style,
    bound_lifetime_depth: u64,
) -> impl Display + 'a {
    display_fn(move |f| match generic_arg {
        GenericArg::Lifetime(lifetime) => display_lifetime(*lifetime, bound_lifetime_depth).fmt(f),
        GenericArg::Type(type_) => display_type(type_, style, bound_lifetime_depth).fmt(f),
        GenericArg::Const(const_) => display_const(const_, style, bound_lifetime_depth, false).fmt(f),
    })
}

fn display_binder(bound_lifetimes: u64, bound_lifetime_depth: u64) -> impl Display {
    display_fn(move |f| {
        write!(
            f,
            "for<{}>",
            display_separated_list(
                (1..=bound_lifetimes)
                    .rev()
                    .map(|i| display_lifetime(i, bound_lifetime_depth + bound_lifetimes)),
                ", "
            )
        )
    })
}

pub fn display_type<'a>(type_: &'a Type, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    display_fn(move |f| match type_ {
        Type::Basic(basic_type) => display_basic_type(*basic_type).fmt(f),
        Type::Named(path) => display_path(path, style, bound_lifetime_depth, false).fmt(f),
        Type::Array(type_, length) => {
            write!(
                f,
                "[{}; {}]",
                display_type(type_, style, bound_lifetime_depth),
                display_const(length, style, bound_lifetime_depth, true)
            )
        }
        Type::Slice(type_) => {
            write!(f, "[{}]", display_type(type_, style, bound_lifetime_depth))
        }
        Type::Tuple(tuple_types) => {
            write!(
                f,
                "({}",
                display_separated_list(
                    tuple_types
                        .iter()
                        .map(|type_| { display_type(type_, style, bound_lifetime_depth) }),
                    ", "
                )
            )?;

            if tuple_types.len() == 1 {
                f.write_char(',')?;
            }

            f.write_char(')')
        }
        Type::Ref { lifetime, type_ } => {
            f.write_char('&')?;

            if *lifetime != 0 {
                write!(f, "{} ", display_lifetime(*lifetime, bound_lifetime_depth))?;
            }

            display_type(type_, style, bound_lifetime_depth).fmt(f)
        }
        Type::RefMut { lifetime, type_ } => {
            f.write_char('&')?;

            if *lifetime != 0 {
                write!(f, "{} ", display_lifetime(*lifetime, bound_lifetime_depth))?;
            }

            write!(f, "mut {}", display_type(type_, style, bound_lifetime_depth))
        }
        Type::PtrConst(type_) => {
            write!(f, "*const {}", display_type(type_, style, bound_lifetime_depth))
        }
        Type::PtrMut(type_) => {
            write!(f, "*mut {}", display_type(type_, style, bound_lifetime_depth))
        }
        Type::Fn(fn_sig) => display_fn_sig(fn_sig, style, bound_lifetime_depth).fmt(f),
        Type::DynTrait { dyn_bounds, lifetime } => {
            display_dyn_bounds(dyn_bounds, style, bound_lifetime_depth).fmt(f)?;

            if *lifetime == 0 {
                Ok(())
            } else {
                write!(f, " + {}", display_lifetime(*lifetime, bound_lifetime_depth))
            }
        }
    })
}

pub fn display_basic_type(basic_type: BasicType) -> impl Display {
    display_fn(move |f| {
        f.write_str(match basic_type {
            BasicType::I8 => "i8",
            BasicType::Bool => "bool",
            BasicType::Char => "char",
            BasicType::F64 => "f64",
            BasicType::Str => "str",
            BasicType::F32 => "f32",
            BasicType::U8 => "u8",
            BasicType::Isize => "isize",
            BasicType::Usize => "usize",
            BasicType::I32 => "i32",
            BasicType::U32 => "u32",
            BasicType::I128 => "i128",
            BasicType::U128 => "u128",
            BasicType::I16 => "i16",
            BasicType::U16 => "u16",
            BasicType::Unit => "()",
            BasicType::Ellipsis => "...",
            BasicType::I64 => "i64",
            BasicType::U64 => "u64",
            BasicType::Never => "!",
            BasicType::Placeholder => "_",
        })
    })
}

pub fn display_fn_sig<'a>(fn_sig: &'a FnSig, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    display_fn(move |f| {
        if fn_sig.bound_lifetimes != 0 {
            write!(f, "{} ", display_binder(fn_sig.bound_lifetimes, bound_lifetime_depth))?;
        }

        let bound_lifetime_depth = bound_lifetime_depth + fn_sig.bound_lifetimes;

        if fn_sig.is_unsafe {
            f.write_str("unsafe ")?;
        }

        if let Some(abi) = &fn_sig.abi {
            write!(f, "extern {} ", display_abi(abi))?;
        }

        write!(
            f,
            "fn({})",
            display_separated_list(
                fn_sig
                    .argument_types
                    .iter()
                    .map(|type_| { display_type(type_, style, bound_lifetime_depth) }),
                ", "
            )
        )?;

        if let Type::Basic(BasicType::Unit) = fn_sig.return_type.as_ref() {
            Ok(())
        } else {
            write!(
                f,
                " -> {}",
                display_type(&fn_sig.return_type, style, bound_lifetime_depth)
            )
        }
    })
}

fn display_abi<'a>(abi: &'a Abi) -> impl Display + 'a {
    display_fn(move |f| {
        f.write_char('"')?;

        match abi {
            Abi::C => f.write_char('C')?,
            Abi::Named(name) => match name {
                UndisambiguatedIdentifier::String(name) => {
                    let mut iter = name.split('_');

                    f.write_str(iter.next().unwrap())?;

                    for item in iter {
                        write!(f, "-{}", item)?;
                    }
                }
                UndisambiguatedIdentifier::PunyCode(_) => return Err(fmt::Error),
            },
        }

        f.write_char('"')
    })
}

fn display_dyn_bounds<'a>(dyn_bounds: &'a DynBounds, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    display_fn(move |f| {
        f.write_str("dyn ")?;

        if dyn_bounds.bound_lifetimes != 0 {
            write!(
                f,
                "{} ",
                display_binder(dyn_bounds.bound_lifetimes, bound_lifetime_depth)
            )?;
        }

        let bound_lifetime_depth = bound_lifetime_depth + dyn_bounds.bound_lifetimes;

        display_separated_list(
            dyn_bounds
                .dyn_traits
                .iter()
                .map(move |dyn_trait| display_dyn_trait(dyn_trait, style, bound_lifetime_depth)),
            " + ",
        )
        .fmt(f)
    })
}

fn display_dyn_trait<'a>(dyn_trait: &'a DynTrait, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    display_fn(move |f| {
        if dyn_trait.dyn_trait_assoc_bindings.is_empty() {
            display_path(&dyn_trait.path, style, bound_lifetime_depth, false).fmt(f)
        } else if let Path::Generic { path, generic_args } = dyn_trait.path.as_ref() {
            write!(
                f,
                "{}<{}>",
                display_path(path, style, bound_lifetime_depth, false),
                display_separated_list(
                    generic_args
                        .iter()
                        .map(Ok)
                        .chain(dyn_trait.dyn_trait_assoc_bindings.iter().map(Err))
                        .map(|value| {
                            display_fn(move |f| match value {
                                Ok(generic_arg) => display_generic_arg(generic_arg, style, bound_lifetime_depth).fmt(f),
                                Err(dyn_trait_assoc_binding) => display_dyn_trait_assoc_binding(
                                    dyn_trait_assoc_binding,
                                    style,
                                    bound_lifetime_depth,
                                )
                                .fmt(f),
                            })
                        }),
                    ", "
                )
            )
        } else {
            write!(
                f,
                "{}<{}>",
                display_path(&dyn_trait.path, style, bound_lifetime_depth, false),
                display_separated_list(
                    dyn_trait
                        .dyn_trait_assoc_bindings
                        .iter()
                        .map(|dyn_trait_assoc_binding| {
                            display_dyn_trait_assoc_binding(dyn_trait_assoc_binding, style, bound_lifetime_depth)
                        }),
                    ", "
                )
            )
        }
    })
}

fn display_dyn_trait_assoc_binding<'a>(
    dyn_trait_assoc_binding: &'a DynTraitAssocBinding,
    style: Style,
    bound_lifetime_depth: u64,
) -> impl Display + 'a {
    display_fn(move |f| {
        write!(
            f,
            "{} = {}",
            display_undisambiguated_identifier(&dyn_trait_assoc_binding.name),
            display_type(&dyn_trait_assoc_binding.type_, style, bound_lifetime_depth)
        )
    })
}

fn write_integer<T: Display>(f: &mut Formatter, value: T, style: Style) -> fmt::Result {
    write!(f, "{}", value)?;

    if matches!(style, Style::Long) {
        f.write_str(any::type_name::<T>())
    } else {
        Ok(())
    }
}

pub fn display_const<'a>(
    const_: &'a Const,
    style: Style,
    bound_lifetime_depth: u64,
    in_value: bool,
) -> impl Display + 'a {
    display_fn(move |f| match *const_ {
        Const::I8(value) => write_integer(f, value, style),
        Const::U8(value) => write_integer(f, value, style),
        Const::Isize(value) => write_integer(f, value, style),
        Const::Usize(value) => write_integer(f, value, style),
        Const::I32(value) => write_integer(f, value, style),
        Const::U32(value) => write_integer(f, value, style),
        Const::I128(value) => write_integer(f, value, style),
        Const::U128(value) => write_integer(f, value, style),
        Const::I16(value) => write_integer(f, value, style),
        Const::U16(value) => write_integer(f, value, style),
        Const::I64(value) => write_integer(f, value, style),
        Const::U64(value) => write_integer(f, value, style),
        Const::Bool(value) => write!(f, "{}", value),
        Const::Char(value) => write!(f, "{:?}", value),
        Const::Str(ref value) => {
            if in_value {
                write!(f, "*{:?}", value)
            } else {
                write!(f, "{{*{:?}}}", value)
            }
        }
        Const::Ref(ref value) => {
            if let Const::Str(value) = value.as_ref() {
                write!(f, "{:?}", value)
            } else if in_value {
                write!(f, "&{}", display_const(value, style, bound_lifetime_depth, true))
            } else {
                write!(f, "{{&{}}}", display_const(value, style, bound_lifetime_depth, true))
            }
        }
        Const::RefMut(ref value) => {
            let inner = display_const(value, style, bound_lifetime_depth, true);

            if in_value {
                write!(f, "&mut {}", inner)
            } else {
                write!(f, "{{&mut {}}}", inner)
            }
        }
        Const::Array(ref items) => {
            let inner = display_separated_list(
                items
                    .iter()
                    .map(|item| display_const(item, style, bound_lifetime_depth, true)),
                ", ",
            );

            if in_value {
                write!(f, "[{}]", inner)
            } else {
                write!(f, "{{[{}]}}", inner)
            }
        }
        Const::Tuple(ref items) => {
            let inner = display_separated_list(
                items
                    .iter()
                    .map(|item| display_const(item, style, bound_lifetime_depth, true)),
                ", ",
            );

            if in_value {
                write!(f, "({}", inner)?;

                f.write_str(if items.len() == 1 { ",)" } else { ")" })
            } else {
                write!(f, "{{({}", inner)?;

                f.write_str(if items.len() == 1 { ",)}" } else { ")}" })
            }
        }
        Const::NamedStruct { ref path, ref fields } => {
            let path = display_path(path, style, bound_lifetime_depth, true);
            let fields = display_const_fields(fields, style, bound_lifetime_depth);

            if in_value {
                write!(f, "{}{}", path, fields)
            } else {
                write!(f, "{{{}{}}}", path, fields)
            }
        }
        Const::Placeholder => write!(f, "_"),
    })
}

fn display_const_fields<'a>(fields: &'a ConstFields, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    display_fn(move |f| match fields {
        ConstFields::Unit => Ok(()),
        ConstFields::Tuple(fields) => write!(
            f,
            "({})",
            display_separated_list(
                fields
                    .iter()
                    .map(|field| display_const(field, style, bound_lifetime_depth, true)),
                ", "
            )
        ),
        ConstFields::Struct(fields) => {
            if fields.is_empty() {
                // Matches the behavior of `rustc-demangle`.
                write!(f, " {{  }}")
            } else {
                write!(
                    f,
                    " {{ {} }}",
                    display_separated_list(
                        fields.iter().map(|(name, value)| {
                            display_fn(move |f| {
                                write!(
                                    f,
                                    "{}: {}",
                                    name,
                                    display_const(value, style, bound_lifetime_depth, true)
                                )
                            })
                        }),
                        ", "
                    )
                )
            }
        }
    })
}

#[cfg(test)]
mod tests {
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
