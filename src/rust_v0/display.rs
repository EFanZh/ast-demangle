//! Pretty printing demangled symbol names.

use crate::rust_v0::{
    Abi, BasicType, Const, ConstFields, DynBounds, DynTrait, DynTraitAssocBinding, FnSig, GenericArg, Path, Type,
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

pub fn display_path<'a>(path: &'a Path, style: Style, bound_lifetime_depth: u64, in_value: bool) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| match path {
        Path::CrateRoot(identifier) => {
            f.write_str(&identifier.name)?;

            if matches!(style, Style::Long) && identifier.disambiguator != 0 {
                f.write_char('[')?;
                LowerHex::fmt(&identifier.disambiguator, f)?;
                f.write_char(']')?;
            }

            Ok(())
        }
        Path::InherentImpl { type_, .. } => {
            f.write_char('<')?;
            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)?;
            f.write_char('>')
        }
        Path::TraitImpl { type_, trait_, .. } | Path::TraitDefinition { type_, trait_ } => {
            f.write_char('<')?;
            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)?;
            f.write_str(" as ")?;
            Display::fmt(&display_path(trait_, style, bound_lifetime_depth, false), f)?;
            f.write_char('>')
        }
        Path::Nested {
            namespace,
            path,
            identifier,
        } => match namespace {
            b'A'..=b'Z' => {
                Display::fmt(&display_path(path, style, bound_lifetime_depth, in_value), f)?;

                f.write_str("::{")?;

                match namespace {
                    b'C' => f.write_str("closure")?,
                    b'S' => f.write_str("shim")?,
                    _ => f.write_char(char::from(*namespace))?,
                }

                if !identifier.name.is_empty() {
                    f.write_str(":")?;
                    f.write_str(&identifier.name)?;
                }

                f.write_char('#')?;
                Display::fmt(&identifier.disambiguator, f)?;
                f.write_char('}')
            }
            b'a'..=b'z' => {
                if matches!(style, Style::Normal | Style::Long)
                    || matches!(
                        **path,
                        Path::InherentImpl { .. }
                            | Path::TraitImpl { .. }
                            | Path::TraitDefinition { .. }
                            | Path::Generic { .. }
                    )
                {
                    Display::fmt(&display_path(path, style, bound_lifetime_depth, in_value), f)?;

                    if identifier.name.is_empty() {
                        Ok(())
                    } else {
                        f.write_str("::")?;
                        f.write_str(&identifier.name)
                    }
                } else if identifier.name.is_empty() {
                    Display::fmt(&display_path(path, style, bound_lifetime_depth, in_value), f)
                } else {
                    f.write_str(&identifier.name)
                }
            }
            _ => Err(fmt::Error),
        },
        Path::Generic { path, generic_args } => {
            Display::fmt(&display_path(path, style, bound_lifetime_depth, in_value), f)?;

            if in_value {
                f.write_str("::")?;
            }

            f.write_char('<')?;

            Display::fmt(
                &fmt_tools::fmt_separated_display_list(
                    || {
                        generic_args
                            .iter()
                            .map(|generic_arg| display_generic_arg(generic_arg, style, bound_lifetime_depth))
                    },
                    ", ",
                ),
                f,
            )?;

            f.write_char('>')
        }
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
                Display::fmt(&depth, f)
            }
        } else {
            Err(fmt::Error)
        }
    })
}

pub fn display_generic_arg<'a>(
    generic_arg: &'a GenericArg,
    style: Style,
    bound_lifetime_depth: u64,
) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| match generic_arg {
        GenericArg::Lifetime(lifetime) => display_lifetime(*lifetime, bound_lifetime_depth).fmt(f),
        GenericArg::Type(type_) => display_type(type_, style, bound_lifetime_depth).fmt(f),
        GenericArg::Const(const_) => display_const(const_, style, bound_lifetime_depth, false).fmt(f),
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

pub fn display_type<'a>(type_: &'a Type, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| match type_ {
        Type::Basic(basic_type) => Display::fmt(&display_basic_type(*basic_type), f),
        Type::Named(path) => Display::fmt(&display_path(path, style, bound_lifetime_depth, false), f),
        Type::Array(type_, length) => {
            f.write_char('[')?;
            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)?;
            f.write_str("; ")?;
            Display::fmt(&display_const(length, style, bound_lifetime_depth, true), f)?;
            f.write_char(']')
        }
        Type::Slice(type_) => {
            f.write_char('[')?;
            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)?;
            f.write_char(']')
        }
        Type::Tuple(tuple_types) => {
            f.write_char('(')?;

            Display::fmt(
                &fmt_tools::fmt_separated_display_list(
                    || {
                        tuple_types
                            .iter()
                            .map(|type_| display_type(type_, style, bound_lifetime_depth))
                    },
                    ", ",
                ),
                f,
            )?;

            if tuple_types.len() == 1 {
                f.write_char(',')?;
            }

            f.write_char(')')
        }
        Type::Ref { lifetime, type_ } => {
            f.write_char('&')?;

            if *lifetime != 0 {
                Display::fmt(&display_lifetime(*lifetime, bound_lifetime_depth), f)?;
                f.write_char(' ')?;
            }

            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)
        }
        Type::RefMut { lifetime, type_ } => {
            f.write_char('&')?;

            if *lifetime != 0 {
                Display::fmt(&display_lifetime(*lifetime, bound_lifetime_depth), f)?;
                f.write_char(' ')?;
            }

            f.write_str("mut ")?;
            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)
        }
        Type::PtrConst(type_) => {
            f.write_str("*const ")?;
            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)
        }
        Type::PtrMut(type_) => {
            f.write_str("*mut ")?;
            Display::fmt(&display_type(type_, style, bound_lifetime_depth), f)
        }
        Type::Fn(fn_sig) => Display::fmt(&display_fn_sig(fn_sig, style, bound_lifetime_depth), f),
        Type::DynTrait { dyn_bounds, lifetime } => {
            Display::fmt(&display_dyn_bounds(dyn_bounds, style, bound_lifetime_depth), f)?;

            if *lifetime == 0 {
                Ok(())
            } else {
                f.write_str(" + ")?;
                Display::fmt(&display_lifetime(*lifetime, bound_lifetime_depth), f)
            }
        }
    })
}

pub fn display_basic_type(basic_type: BasicType) -> impl Display {
    fmt_tools::fmt_fn(move |f| {
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
    fmt_tools::fmt_fn(move |f| {
        if fn_sig.bound_lifetimes != 0 {
            Display::fmt(&display_binder(fn_sig.bound_lifetimes, bound_lifetime_depth), f)?;
            f.write_char(' ')?;
        }

        let bound_lifetime_depth = bound_lifetime_depth + fn_sig.bound_lifetimes;

        if fn_sig.is_unsafe {
            f.write_str("unsafe ")?;
        }

        if let Some(abi) = &fn_sig.abi {
            f.write_str("extern ")?;
            Display::fmt(&display_abi(abi), f)?;
            f.write_char(' ')?;
        }

        f.write_str("fn(")?;

        Display::fmt(
            &fmt_tools::fmt_separated_display_list(
                || {
                    fn_sig
                        .argument_types
                        .iter()
                        .map(|type_| display_type(type_, style, bound_lifetime_depth))
                },
                ", ",
            ),
            f,
        )?;

        f.write_char(')')?;

        if matches!(fn_sig.return_type.as_ref(), Type::Basic(BasicType::Unit)) {
            Ok(())
        } else {
            f.write_str(" -> ")?;
            Display::fmt(&display_type(&fn_sig.return_type, style, bound_lifetime_depth), f)
        }
    })
}

fn display_abi<'a>(abi: &'a Abi) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| {
        f.write_char('"')?;

        match abi {
            Abi::C => f.write_char('C')?,
            Abi::Named(name) => {
                let mut iter = name.split('_');

                f.write_str(iter.next().unwrap())?;

                for item in iter {
                    f.write_char('-')?;
                    f.write_str(item)?;
                }
            }
        }

        f.write_char('"')
    })
}

fn display_dyn_bounds<'a>(dyn_bounds: &'a DynBounds, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| {
        f.write_str("dyn ")?;

        if dyn_bounds.bound_lifetimes != 0 {
            Display::fmt(&display_binder(dyn_bounds.bound_lifetimes, bound_lifetime_depth), f)?;
            f.write_char(' ')?;
        }

        let bound_lifetime_depth = bound_lifetime_depth + dyn_bounds.bound_lifetimes;

        Display::fmt(
            &fmt_tools::fmt_separated_display_list(
                || {
                    dyn_bounds
                        .dyn_traits
                        .iter()
                        .map(move |dyn_trait| display_dyn_trait(dyn_trait, style, bound_lifetime_depth))
                },
                " + ",
            ),
            f,
        )
    })
}

fn display_dyn_trait<'a>(dyn_trait: &'a DynTrait, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| {
        if dyn_trait.dyn_trait_assoc_bindings.is_empty() {
            Display::fmt(&display_path(&dyn_trait.path, style, bound_lifetime_depth, false), f)
        } else if let Path::Generic { path, generic_args } = dyn_trait.path.as_ref() {
            Display::fmt(&display_path(path, style, bound_lifetime_depth, false), f)?;
            f.write_char('<')?;

            Display::fmt(
                &fmt_tools::fmt_separated_display_list(
                    || {
                        generic_args
                            .iter()
                            .map(Ok)
                            .chain(dyn_trait.dyn_trait_assoc_bindings.iter().map(Err))
                            .map(|value| {
                                fmt_tools::fmt_fn(move |f| match value {
                                    Ok(generic_arg) => {
                                        Display::fmt(&display_generic_arg(generic_arg, style, bound_lifetime_depth), f)
                                    }
                                    Err(dyn_trait_assoc_binding) => Display::fmt(
                                        &display_dyn_trait_assoc_binding(
                                            dyn_trait_assoc_binding,
                                            style,
                                            bound_lifetime_depth,
                                        ),
                                        f,
                                    ),
                                })
                            })
                    },
                    ", ",
                ),
                f,
            )?;

            f.write_char('>')
        } else {
            Display::fmt(&display_path(&dyn_trait.path, style, bound_lifetime_depth, false), f)?;
            f.write_char('<')?;

            Display::fmt(
                &fmt_tools::fmt_separated_display_list(
                    || {
                        dyn_trait
                            .dyn_trait_assoc_bindings
                            .iter()
                            .map(|dyn_trait_assoc_binding| {
                                display_dyn_trait_assoc_binding(dyn_trait_assoc_binding, style, bound_lifetime_depth)
                            })
                    },
                    ", ",
                ),
                f,
            )?;

            f.write_char('>')
        }
    })
}

fn display_dyn_trait_assoc_binding<'a>(
    dyn_trait_assoc_binding: &'a DynTraitAssocBinding,
    style: Style,
    bound_lifetime_depth: u64,
) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| {
        f.write_str(&dyn_trait_assoc_binding.name)?;
        f.write_str(" = ")?;

        Display::fmt(
            &display_type(&dyn_trait_assoc_binding.type_, style, bound_lifetime_depth),
            f,
        )
    })
}

fn write_integer<T: Display>(f: &mut Formatter, value: T, style: Style) -> fmt::Result {
    Display::fmt(&value, f)?;

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

pub fn display_const<'a>(
    const_: &'a Const,
    style: Style,
    bound_lifetime_depth: u64,
    in_value: bool,
) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| match *const_ {
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
        Const::Bool(value) => Display::fmt(&value, f),
        Const::Char(value) => Debug::fmt(&value, f),
        Const::Str(ref value) => wrap_with_braces_if_needed(in_value, f, |f| {
            f.write_char('*')?;
            Debug::fmt(value, f)
        }),
        Const::Ref(ref value) => {
            if let Const::Str(value) = value.as_ref() {
                Debug::fmt(value, f)
            } else {
                wrap_with_braces_if_needed(in_value, f, |f| {
                    f.write_char('&')?;
                    Display::fmt(&display_const(value, style, bound_lifetime_depth, true), f)
                })
            }
        }
        Const::RefMut(ref value) => wrap_with_braces_if_needed(in_value, f, |f| {
            f.write_str("&mut ")?;
            Display::fmt(&display_const(value, style, bound_lifetime_depth, true), f)
        }),
        Const::Array(ref items) => wrap_with_braces_if_needed(in_value, f, |f| {
            f.write_char('[')?;

            Display::fmt(
                &fmt_tools::fmt_separated_display_list(
                    || {
                        items
                            .iter()
                            .map(|item| display_const(item, style, bound_lifetime_depth, true))
                    },
                    ", ",
                ),
                f,
            )?;

            f.write_char(']')
        }),
        Const::Tuple(ref items) => wrap_with_braces_if_needed(in_value, f, |f| {
            f.write_char('(')?;

            Display::fmt(
                &fmt_tools::fmt_separated_display_list(
                    || {
                        items
                            .iter()
                            .map(|item| display_const(item, style, bound_lifetime_depth, true))
                    },
                    ", ",
                ),
                f,
            )?;

            if items.len() == 1 {
                f.write_char(',')?;
            }

            f.write_char(')')
        }),
        Const::NamedStruct { ref path, ref fields } => wrap_with_braces_if_needed(in_value, f, |f| {
            Display::fmt(&display_path(path, style, bound_lifetime_depth, true), f)?;
            Display::fmt(&display_const_fields(fields, style, bound_lifetime_depth), f)
        }),
        Const::Placeholder => f.write_char('_'),
    })
}

fn display_const_fields<'a>(fields: &'a ConstFields, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    fmt_tools::fmt_fn(move |f| match fields {
        ConstFields::Unit => Ok(()),
        ConstFields::Tuple(fields) => {
            f.write_char('(')?;

            Display::fmt(
                &fmt_tools::fmt_separated_display_list(
                    || {
                        fields
                            .iter()
                            .map(|field| display_const(field, style, bound_lifetime_depth, true))
                    },
                    ", ",
                ),
                f,
            )?;

            f.write_char(')')
        }
        ConstFields::Struct(fields) => {
            if fields.is_empty() {
                // Matches the behavior of `rustc-demangle`.
                f.write_str(" {  }")
            } else {
                f.write_str(" { ")?;

                Display::fmt(
                    &fmt_tools::fmt_separated_display_list(
                        || {
                            fields.iter().map(|(name, value)| {
                                fmt_tools::fmt_fn(move |f| {
                                    Display::fmt(name, f)?;
                                    f.write_str(": ")?;
                                    Display::fmt(&display_const(value, style, bound_lifetime_depth, true), f)
                                })
                            })
                        },
                        ", ",
                    ),
                    f,
                )?;

                f.write_str(" }")
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::Style;
    use crate::rust_v0::Symbol;
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
