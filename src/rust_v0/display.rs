use crate::rust_v0::{Abi, BasicType, Const, DynBounds, DynTrait, DynTraitAssocBinding, FnSig, GenericArg, Path, Type};
use std::cell::Cell;
use std::convert::TryFrom;
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

pub(super) fn display_path<'a>(
    path: &'a Path,
    style: Style,
    bound_lifetime_depth: u64,
    in_value: bool,
) -> impl Display + 'a {
    display_fn(move |f| match path {
        Path::CrateRoot(name) => match style {
            Style::Short | Style::Normal => f.write_str(&name.name),
            Style::Long => {
                write!(f, "{}[{:x}]", &name.name, name.disambiguator)
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
                    write!(f, ":{}", name.name)?;
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
                        write!(f, "::{}", name.name)
                    }
                } else {
                    name.name.fmt(f)
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
                    generic_args
                        .iter()
                        .map(|generic_arg| { display_generic_arg(generic_arg, style, bound_lifetime_depth) }),
                    ", "
                )
            )
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
            Err(fmt::Error)
        }
    })
}

pub(super) fn display_generic_arg<'a>(
    generic_arg: &'a GenericArg,
    style: Style,
    bound_lifetime_depth: u64,
) -> impl Display + 'a {
    display_fn(move |f| match generic_arg {
        GenericArg::Lifetime(lifetime) => display_lifetime(*lifetime, bound_lifetime_depth).fmt(f),
        GenericArg::Type(type_) => display_type(type_, style, bound_lifetime_depth).fmt(f),
        GenericArg::Const(const_) => display_const(const_, style, bound_lifetime_depth).fmt(f),
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

pub(super) fn display_type<'a>(type_: &'a Type, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    display_fn(move |f| match type_ {
        Type::Basic(basic_type) => display_basic_type(*basic_type).fmt(f),
        Type::Named(path) => display_path(path, style, bound_lifetime_depth, false).fmt(f),
        Type::Array(type_, length) => {
            write!(
                f,
                "[{}; {}]",
                display_type(type_, style, bound_lifetime_depth),
                display_const(length, style, bound_lifetime_depth)
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

pub(super) fn display_basic_type(basic_type: BasicType) -> impl Display {
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

pub(super) fn display_fn_sig<'a>(fn_sig: &'a FnSig, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
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
            Abi::Named(name) => {
                let mut iter = name.split('_');

                f.write_str(iter.next().unwrap())?;

                for item in iter {
                    write!(f, "-{}", item)?;
                }
            }
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
            dyn_trait_assoc_binding.name,
            display_type(&dyn_trait_assoc_binding.type_, style, bound_lifetime_depth)
        )
    })
}

fn display_u64(num_str: &str) -> impl Display + '_ {
    display_fn(move |f| {
        if let Ok(num) = u64::from_str_radix(num_str, 16) {
            write!(f, "{}", num)
        } else {
            write!(f, "0x{}", num_str)
        }
    })
}

pub(super) fn display_const<'a>(const_: &'a Const, style: Style, bound_lifetime_depth: u64) -> impl Display + 'a {
    display_fn(move |f| match const_ {
        Const::Data { type_, data } => {
            if let Type::Basic(basic_type) = type_.as_ref() {
                match basic_type {
                    BasicType::I8
                    | BasicType::I16
                    | BasicType::I32
                    | BasicType::I64
                    | BasicType::I128
                    | BasicType::Isize => {
                        if let Some(num) = data.strip_prefix("n") {
                            write!(f, "-{}", display_u64(num))
                        } else {
                            display_u64(data).fmt(f)
                        }
                    }
                    BasicType::U8
                    | BasicType::U16
                    | BasicType::U32
                    | BasicType::U64
                    | BasicType::U128
                    | BasicType::Usize => display_u64(data).fmt(f),
                    BasicType::Bool => match *data {
                        "0" => f.write_str("false"),
                        "1" => f.write_str("true"),
                        _ => Err(fmt::Error),
                    },
                    BasicType::Char => {
                        let value = u32::from_str_radix(data, 16).map_err(|_| fmt::Error)?;
                        let c = char::try_from(value).map_err(|_| fmt::Error)?;

                        fmt::Debug::fmt(&c, f)
                    }
                    _ => Err(fmt::Error),
                }?;

                if matches!(style, Style::Long if *basic_type != BasicType::Bool && *basic_type != BasicType::Char) {
                    display_type(type_, style, bound_lifetime_depth).fmt(f)
                } else {
                    Ok(())
                }
            } else {
                Err(fmt::Error)
            }
        }
        Const::Placeholder => f.write_char('_'),
    })
}

#[cfg(test)]
mod tests {
    use crate::rust_v0::Symbol;

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

    #[track_caller]
    fn check_with_rustc_demangle(symbol: &str) {
        let actual = Symbol::parse_from_str(symbol).unwrap().0;
        let expected = rustc_demangle::demangle(symbol);

        assert_eq!(actual.to_string(), expected.to_string());
        assert_eq!(format!("{:#}", actual), format!("{:#}", expected));
    }

    #[test]
    fn test_rustc_demangle_crate_with_leading_digitame() {
        check_with_rustc_demangle("_RNvC6_123foo3bar");
    }

    #[test]
    fn test_rustc_demangle_utf8_idents() {
        check_with_rustc_demangle("_RNqCs4fqI2P2rA04_11utf8_identsu30____7hkackfecea1cbdathfdh9hlq6y");
    }

    #[test]
    fn test_rustc_demangle_closure() {
        check_with_rustc_demangle("_RNCNCNgCs6DXkGYLi8lr_2cc5spawn00B5_");

        check_with_rustc_demangle(
            "_RNCINkXs25_NgCsbmNqQUJIY6D_4core5sliceINyB9_4IterhENuNgNoBb_4iter8iterator8Iterator9rpositionNCNgNpB9_6memchr7memrchrs_0E0Bb_"
        );
    }

    #[test]
    fn test_rustc_demangle_dyn_trait() {
        check_with_rustc_demangle(
            "_RINbNbCskIICzLVDPPb_5alloc5alloc8box_freeDINbNiB4_5boxed5FnBoxuEp6OutputuEL_ECs1iopQbuBiw2_3std",
        );
    }

    #[test]
    fn test_rustc_demangle_const_generics() {
        check_with_rustc_demangle("_RINtC8arrayvec8ArrayVechKj7b_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_8UnsignedKhb_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_6SignedKs98_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_6SignedKanb_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4BoolKb0_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4BoolKb1_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4CharKc76_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4CharKca_E");
        check_with_rustc_demangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4CharKc2202_E");
        check_with_rustc_demangle("_RNvNvMCs4fqI2P2rA04_13const_genericINtB4_3FooKpE3foo3FOO");
    }

    #[test]
    fn test_rustc_demangle_exponential_explosion() {
        check_with_rustc_demangle("_RMC0TTTTTTpB8_EB7_EB6_EB5_EB4_EB3_E");
    }

    #[test]
    fn test_rustc_demangle_thinlto() {
        check_with_rustc_demangle("_RC3foo.llvm.9D1C9369");
        check_with_rustc_demangle("_RC3foo.llvm.9D1C9369@@16");
        check_with_rustc_demangle("_RNvC9backtrace3foo.llvm.A5310EB9");
    }

    #[test]
    fn test_rustc_demangle_misc() {
        check_with_rustc_demangle("_RINvYQDNtNtNtNtCs29lbbC0kHcQ_4core4iter6traits8iterator8Iteratorp4ItemTRRNtNtCsf3SUqAoUaAd_12rustc_middle2ty3TySRbNtNtNtCshFshXKPbts9_18rustc_query_system9dep_graph5graph12DepNodeIndexEEL_B5_4foldjNCNvYB3_B30_5count0ECs7ORiaTkfWmE_16rustc_query_impl");
        check_with_rustc_demangle("_RINvYINtNtNtNtCs29lbbC0kHcQ_4core4iter8adapters3map3MapINtCs2U14P4XHdAm_8smallvec8IntoIterANtNtCs6u4OS1VUqkU_9rustc_hir3hir6ItemIdj1_ENCNvMs_Cs4itKs8KOMEp_18rustc_ast_loweringNtB2f_15LoweringContext10lower_stmt0ENtNtNtBa_6traits8iterator8Iterator7collectINtBU_8SmallVecANtB1t_4StmtB24_EEB2f_");
    }
}
