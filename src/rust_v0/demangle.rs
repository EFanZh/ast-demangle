use crate::rust_v0::{
    Abi, BasicType, Const, DynBounds, DynTrait, DynTraitAssocBinding, FnSig, GenericArg, Path, Symbol, Type,
};
use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter, Write};
use std::str;

fn write_separated_list<T, W: Write>(
    values: impl IntoIterator<Item = T>,
    f: &mut W,
    mut write_value: impl FnMut(T, &mut W) -> fmt::Result,
    separator: &str,
) -> fmt::Result {
    let mut iter = values.into_iter();

    if let Some(first) = iter.next() {
        write_value(first, f)?;

        for value in iter {
            f.write_str(separator)?;
            write_value(value, f)?;
        }
    }

    Ok(())
}

fn write_path(path: &Path, f: &mut Formatter, bound_lifetime_depth: u64, in_value: bool) -> fmt::Result {
    match path {
        Path::CrateRoot(name) => {
            f.write_str(&name.name)?;

            if f.alternate() {
                Ok(())
            } else {
                write!(f, "[{:x}]", name.disambiguator)
            }
        }
        Path::InherentImpl { type_, .. } => {
            f.write_char('<')?;
            write_type(type_, f, bound_lifetime_depth)?;
            f.write_char('>')
        }
        Path::TraitImpl {
            type_, trait_: path, ..
        }
        | Path::TraitDefinition { type_, trait_: path } => {
            f.write_char('<')?;
            write_type(type_, f, bound_lifetime_depth)?;
            f.write_str(" as ")?;
            write_path(path, f, bound_lifetime_depth, false)?;
            f.write_char('>')
        }
        Path::Nested { namespace, path, name } => {
            write_path(path, f, bound_lifetime_depth, in_value)?;

            match namespace {
                b'A'..=b'Z' => {
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
                _ => {
                    if name.name.is_empty() {
                        Ok(())
                    } else {
                        write!(f, "::{}", name.name)
                    }
                }
            }
        }
        Path::Generic { path, generic_args } => {
            write_path(path, f, bound_lifetime_depth, in_value)?;

            if in_value {
                f.write_str("::")?;
            }

            f.write_char('<')?;

            write_separated_list(
                generic_args,
                f,
                |generic_arg, f| write_generic_arg(generic_arg, f, bound_lifetime_depth),
                ", ",
            )?;

            f.write_char('>')
        }
    }
}

fn write_lifetime(lifetime: u64, f: &mut impl Write, bound_lifetime_depth: u64) -> fmt::Result {
    if lifetime == 0 {
        f.write_str("'_")
    } else if let Some(depth) = bound_lifetime_depth.checked_sub(lifetime) {
        if depth < 26 {
            write!(f, "'{}", char::from(b'a' + u8::try_from(depth).unwrap()))
        } else {
            write!(f, "'_{}", depth)
        }
    } else {
        Err(fmt::Error)
    }
}

fn write_generic_arg(generic_arg: &GenericArg, f: &mut Formatter, bound_lifetime_depth: u64) -> fmt::Result {
    match generic_arg {
        GenericArg::Lifetme(lifetime) => write_lifetime(*lifetime, f, bound_lifetime_depth),
        GenericArg::Type(type_) => write_type(type_, f, bound_lifetime_depth),
        GenericArg::Const(const_) => write_const(const_, f, bound_lifetime_depth),
    }
}

fn write_binder(bound_lifetimes: u64, f: &mut impl Write, bound_lifetime_depth: u64) -> fmt::Result {
    if bound_lifetimes == 0 {
        Ok(())
    } else {
        f.write_str("for<")?;

        write_separated_list(
            (1..=bound_lifetimes).rev(),
            f,
            |i, f| write_lifetime(i, f, bound_lifetime_depth + bound_lifetimes),
            ", ",
        )?;

        f.write_str("> ")
    }
}

fn write_type(type_: &Type, f: &mut Formatter, bound_lifetime_depth: u64) -> fmt::Result {
    match type_ {
        Type::Basic(basic_type) => write_basic_type(*basic_type, f),
        Type::Named(path) => write_path(path, f, bound_lifetime_depth, false),
        Type::Array(type_, length) => {
            f.write_char('[')?;
            write_type(type_, f, bound_lifetime_depth)?;
            f.write_str("; ")?;
            write_const(length, f, bound_lifetime_depth)?;
            f.write_char(']')
        }
        Type::Slice(type_) => {
            f.write_char('[')?;
            write_type(type_, f, bound_lifetime_depth)?;
            f.write_char(']')
        }
        Type::Tuple(tuple_types) => {
            f.write_str("(")?;
            write_separated_list(
                tuple_types,
                f,
                |type_, f| write_type(type_, f, bound_lifetime_depth),
                ", ",
            )?;
            f.write_str(")")
        }
        Type::Ref { lifetime, type_ } => {
            f.write_char('&')?;

            if *lifetime != 0 {
                write_lifetime(*lifetime, f, bound_lifetime_depth)?;
                f.write_char(' ')?;
            }

            write_type(type_, f, bound_lifetime_depth)
        }
        Type::RefMut { lifetime, type_ } => {
            f.write_char('&')?;

            if *lifetime != 0 {
                write_lifetime(*lifetime, f, bound_lifetime_depth)?;
                f.write_char(' ')?;
            }

            f.write_str("mut ")?;

            write_type(type_, f, bound_lifetime_depth)
        }
        Type::PtrConst(type_) => {
            f.write_str("*const")?;
            write_type(type_, f, bound_lifetime_depth)
        }
        Type::PtrMut(type_) => {
            f.write_str("*mut")?;
            write_type(type_, f, bound_lifetime_depth)
        }
        Type::Fn(fn_sig) => write_fn_sig(fn_sig, f, bound_lifetime_depth),
        Type::DynTrait { dyn_bounds, lifetime } => {
            write_dyn_bounds(dyn_bounds, f, bound_lifetime_depth)?;

            if *lifetime == 0 {
                Ok(())
            } else {
                f.write_str(" + ")?;
                write_lifetime(dyn_bounds.bound_lifetimes, f, bound_lifetime_depth)
            }
        }
    }
}

fn write_basic_type(basic_type: BasicType, f: &mut Formatter) -> fmt::Result {
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
}

fn write_fn_sig(fn_sig: &FnSig, f: &mut Formatter, bound_lifetime_depth: u64) -> fmt::Result {
    if fn_sig.bound_lifetimes != 0 {
        write_binder(fn_sig.bound_lifetimes, f, bound_lifetime_depth)?;
    }

    if fn_sig.is_unsafe {
        f.write_str("unsafe ")?;
    }

    if let Some(abi) = &fn_sig.abi {
        write_abi(abi, f)?;
        f.write_char(' ')?;
    }

    f.write_str("fn(")?;

    write_separated_list(
        &fn_sig.argument_types,
        f,
        |type_, f| write_type(type_, f, bound_lifetime_depth),
        ", ",
    )?;

    f.write_char(')')?;

    if let Type::Basic(BasicType::Unit) = fn_sig.return_type.as_ref() {
        Ok(())
    } else {
        f.write_str(" -> ")?;
        write_type(&fn_sig.return_type, f, bound_lifetime_depth)
    }
}

fn write_abi(abi: &Abi, f: &mut Formatter) -> fmt::Result {
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
}

fn write_dyn_bounds(dyn_bounds: &DynBounds, f: &mut Formatter, bound_lifetime_depth: u64) -> fmt::Result {
    f.write_str("dyn ")?;

    write_binder(dyn_bounds.bound_lifetimes, f, bound_lifetime_depth)?;

    write_separated_list(
        &dyn_bounds.dyn_traits,
        f,
        |dyn_trait, f| write_dyn_trait(dyn_trait, f, bound_lifetime_depth + dyn_bounds.bound_lifetimes),
        " + ",
    )
}

fn write_dyn_trait(dyn_trait: &DynTrait, f: &mut Formatter, bound_lifetime_depth: u64) -> fmt::Result {
    if let Path::Generic { path, generic_args } = dyn_trait.path.as_ref() {
        write_path(path, f, bound_lifetime_depth, false)?;
        f.write_char('<')?;

        write_separated_list(
            generic_args
                .iter()
                .map(Ok)
                .chain(dyn_trait.dyn_trait_assoc_bindings.iter().map(Err)),
            f,
            |value, f| match value {
                Ok(generic_arg) => write_generic_arg(generic_arg, f, bound_lifetime_depth),
                Err(dyn_trait_assoc_binding) => {
                    write_dyn_trait_assoc_binding(dyn_trait_assoc_binding, f, bound_lifetime_depth)
                }
            },
            ", ",
        )?;
    } else {
        write_path(&dyn_trait.path, f, bound_lifetime_depth, false)?;

        f.write_char('<')?;

        write_separated_list(
            &dyn_trait.dyn_trait_assoc_bindings,
            f,
            |dyn_trait_assoc_binding, f| {
                write_dyn_trait_assoc_binding(dyn_trait_assoc_binding, f, bound_lifetime_depth)
            },
            ", ",
        )?;
    }

    f.write_char('>')
}

fn write_dyn_trait_assoc_binding(
    dyn_trait_assoc_binding: &DynTraitAssocBinding,
    f: &mut Formatter,
    bound_lifetime_depth: u64,
) -> fmt::Result {
    write!(f, "{} = ", dyn_trait_assoc_binding.name)?;

    write_type(&dyn_trait_assoc_binding.type_, f, bound_lifetime_depth)
}

fn write_u64(num_str: &str, f: &mut Formatter) -> fmt::Result {
    if let Ok(num) = u64::from_str_radix(num_str, 16) {
        write!(f, "{}", num)
    } else {
        write!(f, "0x{}", num_str)
    }
}

fn write_const(const_: &Const, f: &mut Formatter, bound_lifetime_depth: u64) -> fmt::Result {
    match const_ {
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
                            f.write_char('-')?;

                            write_u64(num, f)
                        } else {
                            write_u64(data, f)
                        }
                    }
                    BasicType::U8
                    | BasicType::U16
                    | BasicType::U32
                    | BasicType::U64
                    | BasicType::U128
                    | BasicType::Usize => write_u64(data, f),
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

                if f.alternate() {
                    Ok(())
                } else {
                    f.write_str(": ")?;
                    write_type(type_, f, bound_lifetime_depth)
                }
            } else {
                Err(fmt::Error)
            }
        }
        Const::Placeholder => f.write_char('_'),
    }
}

impl<'a> Display for Symbol<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write_path(&self.path, f, 0, true)
    }
}

#[cfg(test)]
mod tests {
    use crate::rust_v0::Symbol;

    #[test]
    fn test_write_lifetime() {
        #[track_caller]
        fn check(lifetime: u64, bound_lifetime_depth: u64, expected: &str) {
            let mut result = String::new();

            super::write_lifetime(lifetime, &mut result, bound_lifetime_depth).unwrap();

            assert_eq!(result, expected);
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
    fn test_write_binder() {
        #[track_caller]
        fn check(bound_lifetimes: u64, bound_lifetime_depth: u64, expected: &str) {
            let mut result = String::new();

            super::write_binder(bound_lifetimes, &mut result, bound_lifetime_depth).unwrap();

            assert_eq!(result, expected);
        }

        check(0, 0, "");
        check(0, 1, "");
        check(0, 2, "");

        check(1, 0, "for<'a> ");
        check(1, 1, "for<'b> ");
        check(1, 2, "for<'c> ");

        check(2, 0, "for<'a, 'b> ");
        check(2, 1, "for<'b, 'c> ");
        check(2, 2, "for<'c, 'd> ");
    }

    #[track_caller]
    fn check_with_rustc_dangle(symbol: &str) {
        let actual = Symbol::parse_from_str(symbol).unwrap();
        let expected = rustc_demangle::demangle(symbol);

        assert_eq!(actual.to_string(), expected.to_string());
        assert_eq!(format!("{:#}", actual), format!("{:#}", expected));
    }

    #[test]
    fn test_rustc_demangle_crate_with_leading_digitame() {
        check_with_rustc_dangle("_RNvC6_123foo3bar");
    }

    #[test]
    fn test_rustc_demangle_utf8_idents() {
        check_with_rustc_dangle("_RNqCs4fqI2P2rA04_11utf8_identsu30____7hkackfecea1cbdathfdh9hlq6y");
    }

    #[test]
    fn test_rustc_demangle_closure() {
        check_with_rustc_dangle("_RNCNCNgCs6DXkGYLi8lr_2cc5spawn00B5_");

        check_with_rustc_dangle(
            "_RNCINkXs25_NgCsbmNqQUJIY6D_4core5sliceINyB9_4IterhENuNgNoBb_4iter8iterator8Iterator9rpositionNCNgNpB9_6memchr7memrchrs_0E0Bb_"
        );
    }

    #[test]
    fn test_rustc_demangle_dyn_trait() {
        check_with_rustc_dangle(
            "_RINbNbCskIICzLVDPPb_5alloc5alloc8box_freeDINbNiB4_5boxed5FnBoxuEp6OutputuEL_ECs1iopQbuBiw2_3std",
        );
    }

    #[test]
    fn test_rustc_demangle_const_generics() {
        check_with_rustc_dangle("_RINtC8arrayvec8ArrayVechKj7b_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_8UnsignedKhb_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_6SignedKs98_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_6SignedKanb_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4BoolKb0_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4BoolKb1_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4CharKc76_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4CharKca_E");
        check_with_rustc_dangle("_RMCs4fqI2P2rA04_13const_genericINtB0_4CharKc2202_E");
        check_with_rustc_dangle("_RNvNvMCs4fqI2P2rA04_13const_genericINtB4_3FooKpE3foo3FOO");
    }

    #[test]
    fn test_rustc_demangle_exponential_explosion() {
        check_with_rustc_dangle("_RMC0TTTTTTpB8_EB7_EB6_EB5_EB4_EB3_E");
    }

    #[test]
    fn test_rustc_demangle_thinlto() {
        check_with_rustc_dangle("_RC3foo.llvm.9D1C9369");
        check_with_rustc_dangle("_RC3foo.llvm.9D1C9369@@16");
        check_with_rustc_dangle("_RNvC9backtrace3foo.llvm.A5310EB9");
    }
}
