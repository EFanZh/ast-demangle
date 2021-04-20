use super::context::Context;
use super::{
    Abi, Base62Number, BasicType, Const, DecimalNumber, DynBounds, DynTrait, DynTraitAssocBinding, GenericArg,
    Identifier, ImplPath, Path, SymbolName, Type, UndisambiguatedIdentifier,
};
use nom::error::{Error, ErrorKind};
use nom::IResult;
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use std::str;

fn make_err<T>(input: &str, error_kind: ErrorKind) -> IResult<&str, T> {
    Err(nom::Err::Error(Error::new(input, error_kind)))
}

fn simplify_result<'a, 'b, T>(result: IResult<Context<'a, 'b>, T>) -> IResult<&'a str, T> {
    result
        .map(|(rest, result)| (str::from_utf8(rest.data).unwrap(), result))
        .map_err(|e| e.map(|e| Error::new(str::from_utf8(e.input.data).unwrap(), e.code)))
}

fn id(name: &str) -> Identifier {
    Identifier {
        disambiguator: 0,
        name: name.into(),
    }
}

#[test]
fn test_parse_undisambiguated_identifier() {
    fn parse(input: &str) -> IResult<&str, Cow<str>> {
        let back_ref_table = RefCell::default();

        simplify_result(UndisambiguatedIdentifier::parse(Context::new(
            input.as_bytes(),
            &back_ref_table,
        )))
    }

    assert_eq!(parse(""), make_err("", ErrorKind::Digit));

    assert_eq!(parse("6_123foo"), Ok(("", "123foo".into())));
    assert_eq!(parse("3bar"), Ok(("", "bar".into())));

    assert_eq!(
        parse("u30____7hkackfecea1cbdathfdh9hlq6y"),
        Ok(("", "საჭმელად_გემრიელი_სადილი".into()))
    );
}

#[test]
fn test_abi() {
    fn parse(input: &str) -> IResult<&str, Abi> {
        let back_ref_table = RefCell::default();

        simplify_result(Abi::parse(Context::new(input.as_bytes(), &back_ref_table)))
    }

    assert_eq!(parse(""), make_err("", ErrorKind::Digit));

    assert_eq!(parse("CDEF"), Ok(("DEF", Abi::C)));
    assert_eq!(parse("3abcdef"), Ok(("def", Abi::Named("abc".into()))));
}

#[test]
fn test_parse_decimal_number() {
    fn parse(input: &str) -> IResult<&str, u64> {
        let back_ref_table = RefCell::default();

        simplify_result(DecimalNumber::parse(Context::new(input.as_bytes(), &back_ref_table)))
    }

    assert_eq!(parse(""), make_err("", ErrorKind::Digit));

    assert_eq!(parse("0"), Ok(("", 0)));
    assert_eq!(parse("7"), Ok(("", 7)));
    assert_eq!(parse("c"), make_err("c", ErrorKind::Digit));

    assert_eq!(parse("00"), Ok(("0", 0)));
    assert_eq!(parse("07"), Ok(("7", 0)));
    assert_eq!(parse("0c"), Ok(("c", 0)));
    assert_eq!(parse("70"), Ok(("", 70)));
    assert_eq!(parse("77"), Ok(("", 77)));
    assert_eq!(parse("7c"), Ok(("c", 7)));
    assert_eq!(parse("c0"), make_err("c0", ErrorKind::Digit));
    assert_eq!(parse("c7"), make_err("c7", ErrorKind::Digit));
    assert_eq!(parse("cc"), make_err("cc", ErrorKind::Digit));

    assert_eq!(
        parse("999999999999999999999999"),
        make_err("999999999999999999999999", ErrorKind::MapOpt)
    );
}

#[test]
fn test_parse_base_62_number() {
    fn parse(input: &str) -> IResult<&str, u64> {
        let back_ref_table = RefCell::default();

        simplify_result(Base62Number::parse(Context::new(input.as_bytes(), &back_ref_table)))
    }

    assert_eq!(parse("_"), Ok(("", 0)));
    assert_eq!(parse("0_"), Ok(("", 1)));
    assert_eq!(parse("7_"), Ok(("", 8)));
    assert_eq!(parse("a_"), Ok(("", 11)));
    assert_eq!(parse("Z_"), Ok(("", 62)));
    assert_eq!(parse("10_"), Ok(("", 63)));
}

fn parse_symbol_name(input: &str) -> IResult<&str, SymbolName> {
    let back_ref_table = RefCell::default();

    simplify_result(SymbolName::parse(Context::new(input.as_bytes(), &back_ref_table)))
}

#[test]
fn test_rustc_demangle_crate_with_leading_digit() {
    assert_eq!(
        parse_symbol_name("NvC6_123foo3bar"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::Nested {
                    namespace: b'v',
                    path: Path::CrateRoot(Identifier {
                        disambiguator: 0,
                        name: "123foo".into(),
                    })
                    .into(),
                    name: Identifier {
                        disambiguator: 0,
                        name: "bar".into(),
                    }
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_utf8_idents() {
    assert_eq!(
        parse_symbol_name("NqCs4fqI2P2rA04_11utf8_identsu30____7hkackfecea1cbdathfdh9hlq6y"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::Nested {
                    namespace: b'q',
                    path: Path::CrateRoot(Identifier {
                        disambiguator: 0x_317d_4810_89b8_c8fe,
                        name: "utf8_idents".into()
                    })
                    .into(),
                    name: Identifier {
                        disambiguator: 0,
                        name: "საჭმელად_გემრიელი_სადილი".into()
                    }
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_closure_1() {
    assert_eq!(
        parse_symbol_name("NCNCNgCs6DXkGYLi8lr_2cc5spawn00B5_"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::Nested {
                    namespace: b'C',
                    path: Path::Nested {
                        namespace: b'C',
                        path: Path::Nested {
                            namespace: b'g',
                            path: Path::CrateRoot(Identifier {
                                disambiguator: 0x_4d64_68d6_c9fd_4bb3,
                                name: "cc".into()
                            })
                            .into(),
                            name: Identifier {
                                disambiguator: 0,
                                name: "spawn".into()
                            }
                        }
                        .into(),
                        name: Identifier {
                            disambiguator: 0,
                            name: "".into()
                        }
                    }
                    .into(),
                    name: Identifier {
                        disambiguator: 0,
                        name: "".into()
                    }
                }
                .into(),
                instantiating_crate: Some(
                    Path::CrateRoot(Identifier {
                        disambiguator: 0x_4d64_68d6_c9fd_4bb3,
                        name: "cc".into()
                    })
                    .into()
                )
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_closure_2() {
    let crate_root = Rc::new(Path::CrateRoot(Identifier {
        disambiguator: 0x_8468_17f7_41e5_4dfd,
        name: "core".into(),
    }));

    let core_slice = Rc::new(Path::Nested {
        namespace: b'g',
        path: Rc::clone(&crate_root),
        name: id("slice"),
    });

    assert_eq!(
        parse_symbol_name(
            "NCINkXs25_NgCsbmNqQUJIY6D_4core5sliceINyB9_4IterhENuNgNoBb_4iter8iterator8Iterator9rpositionNCNgNpB9_6memchr7memrchrs_0E0Bb_"
        ),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::Nested {
                    namespace: b'C',
                    path: Path::GenericArgs {
                        path: Path::Nested {
                            namespace: b'k',
                            path: Path::TraitImpl {
                                impl_path: ImplPath {
                                    disambiguator: 131,
                                    path: Rc::clone(&core_slice)
                                },
                                type_: Type::Named(
                                    Path::GenericArgs {
                                        path: Path::Nested {
                                            namespace: b'y',
                                            path: Rc::clone(&core_slice),
                                            name: id("Iter")
                                        }
                                        .into(),
                                        generic_args: vec![GenericArg::Type(Type::Basic(BasicType::U8).into())]
                                    }
                                    .into()
                                )
                                .into(),
                                trait_: Path::Nested {
                                    namespace: b'u',
                                    path: Path::Nested {
                                        namespace: b'g',
                                        path: Path::Nested {
                                            namespace: b'o',
                                            path: Rc::clone(&crate_root),
                                            name: id("iter")
                                        }
                                        .into(),
                                        name: id("iterator")
                                    }
                                    .into(),
                                    name: Identifier {
                                        disambiguator: 0,
                                        name: "Iterator".into()
                                    }
                                }
                                .into()
                            }
                            .into(),
                            name: id("rposition")
                        }
                        .into(),
                        generic_args: vec![GenericArg::Type(
                            Type::Named(
                                Path::Nested {
                                    namespace: b'C',
                                    path: Path::Nested {
                                        namespace: b'g',
                                        path: Path::Nested {
                                            namespace: b'p',
                                            path: Rc::clone(&core_slice),
                                            name: id("memchr")
                                        }
                                        .into(),
                                        name: id("memrchr")
                                    }
                                    .into(),
                                    name: Identifier {
                                        disambiguator: 1,
                                        name: "".into()
                                    }
                                }
                                .into()
                            )
                            .into()
                        )]
                    }
                    .into(),
                    name: Identifier {
                        disambiguator: 0,
                        name: "".into()
                    }
                }
                .into(),
                instantiating_crate: Some(crate_root)
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_dyn_trait() {
    assert_eq!(
        parse_symbol_name(
            "INbNbCskIICzLVDPPb_5alloc5alloc8box_freeDINbNiB4_5boxed5FnBoxuEp6OutputuEL_ECs1iopQbuBiw2_3std"
        ),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::GenericArgs {
                    path: Path::Nested {
                        namespace: b'b',
                        path: Path::Nested {
                            namespace: b'b',
                            path: Path::CrateRoot(Identifier {
                                disambiguator: 0x_f15a_878b_47eb_696b,
                                name: "alloc".into()
                            })
                            .into(),
                            name: Identifier {
                                disambiguator: 0,
                                name: "alloc".into()
                            }
                        }
                        .into(),
                        name: Identifier {
                            disambiguator: 0,
                            name: "box_free".into()
                        }
                    }
                    .into(),
                    generic_args: vec![GenericArg::Type(
                        Type::DynTrait {
                            dyn_bounds: DynBounds {
                                bound_lifetimes: 0,
                                dyn_traits: vec![DynTrait {
                                    path: Path::GenericArgs {
                                        path: Path::Nested {
                                            namespace: b'b',
                                            path: Path::Nested {
                                                namespace: b'i',
                                                path: Path::CrateRoot(Identifier {
                                                    disambiguator: 0x_f15a_878b_47eb_696b,
                                                    name: "alloc".into()
                                                })
                                                .into(),
                                                name: Identifier {
                                                    disambiguator: 0,
                                                    name: "boxed".into()
                                                }
                                            }
                                            .into(),
                                            name: Identifier {
                                                disambiguator: 0,
                                                name: "FnBox".into()
                                            }
                                        }
                                        .into(),
                                        generic_args: vec![GenericArg::Type(Type::Basic(BasicType::Unit).into())]
                                    }
                                    .into(),
                                    dyn_trait_assoc_bindings: vec![DynTraitAssocBinding {
                                        name: "Output".into(),
                                        type_: Type::Basic(BasicType::Unit).into()
                                    }]
                                }]
                            },
                            lifetime: 0
                        }
                        .into()
                    )]
                }
                .into(),
                instantiating_crate: Some(
                    Path::CrateRoot(Identifier {
                        disambiguator: 0x_0f1a_6958_f46c_38e4,
                        name: "std".into()
                    })
                    .into()
                )
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_usize_123() {
    assert_eq!(
        parse_symbol_name("INtC8arrayvec8ArrayVechKj7b_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::GenericArgs {
                    path: Path::Nested {
                        namespace: b't',
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0,
                            name: "arrayvec".into()
                        })
                        .into(),
                        name: Identifier {
                            disambiguator: 0,
                            name: "ArrayVec".into()
                        }
                    }
                    .into(),
                    generic_args: vec![
                        GenericArg::Type(Type::Basic(BasicType::U8).into()),
                        GenericArg::Const(
                            Const::Data {
                                type_: Type::Basic(BasicType::Usize).into(),
                                data: b"7b"
                            }
                            .into()
                        )
                    ]
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_u8_11() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_8UnsignedKhb_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Unsigned".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::U8).into(),
                                    data: b"b"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_i16_152() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_6SignedKs98_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Signed".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::I16).into(),
                                    data: b"98"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_i8_negative_11() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_6SignedKanb_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Signed".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::I8).into(),
                                    data: b"nb"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_bool_false() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_4BoolKb0_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Bool".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::Bool).into(),
                                    data: b"0"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_bool_true() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_4BoolKb1_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Bool".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::Bool).into(),
                                    data: b"1"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_char_v() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_4CharKc76_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Char".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::Char).into(),
                                    data: b"76"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_char_lf() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_4CharKca_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Char".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::Char).into(),
                                    data: b"a"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_char_partial_differential() {
    assert_eq!(
        parse_symbol_name("MCs4fqI2P2rA04_13const_genericINtB0_4CharKc2202_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0x_317d_4810_89b8_c8fe,
                            name: "const_generic".into()
                        })
                        .into()
                    },
                    type_: Type::Named(
                        Path::GenericArgs {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "Char".into()
                                }
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(
                                Const::Data {
                                    type_: Type::Basic(BasicType::Char).into(),
                                    data: b"2202"
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_placeholder() {
    assert_eq!(
        parse_symbol_name("NvNvMCs4fqI2P2rA04_13const_genericINtB4_3FooKpE3foo3FOO"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::Nested {
                    namespace: b'v',
                    path: Path::Nested {
                        namespace: b'v',
                        path: Path::InherentImpl {
                            impl_path: ImplPath {
                                disambiguator: 0,
                                path: Path::CrateRoot(Identifier {
                                    disambiguator: 0x_317d_4810_89b8_c8fe,
                                    name: "const_generic".into()
                                })
                                .into()
                            },
                            type_: Type::Named(
                                Path::GenericArgs {
                                    path: Path::Nested {
                                        namespace: b't',
                                        path: Path::CrateRoot(Identifier {
                                            disambiguator: 0x_317d_4810_89b8_c8fe,
                                            name: "const_generic".into()
                                        })
                                        .into(),
                                        name: Identifier {
                                            disambiguator: 0,
                                            name: "Foo".into()
                                        }
                                    }
                                    .into(),
                                    generic_args: vec![GenericArg::Const(Const::Placeholder.into())]
                                }
                                .into()
                            )
                            .into()
                        }
                        .into(),
                        name: Identifier {
                            disambiguator: 0,
                            name: "foo".into()
                        }
                    }
                    .into(),
                    name: Identifier {
                        disambiguator: 0,
                        name: "FOO".into()
                    }
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_exponential_explosion() {
    let placeholder = Rc::new(Type::Basic(BasicType::Placeholder));
    let tuple = Rc::new(Type::Tuple(vec![Rc::clone(&placeholder), placeholder]));
    let tuple = Rc::new(Type::Tuple(vec![Rc::clone(&tuple), tuple]));
    let tuple = Rc::new(Type::Tuple(vec![Rc::clone(&tuple), tuple]));
    let tuple = Rc::new(Type::Tuple(vec![Rc::clone(&tuple), tuple]));
    let tuple = Rc::new(Type::Tuple(vec![Rc::clone(&tuple), tuple]));
    let tuple = Rc::new(Type::Tuple(vec![Rc::clone(&tuple), tuple]));

    assert_eq!(
        parse_symbol_name("MC0TTTTTTpB8_EB7_EB6_EB5_EB4_EB3_E"),
        Ok((
            "",
            SymbolName {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        disambiguator: 0,
                        path: Path::CrateRoot(Identifier {
                            disambiguator: 0,
                            name: "".into()
                        })
                        .into()
                    },
                    type_: tuple
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_thinlto() {
    assert_eq!(
        parse_symbol_name("C3foo.llvm.9D1C9369"),
        Ok((
            ".llvm.9D1C9369",
            SymbolName {
                version: None,
                path: Path::CrateRoot(Identifier {
                    disambiguator: 0,
                    name: "foo".into()
                })
                .into(),
                instantiating_crate: None
            }
        ))
    );

    assert_eq!(
        parse_symbol_name("C3foo.llvm.9D1C9369@@16"),
        Ok((
            ".llvm.9D1C9369@@16",
            SymbolName {
                version: None,
                path: Path::CrateRoot(Identifier {
                    disambiguator: 0,
                    name: "foo".into()
                })
                .into(),
                instantiating_crate: None
            }
        ))
    );

    assert_eq!(
        parse_symbol_name("NvC9backtrace3foo.llvm.A5310EB9"),
        Ok((
            ".llvm.A5310EB9",
            SymbolName {
                version: None,
                path: Path::Nested {
                    namespace: b'v',
                    path: Path::CrateRoot(Identifier {
                        disambiguator: 0,
                        name: "backtrace".into()
                    })
                    .into(),
                    name: Identifier {
                        disambiguator: 0,
                        name: "foo".into()
                    }
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}

#[test]
fn test_rustc_demangle_extra_suffix() {
    assert_eq!(
        parse_symbol_name("NvNtNtNtNtCs92dm3009vxr_4rand4rngs7adapter9reseeding4fork23FORK_HANDLER_REGISTERED.0.0"),
        Ok((
            ".0.0",
            SymbolName {
                version: None,
                path: Path::Nested {
                    namespace: 118,
                    path: Path::Nested {
                        namespace: 116,
                        path: Path::Nested {
                            namespace: 116,
                            path: Path::Nested {
                                namespace: 116,
                                path: Path::Nested {
                                    namespace: 116,
                                    path: Path::CrateRoot(Identifier {
                                        disambiguator: 0x_693e_a8e7_2247_470f,
                                        name: "rand".into()
                                    })
                                    .into(),
                                    name: Identifier {
                                        disambiguator: 0,
                                        name: "rngs".into()
                                    }
                                }
                                .into(),
                                name: Identifier {
                                    disambiguator: 0,
                                    name: "adapter".into()
                                }
                            }
                            .into(),
                            name: Identifier {
                                disambiguator: 0,
                                name: "reseeding".into()
                            }
                        }
                        .into(),
                        name: Identifier {
                            disambiguator: 0,
                            name: "fork".into()
                        }
                    }
                    .into(),
                    name: Identifier {
                        disambiguator: 0,
                        name: "FORK_HANDLER_REGISTERED".into()
                    }
                }
                .into(),
                instantiating_crate: None
            }
        ))
    );
}
