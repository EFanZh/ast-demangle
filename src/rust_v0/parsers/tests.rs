use crate::rust_v0::context::Context;
use crate::rust_v0::display::{self, Style};
use crate::rust_v0::{
    Abi, BasicType, Const, DynBounds, DynTrait, DynTraitAssocBinding, GenericArg, Identifier, ImplPath, Path, Symbol,
    Type,
};
use nom::error::{Error, ErrorKind};
use nom::IResult;
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;

fn make_err<T>(input: &str, error_kind: ErrorKind) -> IResult<&str, T> {
    Err(nom::Err::Error(Error::new(input, error_kind)))
}

fn simplify_result<'a, T>(result: IResult<Context<'a, '_>, T>) -> IResult<&'a str, T> {
    result
        .map(|(rest, result)| (rest.data, result))
        .map_err(|e| e.map(|e| Error::new(e.input.data, e.code)))
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
        simplify_result(super::parse_undisambiguated_identifier(Context::new(
            input,
            &RefCell::default(),
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
fn test_parse_abi() {
    fn parse(input: &str) -> IResult<&str, Abi> {
        let back_ref_table = RefCell::default();

        simplify_result(super::parse_abi(Context::new(input, &back_ref_table)))
    }

    assert_eq!(parse(""), make_err("", ErrorKind::Digit));

    assert_eq!(parse("CDEF"), Ok(("DEF", Abi::C)));
    assert_eq!(parse("3abcdef"), Ok(("def", Abi::Named("abc".into()))));
}

#[track_caller]
fn check_parse_const(input: &str, expected: &str) {
    let back_ref_table = RefCell::default();

    let result = display::display_const(
        &super::parse_const(Context::new(input, &back_ref_table)).ok().unwrap().1,
        Style::Normal,
        0,
        false,
    )
    .to_string();

    assert_eq!(result, expected);
}

#[test]
fn test_parse_const_str() {
    check_parse_const("e616263_", "{*\"abc\"}");
    check_parse_const("e27_", r#"{*"'"}"#);
    check_parse_const("e090a_", "{*\"\\t\\n\"}");
    check_parse_const("ee28882c3bc_", "{*\"∂ü\"}");

    check_parse_const(
        "ee183a1e18390e183ade1839be18394e1839ae18390e183935fe18392e18394e1839b\
         e183a0e18398e18394e1839ae183985fe183a1e18390e18393e18398e1839ae18398_",
        "{*\"საჭმელად_გემრიელი_სადილი\"}",
    );

    check_parse_const(
        "ef09f908af09fa688f09fa686f09f90ae20c2a720f09f90b6f09f9192e298\
         95f09f94a520c2a720f09fa7a1f09f929bf09f929af09f9299f09f929c_",
        "{*\"🐊🦈🦆🐮 § 🐶👒☕🔥 § 🧡💛💚💙💜\"}",
    );
}

#[test]
fn test_parse_const_ref_str() {
    check_parse_const("Re616263_", "\"abc\"");
    check_parse_const("Re27_", r#""'""#);
    check_parse_const("Re090a_", "\"\\t\\n\"");
    check_parse_const("Ree28882c3bc_", "\"∂ü\"");

    check_parse_const(
        "Ree183a1e18390e183ade1839be18394e1839ae18390e183935fe18392e18394e1839b\
         e183a0e18398e18394e1839ae183985fe183a1e18390e18393e18398e1839ae18398_",
        "\"საჭმელად_გემრიელი_სადილი\"",
    );

    check_parse_const(
        "Ref09f908af09fa688f09fa686f09f90ae20c2a720f09f90b6f09f9192e298\
         95f09f94a520c2a720f09fa7a1f09f929bf09f929af09f9299f09f929c_",
        "\"🐊🦈🦆🐮 § 🐶👒☕🔥 § 🧡💛💚💙💜\"",
    );
}

#[test]
fn test_parse_const_ref() {
    check_parse_const("Rp", "{&_}");
    check_parse_const("Rh7b_", "{&123}");
    check_parse_const("Rb0_", "{&false}");
    check_parse_const("Rc58_", "{&'X'}");
    check_parse_const("RRRh0_", "{&&&0}");
    check_parse_const("RRRe_", "{&&\"\"}");
    check_parse_const("QAE", "{&mut []}");
}

#[test]
fn test_parse_const_array() {
    check_parse_const("AE", "{[]}");
    check_parse_const("Aj0_E", "{[0]}");
    check_parse_const("Ah1_h2_h3_E", "{[1, 2, 3]}");
    check_parse_const("ARe61_Re62_Re63_E", "{[\"a\", \"b\", \"c\"]}");
    check_parse_const("AAh1_h2_EAh3_h4_EE", "{[[1, 2], [3, 4]]}");
}

#[test]
fn test_parse_const_tuple() {
    check_parse_const("TE", "{()}");
    check_parse_const("Tj0_E", "{(0,)}");
    check_parse_const("Th1_b0_E", "{(1, false)}");
    check_parse_const("TRe616263_c78_RAh1_h2_h3_EE", "{(\"abc\", 'x', &[1, 2, 3])}");
}

#[test]
fn test_parse_const_adt() {
    check_parse_const(
        "VNvINtNtC4core6option6OptionjE4NoneU",
        "{core::option::Option::<usize>::None}",
    );

    check_parse_const(
        "VNvINtNtC4core6option6OptionjE4SomeTj0_E",
        "{core::option::Option::<usize>::Some(0)}",
    );

    check_parse_const(
        "VNtC3foo3BarS1sRe616263_2chc78_5sliceRAh1_h2_h3_EE",
        "{foo::Bar { s: \"abc\", ch: 'x', slice: &[1, 2, 3] }}",
    );
}

#[test]
fn test_parse_base62_number() {
    fn parse(input: &str) -> IResult<&str, u64> {
        simplify_result(super::parse_base62_number(Context::new(input, &RefCell::default())))
    }

    assert_eq!(parse("_"), Ok(("", 0)));
    assert_eq!(parse("0_"), Ok(("", 1)));
    assert_eq!(parse("7_"), Ok(("", 8)));
    assert_eq!(parse("a_"), Ok(("", 11)));
    assert_eq!(parse("Z_"), Ok(("", 62)));
    assert_eq!(parse("10_"), Ok(("", 63)));
}

#[test]
fn test_parse_decimal_number() {
    fn parse(input: &str) -> IResult<&str, u64> {
        simplify_result(super::parse_decimal_number(Context::new(input, &RefCell::default())))
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

fn parse_symbol(input: &str) -> IResult<&str, Symbol> {
    simplify_result(super::parse_symbol(Context::new(input, &RefCell::default())))
}

#[test]
fn test_rustc_demangle_crate_with_leading_digit() {
    assert_eq!(
        parse_symbol("NvC6_123foo3bar"),
        Ok((
            "",
            Symbol {
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
        parse_symbol("NqCs4fqI2P2rA04_11utf8_identsu30____7hkackfecea1cbdathfdh9hlq6y"),
        Ok((
            "",
            Symbol {
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
        parse_symbol("NCNCNgCs6DXkGYLi8lr_2cc5spawn00B5_"),
        Ok((
            "",
            Symbol {
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
        parse_symbol(
            "NCINkXs25_NgCsbmNqQUJIY6D_4core5sliceINyB9_4IterhENuNgNoBb_4iter8iterator8Iterator9rpositionNCNgNpB9_6memchr7memrchrs_0E0Bb_"
        ),
        Ok((
            "",
            Symbol {
                version: None,
                path: Path::Nested {
                    namespace: b'C',
                    path: Path::Generic {
                        path: Path::Nested {
                            namespace: b'k',
                            path: Path::TraitImpl {
                                impl_path: ImplPath {
                                    disambiguator: 131,
                                    path: Rc::clone(&core_slice)
                                },
                                type_: Type::Named(
                                    Path::Generic {
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
        parse_symbol("INbNbCskIICzLVDPPb_5alloc5alloc8box_freeDINbNiB4_5boxed5FnBoxuEp6OutputuEL_ECs1iopQbuBiw2_3std"),
        Ok((
            "",
            Symbol {
                version: None,
                path: Path::Generic {
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
                                    path: Path::Generic {
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
        parse_symbol("INtC8arrayvec8ArrayVechKj7b_E"),
        Ok((
            "",
            Symbol {
                version: None,
                path: Path::Generic {
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
                        GenericArg::Const(Const::Usize(123).into())
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_8UnsignedKhb_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::U8(11).into())]
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_6SignedKs98_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::I16(152).into())]
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_6SignedKanb_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::I8(-11).into())]
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4BoolKb0_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::Bool(false).into())]
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4BoolKb1_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::Bool(true).into())]
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4CharKc76_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::Char('v').into())]
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4CharKca_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::Char('\n').into())]
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
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4CharKc2202_E"),
        Ok((
            "",
            Symbol {
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
                        Path::Generic {
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
                            generic_args: vec![GenericArg::Const(Const::Char('∂').into())]
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
        parse_symbol("NvNvMCs4fqI2P2rA04_13const_genericINtB4_3FooKpE3foo3FOO"),
        Ok((
            "",
            Symbol {
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
                                Path::Generic {
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
        parse_symbol("MC0TTTTTTpB8_EB7_EB6_EB5_EB4_EB3_E"),
        Ok((
            "",
            Symbol {
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
        parse_symbol("C3foo.llvm.9D1C9369"),
        Ok((
            ".llvm.9D1C9369",
            Symbol {
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
        parse_symbol("C3foo.llvm.9D1C9369@@16"),
        Ok((
            ".llvm.9D1C9369@@16",
            Symbol {
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
        parse_symbol("NvC9backtrace3foo.llvm.A5310EB9"),
        Ok((
            ".llvm.A5310EB9",
            Symbol {
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
        parse_symbol("NvNtNtNtNtCs92dm3009vxr_4rand4rngs7adapter9reseeding4fork23FORK_HANDLER_REGISTERED.0.0"),
        Ok((
            ".0.0",
            Symbol {
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
