use crate::mini_parser::Parser;
use crate::rust_v0::display::{self, Style};
use crate::rust_v0::parsers::{Context, IndexedStr};
use crate::rust_v0::{
    Abi, BasicType, Const, DynBounds, DynTrait, DynTraitAssocBinding, GenericArg, Identifier, ImplPath, Path, Symbol,
    Type,
};
use std::borrow::Cow;
use std::rc::Rc;

fn id(disambiguator: u64, name: &str) -> Identifier {
    Identifier {
        name: Cow::Borrowed(name),
        disambiguator,
    }
}

fn simplify_parser<'a, P, T>(mut parser: P) -> impl FnMut(&'a str) -> Result<(T, &'a str), ()>
where
    P: Parser<IndexedStr<'a>, Context<'a>, Output = T>,
{
    move |input| {
        parser
            .parse(IndexedStr::new(input), &mut Context::default())
            .map(|(result, suffix)| (result, suffix.data))
    }
}

#[test]
fn test_parse_undisambiguated_identifier() {
    let mut parse = simplify_parser(super::parse_undisambiguated_identifier);

    assert_eq!(parse(""), Err(()));
    assert_eq!(parse("6_123foo"), Ok((Cow::Borrowed("123foo"), "")));
    assert_eq!(parse("3bar"), Ok((Cow::Borrowed("bar"), "")));

    assert_eq!(
        parse("u30____7hkackfecea1cbdathfdh9hlq6y"),
        Ok((Cow::Borrowed("საჭმელად_გემრიელი_სადილი"), ""))
    );
}

#[test]
fn test_parse_abi() {
    let mut parse = simplify_parser(super::parse_abi);

    assert_eq!(parse(""), Err(()));
    assert_eq!(parse("CDEF"), Ok((Abi::C, "DEF")));
    assert_eq!(parse("3abcdef"), Ok((Abi::Named(Cow::Borrowed("abc")), "def")));
}

#[track_caller]
fn check_parse_const(input: &str, expected: &str) {
    let result = display::display_const(
        &simplify_parser(super::parse_const)(input).unwrap().0,
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
    let mut parse = simplify_parser(super::parse_base62_number);

    assert_eq!(parse("_"), Ok((0, "")));
    assert_eq!(parse("0_"), Ok((1, "")));
    assert_eq!(parse("7_"), Ok((8, "")));
    assert_eq!(parse("a_"), Ok((11, "")));
    assert_eq!(parse("Z_"), Ok((62, "")));
    assert_eq!(parse("10_"), Ok((63, "")));
}

#[test]
fn test_parse_decimal_number() {
    let mut parse = simplify_parser(super::parse_decimal_number);

    assert_eq!(parse(""), Err(()));

    assert_eq!(parse("0"), Ok((0, "")));
    assert_eq!(parse("7"), Ok((7, "")));
    assert_eq!(parse("c"), Err(()));

    assert_eq!(parse("00"), Ok((0, "0",)));
    assert_eq!(parse("07"), Ok((0, "7",)));
    assert_eq!(parse("0c"), Ok((0, "c",)));
    assert_eq!(parse("70"), Ok((70, "",)));
    assert_eq!(parse("77"), Ok((77, "",)));
    assert_eq!(parse("7c"), Ok((7, "c",)));
    assert_eq!(parse("c0"), Err(()));
    assert_eq!(parse("c7"), Err(()));
    assert_eq!(parse("cc"), Err(()));

    assert_eq!(parse("999999999999999999999999"), Err(()));
}

fn parse_symbol(input: &str) -> Result<(Symbol, &str), ()> {
    super::parse_symbol(input)
}

#[test]
fn test_rustc_demangle_crate_with_leading_digit() {
    assert_eq!(
        parse_symbol("NvC6_123foo3bar"),
        Ok((
            Symbol {
                version: None,
                path: Path::Nested {
                    namespace: b'v',
                    path: Path::CrateRoot(id(0, "123foo")).into(),
                    identifier: Identifier {
                        name: Cow::Borrowed("bar"),
                        disambiguator: 0,
                    }
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_utf8_idents() {
    assert_eq!(
        parse_symbol("NqCs4fqI2P2rA04_11utf8_identsu30____7hkackfecea1cbdathfdh9hlq6y"),
        Ok((
            Symbol {
                version: None,
                path: Path::Nested {
                    namespace: b'q',
                    path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "utf8_idents")).into(),
                    identifier: id(0, "საჭმელად_გემრიელი_სადილი")
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_closure_1() {
    assert_eq!(
        parse_symbol("NCNCNgCs6DXkGYLi8lr_2cc5spawn00B5_"),
        Ok((
            Symbol {
                version: None,
                path: Path::Nested {
                    namespace: b'C',
                    path: Path::Nested {
                        namespace: b'C',
                        path: Path::Nested {
                            namespace: b'g',
                            path: Path::CrateRoot(id(0x_4d64_68d6_c9fd_4bb3, "cc")).into(),
                            identifier: id(0, "spawn")
                        }
                        .into(),
                        identifier: id(0, "")
                    }
                    .into(),
                    identifier: id(0, "")
                }
                .into(),
                instantiating_crate: Some(Path::CrateRoot(id(0x_4d64_68d6_c9fd_4bb3, "cc")).into()),
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_closure_2() {
    let crate_root = Rc::new(Path::CrateRoot(Identifier {
        disambiguator: 0x_8468_17f7_41e5_4dfd,
        name: Cow::Borrowed("core"),
    }));

    let core_slice = Rc::new(Path::Nested {
        namespace: b'g',
        path: Rc::clone(&crate_root),
        identifier: id(0, "slice"),
    });

    assert_eq!(
        parse_symbol(
            "NCINkXs25_NgCsbmNqQUJIY6D_4core5sliceINyB9_4IterhENuNgNoBb_4iter8iterator8Iterator9rpositionNCNgNpB9_6memchr7memrchrs_0E0Bb_"
        ),
        Ok((
            Symbol {
                version: None,
                path: Path::Nested {
                    namespace: b'C',
                    path: Path::Generic {
                        path: Path::Nested {
                            namespace: b'k',
                            path: Path::TraitImpl {
                                impl_path: ImplPath {
                                    path: Rc::clone(&core_slice),
                                    disambiguator: 131,
                                },
                                type_: Type::Named(
                                    Path::Generic {
                                        path: Path::Nested {
                                            namespace: b'y',
                                            path: Rc::clone(&core_slice),
                                            identifier: id(0, "Iter")
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
                                            identifier: id(0, "iter")
                                        }
                                        .into(),
                                        identifier: id(0, "iterator")
                                    }
                                    .into(),
                                    identifier: id(0, "Iterator")
                                }
                                .into()
                            }
                            .into(),
                            identifier: id(0, "rposition")
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
                                            identifier: id(0, "memchr")
                                        }
                                        .into(),
                                        identifier: id(0, "memrchr")
                                    }
                                    .into(),
                                    identifier: id(1, "")
                                }
                                .into()
                            )
                            .into()
                        )]
                    }
                    .into(),
                    identifier: id(0, "")
                }
                .into(),
                instantiating_crate: Some(crate_root),
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_dyn_trait() {
    assert_eq!(
        parse_symbol("INbNbCskIICzLVDPPb_5alloc5alloc8box_freeDINbNiB4_5boxed5FnBoxuEp6OutputuEL_ECs1iopQbuBiw2_3std"),
        Ok((
            Symbol {
                version: None,
                path: Path::Generic {
                    path: Path::Nested {
                        namespace: b'b',
                        path: Path::Nested {
                            namespace: b'b',
                            path: Path::CrateRoot(id(0x_f15a_878b_47eb_696b, "alloc")).into(),
                            identifier: id(0, "alloc")
                        }
                        .into(),
                        identifier: id(0, "box_free")
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
                                                path: Path::CrateRoot(id(0x_f15a_878b_47eb_696b, "alloc")).into(),
                                                identifier: id(0, "boxed")
                                            }
                                            .into(),
                                            identifier: id(0, "FnBox")
                                        }
                                        .into(),
                                        generic_args: vec![GenericArg::Type(Type::Basic(BasicType::Unit).into())]
                                    }
                                    .into(),
                                    dyn_trait_assoc_bindings: vec![DynTraitAssocBinding {
                                        name: Cow::Borrowed("Output"),
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
                instantiating_crate: Some(Path::CrateRoot(id(0x_0f1a_6958_f46c_38e4, "std")).into()),
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_usize_123() {
    assert_eq!(
        parse_symbol("INtC8arrayvec8ArrayVechKj7b_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::Generic {
                    path: Path::Nested {
                        namespace: b't',
                        path: Path::CrateRoot(id(0, "arrayvec")).into(),
                        identifier: id(0, "ArrayVec")
                    }
                    .into(),
                    generic_args: vec![
                        GenericArg::Type(Type::Basic(BasicType::U8).into()),
                        GenericArg::Const(Const::Usize(123).into())
                    ]
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_u8_11() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_8UnsignedKhb_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Unsigned")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::U8(11).into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_i16_152() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_6SignedKs98_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Signed")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::I16(152).into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_i8_negative_11() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_6SignedKanb_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Signed")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::I8(-11).into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_bool_false() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4BoolKb0_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Bool")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::Bool(false).into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_bool_true() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4BoolKb1_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Bool")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::Bool(true).into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_char_v() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4CharKc76_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Char")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::Char('v').into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_char_lf() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4CharKca_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Char")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::Char('\n').into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_char_partial_differential() {
    assert_eq!(
        parse_symbol("MCs4fqI2P2rA04_13const_genericINtB0_4CharKc2202_E"),
        Ok((
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                        disambiguator: 0,
                    },
                    type_: Type::Named(
                        Path::Generic {
                            path: Path::Nested {
                                namespace: b't',
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                identifier: id(0, "Char")
                            }
                            .into(),
                            generic_args: vec![GenericArg::Const(Const::Char('∂').into())]
                        }
                        .into()
                    )
                    .into()
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_const_generics_placeholder() {
    assert_eq!(
        parse_symbol("NvNvMCs4fqI2P2rA04_13const_genericINtB4_3FooKpE3foo3FOO"),
        Ok((
            Symbol {
                version: None,
                path: Path::Nested {
                    namespace: b'v',
                    path: Path::Nested {
                        namespace: b'v',
                        path: Path::InherentImpl {
                            impl_path: ImplPath {
                                path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                disambiguator: 0,
                            },
                            type_: Type::Named(
                                Path::Generic {
                                    path: Path::Nested {
                                        namespace: b't',
                                        path: Path::CrateRoot(id(0x_317d_4810_89b8_c8fe, "const_generic")).into(),
                                        identifier: id(0, "Foo")
                                    }
                                    .into(),
                                    generic_args: vec![GenericArg::Const(Const::Placeholder.into())]
                                }
                                .into()
                            )
                            .into()
                        }
                        .into(),
                        identifier: id(0, "foo")
                    }
                    .into(),
                    identifier: id(0, "FOO")
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
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
            Symbol {
                version: None,
                path: Path::InherentImpl {
                    impl_path: ImplPath {
                        path: Path::CrateRoot(id(0, "")).into(),
                        disambiguator: 0,
                    },
                    type_: tuple
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: None,
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_thinlto() {
    assert_eq!(
        parse_symbol("C3foo.llvm.9D1C9369"),
        Ok((
            Symbol {
                version: None,
                path: Path::CrateRoot(id(0, "foo")).into(),
                instantiating_crate: None,
                vendor_specific_suffix: Some(".llvm.9D1C9369"),
            },
            ""
        ))
    );

    assert_eq!(
        parse_symbol("C3foo.llvm.9D1C9369@@16"),
        Ok((
            Symbol {
                version: None,
                path: Path::CrateRoot(id(0, "foo")).into(),
                instantiating_crate: None,
                vendor_specific_suffix: Some(".llvm.9D1C9369@@16"),
            },
            ""
        ))
    );

    assert_eq!(
        parse_symbol("NvC9backtrace3foo.llvm.A5310EB9"),
        Ok((
            Symbol {
                version: None,
                path: Path::Nested {
                    namespace: b'v',
                    path: Path::CrateRoot(id(0, "backtrace")).into(),
                    identifier: id(0, "foo")
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: Some(".llvm.A5310EB9"),
            },
            "",
        ))
    );
}

#[test]
fn test_rustc_demangle_extra_suffix() {
    assert_eq!(
        parse_symbol("NvNtNtNtNtCs92dm3009vxr_4rand4rngs7adapter9reseeding4fork23FORK_HANDLER_REGISTERED.0.0"),
        Ok((
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
                                    path: Path::CrateRoot(id(0x_693e_a8e7_2247_470f, "rand")).into(),
                                    identifier: id(0, "rngs")
                                }
                                .into(),
                                identifier: id(0, "adapter")
                            }
                            .into(),
                            identifier: id(0, "reseeding")
                        }
                        .into(),
                        identifier: id(0, "fork")
                    }
                    .into(),
                    identifier: id(0, "FORK_HANDLER_REGISTERED")
                }
                .into(),
                instantiating_crate: None,
                vendor_specific_suffix: Some(".0.0"),
            },
            "",
        ))
    );
}
