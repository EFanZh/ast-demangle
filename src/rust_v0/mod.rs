use self::context::Context;
use nom::error::ParseError;
use nom::{IResult, Parser};
use std::borrow::Cow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::error::Error;
use std::rc::Rc;
use std::str;

mod context;

#[cfg(test)]
mod tests;

fn opt_u64<I: Clone, E: ParseError<I>>(parser: impl Parser<I, u64, E>) -> impl FnMut(I) -> IResult<I, u64, E> {
    use nom::combinator::{map_opt, opt};

    map_opt(opt(parser), |num| {
        Some(match num {
            None => 0,
            Some(num) => num.checked_add(1)?,
        })
    })
}

// References:
//
// - https://github.com/alexcrichton/rustc-demangle/blob/master/src/v0.rs.
// - https://github.com/michaelwoerister/std-mangle-rs/blob/master/src/ast_demangle.rs.
// - https://github.com/rust-lang/rust/blob/master/compiler/rustc_symbol_mangling/src/v0.rs.
// - https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html.

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SymbolName<'a> {
    pub version: Option<u64>,
    pub path: Rc<Path<'a>>,
    pub instantiating_crate: Option<Rc<Path<'a>>>,
}

impl<'a> SymbolName<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::combinator::opt;
        use nom::sequence::tuple;

        tuple((opt(DecimalNumber::parse), Path::parse, opt(Path::parse)))
            .map(|(version, path, instantiating_crate)| Self {
                version,
                path,
                instantiating_crate,
            })
            .parse(context)
    }

    pub fn parse_from_str(input: &'a str) -> Result<Self, Box<dyn Error>> {
        let input = input.as_bytes();

        let input = input
            .strip_prefix(b"_R")
            .or_else(|| input.strip_prefix(b"R"))
            .or_else(|| input.strip_prefix(b"__R"))
            .unwrap_or(input);

        Self::parse(Context::new(input, &RefCell::default()))
            .map(|(_, result)| result)
            .map_err(|e| e.map(|e| e.code).into())
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Path<'a> {
    CrateRoot(Identifier<'a>),
    InherentImpl {
        impl_path: ImplPath<'a>,
        type_: Rc<Type<'a>>,
    },
    TraitImpl {
        impl_path: ImplPath<'a>,
        type_: Rc<Type<'a>>,
        path: Rc<Path<'a>>,
    },
    TraitDefinition {
        type_: Rc<Type<'a>>,
        path: Rc<Path<'a>>,
    },
    Nested {
        namespace: u8,
        path: Rc<Path<'a>>,
        identifier: Identifier<'a>,
    },
    GenericArgs {
        path: Rc<Path<'a>>,
        generic_args: Vec<GenericArg<'a>>,
    },
}

impl<'a> Path<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Rc<Self>> {
        use nom::branch::alt;
        use nom::bytes::complete::{tag, take};
        use nom::combinator::map_opt;
        use nom::multi::many0;
        use nom::sequence::{delimited, preceded, tuple};

        alt((
            preceded(tag("C"), Identifier::parse).map(Self::CrateRoot),
            preceded(tag("M"), ImplPath::parse.and(Type::parse))
                .map(|(impl_path, type_)| Self::InherentImpl { impl_path, type_ }),
            preceded(tag("X"), tuple((ImplPath::parse, Type::parse, Path::parse)))
                .map(|(impl_path, type_, path)| Self::TraitImpl { impl_path, type_, path }),
            preceded(tag("Y"), Type::parse.and(Path::parse)).map(|(type_, path)| Self::TraitDefinition { type_, path }),
            preceded(tag("N"), tuple((take(1_usize), Path::parse, Identifier::parse))).map(
                |(namespace, path, identifier)| Self::Nested {
                    namespace: namespace.data[0],
                    path,
                    identifier,
                },
            ),
            delimited(tag("I"), Path::parse.and(many0(GenericArg::parse)), tag("E"))
                .map(|(path, generic_args)| Self::GenericArgs { path, generic_args }),
        ))
        .map(|result| {
            let result = Rc::new(result);

            context
                .back_ref_table
                .borrow_mut()
                .paths
                .insert(context.index, Rc::clone(&result));

            result
        })
        .or(map_opt(BackRef::parse, |back_ref| {
            context.back_ref_table.borrow().paths.get(&back_ref).cloned()
        }))
        .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ImplPath<'a> {
    pub disambiguator: u64,
    pub path: Rc<Path<'a>>,
}

impl<'a> ImplPath<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        opt_u64(Disambiguator::parse)
            .and(Path::parse)
            .map(|(disambiguator, path)| Self { disambiguator, path })
            .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier<'a> {
    pub disambiguator: u64,
    pub name: Cow<'a, str>,
}

impl<'a> Identifier<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        opt_u64(Disambiguator::parse)
            .and(UndisambiguatedIdentifier::parse)
            .map(|(disambiguator, name)| Self { disambiguator, name })
            .parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Disambiguator;

impl Disambiguator {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
        use nom::bytes::complete::tag;
        use nom::sequence::preceded;

        preceded(tag("s"), Base62Number::parse).parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct UndisambiguatedIdentifier;

impl UndisambiguatedIdentifier {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Cow<'a, str>> {
        use nom::bytes::complete::{tag, take};
        use nom::combinator::{map_opt, opt};
        use nom::sequence::tuple;

        tuple((
            opt(tag("u")).map(|tag| tag.is_some()),
            DecimalNumber::parse,
            opt(tag("_")),
        ))
        .flat_map(|(is_punycode, length, _)| {
            map_opt(take(length), move |name: Context<'a, 'b>| {
                Some(if is_punycode {
                    let mut buffer = name.data.to_vec();

                    if let Some(c) = buffer.iter_mut().rfind(|&&mut c| c == b'_') {
                        *c = b'-';
                    }

                    Cow::Owned(punycode::decode(str::from_utf8(&buffer).ok()?).ok()?)
                } else {
                    Cow::Borrowed(str::from_utf8(name.data).ok()?)
                })
            })
        })
        .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum GenericArg<'a> {
    Lifetme(u64),
    Type(Rc<Type<'a>>),
    Const(Rc<Const<'a>>),
}

impl<'a> GenericArg<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::sequence::preceded;

        alt((
            Lifetime::parse.map(Self::Lifetme),
            Type::parse.map(Self::Type),
            preceded(tag("K"), Const::parse).map(Self::Const),
        ))
        .parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Lifetime;

impl Lifetime {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
        use nom::bytes::complete::tag;
        use nom::sequence::preceded;

        preceded(tag("L"), Base62Number::parse).parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Binder;

impl Binder {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
        use nom::bytes::complete::tag;
        use nom::sequence::preceded;

        preceded(tag("G"), Base62Number::parse).parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type<'a> {
    Basic(BasicType),
    Named(Rc<Path<'a>>),
    Array(Rc<Type<'a>>, Rc<Const<'a>>),
    Slice(Rc<Type<'a>>),
    Tuple(Vec<Rc<Type<'a>>>),
    Ref { lifetime: Option<u64>, type_: Rc<Type<'a>> },
    RefMut { lifetime: Option<u64>, type_: Rc<Type<'a>> },
    PtrConst(Rc<Type<'a>>),
    PtrMut(Rc<Type<'a>>),
    Fn(FnSig<'a>),
    DynTrait { dyn_bounds: DynBounds<'a>, lifetime: u64 },
}

impl<'a> Type<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Rc<Self>> {
        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::combinator::{map_opt, opt};
        use nom::multi::many0;
        use nom::sequence::{delimited, preceded};

        alt((
            BasicType::parse.map(Self::Basic),
            Path::parse.map(Self::Named),
            preceded(tag("A"), Type::parse.and(Const::parse)).map(|(type_, length)| Self::Array(type_, length)),
            preceded(tag("S"), Type::parse).map(Self::Slice),
            delimited(tag("T"), many0(Type::parse), tag("E")).map(Self::Tuple),
            preceded(tag("R"), opt(Lifetime::parse).and(Type::parse))
                .map(|(lifetime, type_)| Self::Ref { lifetime, type_ }),
            preceded(tag("Q"), opt(Lifetime::parse).and(Type::parse))
                .map(|(lifetime, type_)| Self::RefMut { lifetime, type_ }),
            preceded(tag("P"), Type::parse).map(Self::PtrConst),
            preceded(tag("O"), Type::parse).map(Self::PtrMut),
            preceded(tag("F"), FnSig::parse).map(Self::Fn),
            preceded(tag("D"), DynBounds::parse.and(Lifetime::parse))
                .map(|(dyn_bounds, lifetime)| Self::DynTrait { dyn_bounds, lifetime }),
        ))
        .map(|result| {
            let result = Rc::new(result);

            context
                .back_ref_table
                .borrow_mut()
                .types
                .insert(context.index, Rc::clone(&result));

            result
        })
        .or(map_opt(BackRef::parse, |back_ref| {
            context.back_ref_table.borrow().types.get(&back_ref).cloned()
        }))
        .parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BasicType {
    I8,
    Bool,
    Char,
    F64,
    Str,
    F32,
    U8,
    Isize,
    Usize,
    I32,
    U32,
    I128,
    U128,
    I16,
    U16,
    Unit,
    Ellipsis,
    I64,
    U64,
    Never,
    Placeholder,
}

impl BasicType {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::bytes::complete::take;
        use nom::combinator::map_opt;

        map_opt(take(1_usize), |s: Context<'a, 'b>| match s.data[0] {
            b'a' => Some(Self::I8),
            b'b' => Some(Self::Bool),
            b'c' => Some(Self::Char),
            b'd' => Some(Self::F64),
            b'e' => Some(Self::Str),
            b'f' => Some(Self::F32),
            b'h' => Some(Self::U8),
            b'i' => Some(Self::Isize),
            b'j' => Some(Self::Usize),
            b'l' => Some(Self::I32),
            b'm' => Some(Self::U32),
            b'n' => Some(Self::I128),
            b'o' => Some(Self::U128),
            b's' => Some(Self::I16),
            b't' => Some(Self::U16),
            b'u' => Some(Self::Unit),
            b'v' => Some(Self::Ellipsis),
            b'x' => Some(Self::I64),
            b'y' => Some(Self::U64),
            b'z' => Some(Self::Never),
            b'p' => Some(Self::Placeholder),
            _ => None,
        })
        .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FnSig<'a> {
    pub binder: u64,
    pub is_unsafe: bool,
    pub abi: Option<Abi<'a>>,
    pub argument_types: Vec<Rc<Type<'a>>>,
    pub return_type: Rc<Type<'a>>,
}

impl<'a> FnSig<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::bytes::complete::tag;
        use nom::combinator::opt;
        use nom::multi::many0;
        use nom::sequence::{preceded, terminated, tuple};

        tuple((
            opt_u64(Binder::parse),
            opt(tag("U")).map(|u| u.is_some()),
            opt(preceded(tag("K"), Abi::parse)),
            terminated(many0(Type::parse), tag("E")),
            Type::parse,
        ))
        .map(|(binder, is_unsafe, abi, argument_types, return_type)| Self {
            binder,
            is_unsafe,
            abi,
            argument_types,
            return_type,
        })
        .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Abi<'a> {
    C,
    Named(Cow<'a, str>),
}

impl<'a> Abi<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::bytes::complete::tag;

        tag("C")
            .map(|_| Abi::C)
            .or(UndisambiguatedIdentifier::parse.map(Self::Named))
            .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynBounds<'a> {
    pub binder: u64,
    pub dyn_traits: Vec<DynTrait<'a>>,
}

impl<'a> DynBounds<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::bytes::complete::tag;
        use nom::multi::many0;
        use nom::sequence::terminated;

        opt_u64(Binder::parse)
            .and(terminated(many0(DynTrait::parse), tag("E")))
            .map(|(binder, dyn_traits)| Self { binder, dyn_traits })
            .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynTrait<'a> {
    pub path: Rc<Path<'a>>,
    pub dyn_trait_assoc_bindings: Vec<DynTraitAssocBinding<'a>>,
}

impl<'a> DynTrait<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::multi::many0;

        Path::parse
            .and(many0(DynTraitAssocBinding::parse))
            .map(|(path, dyn_trait_assoc_bindings)| Self {
                path,
                dyn_trait_assoc_bindings,
            })
            .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DynTraitAssocBinding<'a> {
    pub identifier: Cow<'a, str>,
    pub type_: Rc<Type<'a>>,
}

impl<'a> DynTraitAssocBinding<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Self> {
        use nom::bytes::complete::tag;
        use nom::sequence::preceded;

        preceded(tag("p"), UndisambiguatedIdentifier::parse.and(Type::parse))
            .map(|(identifier, type_)| Self { identifier, type_ })
            .parse(context)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Const<'a> {
    Data { type_: Rc<Type<'a>>, data: &'a [u8] },
    Placeholder,
}

impl<'a> Const<'a> {
    fn parse<'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Rc<Self>> {
        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::character::complete::hex_digit0;
        use nom::combinator::{map_opt, opt, recognize};
        use nom::sequence::terminated;

        alt((
            Type::parse
                .and(terminated(recognize(opt(tag("n")).and(hex_digit0)), tag("_")))
                .map(|(type_, data)| Self::Data { type_, data: data.data }),
            tag("p").map(|_| Self::Placeholder),
        ))
        .map(|result| {
            let result = Rc::new(result);

            context
                .back_ref_table
                .borrow_mut()
                .consts
                .insert(context.index, Rc::clone(&result));

            result
        })
        .or(map_opt(BackRef::parse, |back_ref| {
            context.back_ref_table.borrow().consts.get(&back_ref).cloned()
        }))
        .parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Base62Number;

impl Base62Number {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
        use nom::bytes::complete::tag;
        use nom::character::complete::alphanumeric0;
        use nom::combinator::map_opt;
        use nom::sequence::terminated;

        map_opt(terminated(alphanumeric0, tag("_")), |num: Context| {
            let num = if num.data.is_empty() {
                0
            } else {
                let mut value = 0_u64;

                for c in num.data {
                    let digit = match c {
                        b'0'..=b'9' => c - b'0',
                        b'a'..=b'z' => 10 + (c - b'a'),
                        _ => 36 + (c - b'A'),
                    };

                    value = value.checked_mul(62)?;
                    value = value.checked_add(digit.into())?;
                }

                value.checked_add(1)?
            };

            Some(num)
        })
        .parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct BackRef;

impl BackRef {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, usize> {
        use nom::bytes::complete::tag;
        use nom::combinator::map_opt;
        use nom::sequence::preceded;

        map_opt(preceded(tag("B"), Base62Number::parse), |num| num.try_into().ok()).parse(context)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct DecimalNumber;

impl DecimalNumber {
    fn parse<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
        use nom::bytes::complete::tag;
        use nom::character::complete::digit1;
        use nom::combinator::map_opt;

        map_opt(tag("0").or(digit1), |num: Context<'a, 'b>| {
            str::from_utf8(num.data).ok()?.parse().ok()
        })
        .parse(context)
    }
}
