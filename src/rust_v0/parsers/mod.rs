use crate::mini_parser::combinators::{alt, and, delimited, or, preceded, terminated, tuple};
use crate::mini_parser::input::{Find, SplitAt, StripPrefix};
use crate::mini_parser::parsers::{alphanumeric0, digit1, lower_hex_digit0, tag, take};
use crate::mini_parser::Parser;
use crate::rust_v0::{
    Abi, BasicType, Const, ConstFields, DynBounds, DynTrait, DynTraitAssocBinding, FnSig, GenericArg, Identifier,
    ImplPath, Path, Symbol, Type,
};
use num_traits::{CheckedNeg, PrimInt};
use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;
use std::str;

#[cfg(test)]
mod tests;

const MAX_DEPTH: usize = 100;

#[derive(Default)]
struct Context<'a> {
    paths: HashMap<usize, Rc<Path<'a>>>,
    types: HashMap<usize, Rc<Type<'a>>>,
    consts: HashMap<usize, Rc<Const<'a>>>,
    depth: usize,
}

#[derive(Clone)]
struct IndexedStr<'a> {
    index: usize,
    data: &'a str,
}

impl<'a> IndexedStr<'a> {
    fn new(data: &'a str) -> Self {
        Self { index: 0, data }
    }
}

impl Find for IndexedStr<'_> {
    type Item = char;

    fn find(&self, pattern: impl FnMut(Self::Item) -> bool) -> usize {
        Find::find(&self.data, pattern)
    }
}

impl<'a> SplitAt for IndexedStr<'a> {
    type Prefix = &'a str;

    fn split_at(self, index: usize) -> Option<(Self::Prefix, Self)> {
        let (left, right) = SplitAt::split_at(self.data, index)?;

        Some((
            left,
            Self {
                index: self.index + left.len(),
                data: right,
            },
        ))
    }
}

impl<'a, T> StripPrefix<T> for IndexedStr<'a>
where
    &'a str: StripPrefix<T, Prefix = &'a str>,
{
    type Prefix = &'a str;

    fn strip_prefix(self, prefix: T) -> Option<(Self::Prefix, Self)> {
        let (left, right) = StripPrefix::strip_prefix(self.data, prefix)?;

        Some((
            left,
            Self {
                index: self.index + left.len(),
                data: right,
            },
        ))
    }
}

fn opt_u64<I, C>(parser: impl Parser<I, C, Output = u64>) -> impl Parser<I, C, Output = u64>
where
    I: Clone,
{
    parser.opt().map_opt(|num| {
        Some(match num {
            None => 0,
            Some(num) => num.checked_add(1)?,
        })
    })
}

fn limit_recursion_depth<'a, I, T>(
    mut parser: impl Parser<I, Context<'a>, Output = T>,
) -> impl Parser<I, Context<'a>, Output = T> {
    move |input, context: &mut Context<'a>| {
        if context.depth < MAX_DEPTH {
            context.depth += 1;

            let result = parser.parse(input, context);

            context.depth -= 1;

            result
        } else {
            Err(())
        }
    }
}

fn back_referenced<'a, T>(
    index: usize,
    base_parser: impl Parser<IndexedStr<'a>, Context<'a>, Output = T>,
    mut get_table_fn: impl for<'b> FnMut(&'b mut Context<'a>) -> &'b mut HashMap<usize, Rc<T>> + Copy,
) -> impl Parser<IndexedStr<'a>, Context<'a>, Output = Rc<T>>
where
    T: 'a,
{
    limit_recursion_depth(
        or(
            base_parser.map(Rc::new),
            parse_back_ref.map_opt_with_context(move |back_ref, context| get_table_fn(context).get(&back_ref).cloned()),
        )
        .inspect_with_context(move |result, context| {
            get_table_fn(context).insert(index, Rc::clone(result));
        }),
    )
}

// References:
//
// - <https://github.com/rust-lang/rustc-demangle/blob/main/src/v0.rs>.
// - <https://github.com/michaelwoerister/std-mangle-rs/blob/master/src/ast_demangle.rs>.
// - <https://github.com/rust-lang/rust/blob/master/compiler/rustc_symbol_mangling/src/v0.rs>.
// - <https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html>.

pub fn parse_symbol(input: &str) -> Result<(Symbol, &str), ()> {
    parse_symbol_inner(IndexedStr::new(input), &mut Context::default()).map(|(symbol, suffix)| (symbol, suffix.data))
}

fn parse_symbol_inner<'a>(
    input: IndexedStr<'a>,
    context: &mut Context<'a>,
) -> Result<(Symbol<'a>, IndexedStr<'a>), ()> {
    tuple((
        parse_decimal_number.opt(),
        parse_path,
        parse_path.opt(),
        parse_vendor_specific_suffix.opt(),
    ))
    .map(|(version, path, instantiating_crate, vendor_specific_suffix)| Symbol {
        version,
        path,
        instantiating_crate,
        vendor_specific_suffix,
    })
    .parse(input, context)
}

fn parse_path<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(Rc<Path<'a>>, IndexedStr<'a>), ()> {
    back_referenced(
        input.index,
        alt((
            preceded(tag('C'), parse_identifier).map(Path::CrateRoot),
            preceded(tag('M'), and(parse_impl_path, parse_type))
                .map(|(impl_path, type_)| Path::InherentImpl { impl_path, type_ }),
            preceded(tag('X'), tuple((parse_impl_path, parse_type, parse_path))).map(|(impl_path, type_, trait_)| {
                Path::TraitImpl {
                    impl_path,
                    type_,
                    trait_,
                }
            }),
            preceded(tag('Y'), and(parse_type, parse_path))
                .map(|(type_, trait_)| Path::TraitDefinition { type_, trait_ }),
            preceded(tag('N'), tuple((take(1_usize), parse_path, parse_identifier))).map_opt(
                |(namespace, path, identifier)| {
                    namespace.as_bytes()[0].is_ascii_alphabetic().then(|| Path::Nested {
                        namespace: namespace.as_bytes()[0],
                        path,
                        identifier,
                    })
                },
            ),
            delimited(tag('I'), and(parse_path, parse_generic_arg.many0()), tag('E'))
                .map(|(path, generic_args)| Path::Generic { path, generic_args }),
        )),
        |context| &mut context.paths,
    )
    .parse(input, context)
}

fn parse_impl_path<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(ImplPath<'a>, IndexedStr<'a>), ()> {
    and(opt_u64(parse_disambiguator), parse_path)
        .map(|(disambiguator, path)| ImplPath { disambiguator, path })
        .parse(input, context)
}

fn parse_identifier<'a>(
    input: IndexedStr<'a>,
    context: &mut Context<'a>,
) -> Result<(Identifier<'a>, IndexedStr<'a>), ()> {
    and(opt_u64(parse_disambiguator), parse_undisambiguated_identifier)
        .map(|(disambiguator, name)| Identifier { disambiguator, name })
        .parse(input, context)
}

fn parse_disambiguator<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(u64, IndexedStr<'a>), ()> {
    preceded(tag('s'), parse_base62_number).parse(input, context)
}

fn parse_undisambiguated_identifier<'a>(
    input: IndexedStr<'a>,
    context: &mut Context<'a>,
) -> Result<(Cow<'a, str>, IndexedStr<'a>), ()> {
    tuple((tag('u').opt(), parse_decimal_number, tag('_').opt()))
        .flat_map(|(punycode, length, _)| {
            let is_punycode = punycode.is_some();

            take(length).map_opt(move |name: &str| {
                if is_punycode {
                    let i = name.bytes().rposition(|c| c == b'_').map_or(0, |i| i + 1);
                    let right = &name[i..];

                    if right.is_empty() {
                        None
                    } else if right.bytes().all(|c| matches!(c, b'0'..=b'9' | b'a'..=b'z')) {
                        let mut bytes = Vec::with_capacity(name.len());

                        if i != 0 {
                            bytes.extend(&name.as_bytes()[..i - 1]);
                            bytes.push(b'-');
                        }

                        bytes.extend(right.as_bytes());

                        punycode::decode(str::from_utf8(&bytes).unwrap()).ok().map(Cow::Owned)
                    } else {
                        None
                    }
                } else {
                    Some(Cow::Borrowed(name))
                }
            })
        })
        .parse(input, context)
}

fn parse_generic_arg<'a>(
    input: IndexedStr<'a>,
    context: &mut Context<'a>,
) -> Result<(GenericArg<'a>, IndexedStr<'a>), ()> {
    alt((
        parse_lifetime.map(GenericArg::Lifetime),
        parse_type.map(GenericArg::Type),
        preceded(tag('K'), parse_const).map(GenericArg::Const),
    ))
    .parse(input, context)
}

fn parse_lifetime<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(u64, IndexedStr<'a>), ()> {
    preceded(tag('L'), parse_base62_number).parse(input, context)
}

fn parse_binder<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(u64, IndexedStr<'a>), ()> {
    preceded(tag('G'), parse_base62_number).parse(input, context)
}

fn parse_type<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(Rc<Type<'a>>, IndexedStr<'a>), ()> {
    back_referenced(
        input.index,
        alt((
            parse_basic_type.map(Type::Basic),
            parse_path.map(Type::Named),
            preceded(tag('A'), and(parse_type, parse_const)).map(|(type_, length)| Type::Array(type_, length)),
            preceded(tag('S'), parse_type).map(Type::Slice),
            delimited(tag('T'), parse_type.many0(), tag('E')).map(Type::Tuple),
            preceded(
                tag('R'),
                and(parse_lifetime.opt().map(Option::unwrap_or_default), parse_type),
            )
            .map(|(lifetime, type_)| Type::Ref { lifetime, type_ }),
            preceded(
                tag('Q'),
                and(parse_lifetime.opt().map(Option::unwrap_or_default), parse_type),
            )
            .map(|(lifetime, type_)| Type::RefMut { lifetime, type_ }),
            preceded(tag('P'), parse_type).map(Type::PtrConst),
            preceded(tag('O'), parse_type).map(Type::PtrMut),
            preceded(tag('F'), parse_fn_sig).map(Type::Fn),
            preceded(tag('D'), and(parse_dyn_bounds, parse_lifetime))
                .map(|(dyn_bounds, lifetime)| Type::DynTrait { dyn_bounds, lifetime }),
        )),
        |context| &mut context.types,
    )
    .parse(input, context)
}

fn parse_basic_type<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(BasicType, IndexedStr<'a>), ()> {
    take(1_usize)
        .map_opt(|s: &str| match s.as_bytes()[0] {
            b'a' => Some(BasicType::I8),
            b'b' => Some(BasicType::Bool),
            b'c' => Some(BasicType::Char),
            b'd' => Some(BasicType::F64),
            b'e' => Some(BasicType::Str),
            b'f' => Some(BasicType::F32),
            b'h' => Some(BasicType::U8),
            b'i' => Some(BasicType::Isize),
            b'j' => Some(BasicType::Usize),
            b'l' => Some(BasicType::I32),
            b'm' => Some(BasicType::U32),
            b'n' => Some(BasicType::I128),
            b'o' => Some(BasicType::U128),
            b's' => Some(BasicType::I16),
            b't' => Some(BasicType::U16),
            b'u' => Some(BasicType::Unit),
            b'v' => Some(BasicType::Ellipsis),
            b'x' => Some(BasicType::I64),
            b'y' => Some(BasicType::U64),
            b'z' => Some(BasicType::Never),
            b'p' => Some(BasicType::Placeholder),
            _ => None,
        })
        .parse(input, context)
}

fn parse_fn_sig<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(FnSig<'a>, IndexedStr<'a>), ()> {
    tuple((
        opt_u64(parse_binder),
        tag('U').opt(),
        preceded(tag('K'), parse_abi).opt(),
        terminated(parse_type.many0(), tag('E')),
        parse_type,
    ))
    .map(
        |(bound_lifetimes, unsafe_tag, abi, argument_types, return_type)| FnSig {
            bound_lifetimes,
            is_unsafe: unsafe_tag.is_some(),
            abi,
            argument_types,
            return_type,
        },
    )
    .parse(input, context)
}

fn parse_abi<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(Abi<'a>, IndexedStr<'a>), ()> {
    fn is_abi_name(name: &str) -> bool {
        !name.is_empty() && name.is_ascii()
    }

    alt((
        tag('C').map(|_| Abi::C),
        parse_undisambiguated_identifier.map_opt(|id| is_abi_name(&id).then_some(Abi::Named(id))),
    ))
    .parse(input, context)
}

fn parse_dyn_bounds<'a>(
    input: IndexedStr<'a>,
    context: &mut Context<'a>,
) -> Result<(DynBounds<'a>, IndexedStr<'a>), ()> {
    and(opt_u64(parse_binder), terminated(parse_dyn_trait.many0(), tag('E')))
        .map(|(bound_lifetimes, dyn_traits)| DynBounds {
            bound_lifetimes,
            dyn_traits,
        })
        .parse(input, context)
}

fn parse_dyn_trait<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(DynTrait<'a>, IndexedStr<'a>), ()> {
    and(parse_path, parse_dyn_trait_assoc_binding.many0())
        .map(|(path, dyn_trait_assoc_bindings)| DynTrait {
            path,
            dyn_trait_assoc_bindings,
        })
        .parse(input, context)
}

fn parse_dyn_trait_assoc_binding<'a>(
    input: IndexedStr<'a>,
    context: &mut Context<'a>,
) -> Result<(DynTraitAssocBinding<'a>, IndexedStr<'a>), ()> {
    preceded(tag('p'), and(parse_undisambiguated_identifier, parse_type))
        .map(|(name, type_)| DynTraitAssocBinding { name, type_ })
        .parse(input, context)
}

fn parse_const<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(Rc<Const<'a>>, IndexedStr<'a>), ()> {
    let index = input.index;

    back_referenced(
        index,
        alt((
            preceded(tag('a'), parse_const_int).map(Const::I8),
            preceded(tag('h'), parse_const_int).map(Const::U8),
            preceded(tag('i'), parse_const_int).map(Const::Isize),
            preceded(tag('j'), parse_const_int).map(Const::Usize),
            preceded(tag('l'), parse_const_int).map(Const::I32),
            preceded(tag('m'), parse_const_int).map(Const::U32),
            preceded(tag('n'), parse_const_int).map(Const::I128),
            preceded(tag('o'), parse_const_int).map(Const::U128),
            preceded(tag('s'), parse_const_int).map(Const::I16),
            preceded(tag('t'), parse_const_int).map(Const::U16),
            preceded(tag('x'), parse_const_int).map(Const::I64),
            preceded(tag('y'), parse_const_int).map(Const::U64),
            preceded(tag('b'), parse_const_int::<u8>).map_opt(|result| match result {
                0 => Some(Const::Bool(false)),
                1 => Some(Const::Bool(true)),
                _ => None,
            }),
            preceded(tag('c'), parse_const_int::<u32>).map_opt(|result| result.try_into().ok().map(Const::Char)),
            preceded(tag('e'), parse_const_str).map(Const::Str),
            preceded(tag('R'), parse_const).map(Const::Ref),
            preceded(tag('Q'), parse_const).map(Const::RefMut),
            delimited(tag('A'), parse_const.many0(), tag('E')).map(Const::Array),
            delimited(tag('T'), parse_const.many0(), tag('E')).map(Const::Tuple),
            preceded(tag('V'), and(parse_path, parse_const_fields))
                .map(|(path, fields)| Const::NamedStruct { path, fields }),
            tag('p').map(|_| Const::Placeholder),
        )),
        |context| &mut context.consts,
    )
    .parse(input, context)
}

fn parse_const_fields<'a>(
    input: IndexedStr<'a>,
    context: &mut Context<'a>,
) -> Result<(ConstFields<'a>, IndexedStr<'a>), ()> {
    alt((
        tag('U').map(|_| ConstFields::Unit),
        delimited(tag('T'), parse_const.many0(), tag('E')).map(ConstFields::Tuple),
        delimited(tag('S'), and(parse_identifier, parse_const).many0(), tag('E')).map(ConstFields::Struct),
    ))
    .parse(input, context)
}

fn parse_const_int<'a, T>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(T, IndexedStr<'a>), ()>
where
    T: CheckedNeg + PrimInt,
{
    terminated(
        and(tag('n').opt(), lower_hex_digit0).map_opt(|(is_negative, data): (_, &str)| {
            if data.is_empty() {
                Some(T::zero())
            } else {
                let base = T::from_str_radix(data, 16).ok();

                if is_negative.is_none() {
                    base
                } else {
                    base.and_then(|value| value.checked_neg())
                }
            }
        }),
        tag('_'),
    )
    .parse(input, context)
}

fn parse_const_str<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(String, IndexedStr<'a>), ()> {
    fn decode_hex_digit(digit: u8) -> Option<u8> {
        match digit {
            b'0'..=b'9' => Some(digit - b'0'),
            b'a'..=b'f' => Some(digit - (b'a' - 10)),
            _ => None,
        }
    }

    terminated(lower_hex_digit0, tag('_'))
        .map_opt(|s: &str| {
            if s.len() % 2 == 0 {
                if let Some(s2) = s.as_bytes().get(1..) {
                    let mut bytes = Vec::with_capacity(s.len() / 2);

                    for (high, low) in s.bytes().zip(s2.iter().copied()).step_by(2) {
                        bytes.push((decode_hex_digit(high)? << 4) | decode_hex_digit(low)?);
                    }

                    String::from_utf8(bytes).ok()
                } else {
                    Some(String::new())
                }
            } else {
                None
            }
        })
        .parse(input, context)
}

fn parse_base62_number<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(u64, IndexedStr<'a>), ()> {
    terminated(alphanumeric0, tag('_'))
        .map_opt(|num: &str| {
            if num.is_empty() {
                Some(0)
            } else {
                let mut value = 0_u64;

                for c in num.bytes() {
                    let digit = match c {
                        b'0'..=b'9' => c - b'0',
                        b'a'..=b'z' => 10 + (c - b'a'),
                        _ => 36 + (c - b'A'),
                    };

                    value = value.checked_mul(62)?;
                    value = value.checked_add(digit.into())?;
                }

                value.checked_add(1)
            }
        })
        .parse(input, context)
}

fn parse_back_ref<'a>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(usize, IndexedStr<'a>), ()> {
    preceded(tag('B'), parse_base62_number)
        .map_opt(|num| num.try_into().ok())
        .parse(input, context)
}

fn parse_vendor_specific_suffix<'a>(
    input: IndexedStr<'a>,
    _: &mut Context<'a>,
) -> Result<(&'a str, IndexedStr<'a>), ()> {
    if input.data.starts_with(['.', '$']) {
        let length = input.data.len();

        Ok(input.split_at(length).unwrap())
    } else {
        Err(())
    }
}

fn parse_decimal_number<'a, T>(input: IndexedStr<'a>, context: &mut Context<'a>) -> Result<(T, IndexedStr<'a>), ()>
where
    T: PrimInt,
{
    or(tag('0'), digit1)
        .map_opt(|num: &str| T::from_str_radix(num, 10).ok())
        .parse(input, context)
}
