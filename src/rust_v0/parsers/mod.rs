use crate::rust_v0::{
    Abi, BasicType, Const, ConstFields, DynBounds, DynTrait, FnSig, GenericArg, Identifier, ImplPath, Path, Symbol,
    Type,
};
use mini_parser::combinators::{alt, delimited, or, preceded, terminated, tuple};
use mini_parser::{Cursor, Parser, ParserExt};
use num_traits::{CheckedNeg, PrimInt};
use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;
use std::str;

#[cfg(test)]
mod tests;

const MAX_DEPTH: usize = 100;

struct Context<'a> {
    data: &'a str,
    index: usize,
    paths: HashMap<usize, Rc<Path<'a>>>,
    types: HashMap<usize, Rc<Type<'a>>>,
    consts: HashMap<usize, Rc<Const<'a>>>,
    depth: usize,
}

impl<'a> Context<'a> {
    fn new(data: &'a str) -> Self {
        Self {
            data,
            index: 0,
            paths: HashMap::new(),
            types: HashMap::new(),
            consts: HashMap::new(),
            depth: 0,
        }
    }
}

impl Cursor for Context<'_> {
    type Cursor = usize;

    fn get_cursor(&mut self) -> Self::Cursor {
        self.index
    }

    fn set_cursor(&mut self, cursor: Self::Cursor) {
        self.index = cursor;
    }
}

// Primitive parsers.

fn digit1<'a>(context: &mut Context<'a>) -> Result<&'a str, ()> {
    let data = &context.data[context.index..];
    let length = data.bytes().take_while(u8::is_ascii_digit).count();

    if length == 0 {
        Err(())
    } else {
        let result = &data[..length];

        context.index += length;

        Ok(result)
    }
}

fn tag<'a>(c: &str) -> impl Parser<Context<'a>, Output = &'a str> {
    move |context: &mut Context<'a>| -> Result<&'a str, ()> {
        context
            .index
            .checked_add(c.len())
            .and_then(|end| {
                let result = context.data.get(context.index..end);

                if result == Some(c) {
                    context.index = end;

                    result
                } else {
                    None
                }
            })
            .ok_or(())
    }
}

fn take<'a>(length: usize) -> impl Parser<Context<'a>, Output = &'a str> {
    move |context: &mut Context<'a>| -> Result<&'a str, ()> {
        context
            .index
            .checked_add(length)
            .and_then(|end| {
                let result = context.data.get(context.index..end);

                if result.is_some() {
                    context.index = end;
                }

                result
            })
            .ok_or(())
    }
}

fn token<'a>(c: u8) -> impl Parser<Context<'a>, Output = u8> {
    move |context: &mut Context| {
        if context.data.as_bytes().get(context.index).copied() == Some(c) {
            context.index += 1;

            Ok(c)
        } else {
            Err(())
        }
    }
}

fn take_while<'a>(context: &mut Context<'a>, mut f: impl FnMut(u8) -> bool) -> Result<&'a str, ()> {
    let s = context.data.get(context.index..).ok_or(())?;
    let length = s.bytes().take_while(|&c| f(c)).count();

    context.index += length;

    Ok(&s[..length])
}

fn alphanumeric0<'a>(context: &mut Context<'a>) -> Result<&'a str, ()> {
    take_while(context, |c| c.is_ascii_alphanumeric())
}

fn lower_hex_digit0<'a>(context: &mut Context<'a>) -> Result<&'a str, ()> {
    take_while(context, |c| matches!(c, b'0'..=b'9' | b'a'..=b'z'))
}

// Helper parsers.

fn opt_u64<'a>(parser: impl Parser<Context<'a>, Output = u64>) -> impl Parser<Context<'a>, Output = u64> {
    parser
        .opt()
        .map_opt(|_: &mut _, num: Option<u64>| num.map_or(Some(0), |num| num.checked_add(1)))
}

fn limit_recursion_depth<'a, T>(
    mut parser: impl Parser<Context<'a>, Output = T>,
) -> impl Parser<Context<'a>, Output = T> {
    move |context: &mut Context<'a>| {
        if context.depth < MAX_DEPTH {
            context.depth += 1;

            let result = parser.parse(context);

            context.depth -= 1;

            result
        } else {
            Err(())
        }
    }
}

fn back_referenced<'a, T>(
    index: usize,
    base_parser: impl Parser<Context<'a>, Output = T>,
    mut get_table_fn: impl for<'b> FnMut(&'b mut Context<'a>) -> &'b mut HashMap<usize, Rc<T>> + Copy,
) -> impl Parser<Context<'a>, Output = Rc<T>>
where
    T: 'a,
{
    limit_recursion_depth(
        or(
            base_parser.map(Rc::new),
            parse_back_ref.map_opt(move |context: &mut _, back_ref| get_table_fn(context).get(&back_ref).cloned()),
        )
        .inspect(move |context: &mut _, result: &_| {
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
    let mut context = Context::new(input);

    parse_symbol_inner(&mut context).map(|symbol| (symbol, &input[context.index..]))
}

fn parse_symbol_inner<'a>(context: &mut Context<'a>) -> Result<Symbol<'a>, ()> {
    tuple((
        parse_decimal_number.opt(),
        parse_path,
        parse_path.opt(),
        parse_vendor_specific_suffix.opt(),
    ))
    .map(
        |(encoding_version, path, instantiating_crate, vendor_specific_suffix)| Symbol {
            encoding_version,
            path,
            instantiating_crate,
            vendor_specific_suffix,
        },
    )
    .parse(context)
}

fn parse_path<'a>(context: &mut Context<'a>) -> Result<Rc<Path<'a>>, ()> {
    back_referenced(
        context.get_cursor(),
        alt((
            preceded(token(b'C'), parse_identifier).map(Path::CrateRoot),
            preceded(token(b'M'), tuple((parse_impl_path, parse_type)))
                .map(|(impl_path, r#type)| Path::InherentImpl { impl_path, r#type }),
            preceded(token(b'X'), tuple((parse_impl_path, parse_type, parse_path))).map(
                |(impl_path, r#type, r#trait)| Path::TraitImpl {
                    impl_path,
                    r#type,
                    r#trait,
                },
            ),
            preceded(token(b'Y'), tuple((parse_type, parse_path)))
                .map(|(r#type, r#trait)| Path::TraitDefinition { r#type, r#trait }),
            preceded(token(b'N'), tuple((take(1), parse_path, parse_identifier))).map_opt(
                |_: &mut _, (namespace, path, identifier): (&str, _, _)| {
                    namespace.as_bytes()[0].is_ascii_alphabetic().then(|| Path::Nested {
                        namespace: namespace.as_bytes()[0],
                        path,
                        identifier,
                    })
                },
            ),
            delimited(token(b'I'), tuple((parse_path, parse_generic_arg.many0())), token(b'E'))
                .map(|(path, generic_args)| Path::Generic { path, generic_args }),
        )),
        |context| &mut context.paths,
    )
    .parse(context)
}

fn parse_impl_path<'a>(context: &mut Context<'a>) -> Result<ImplPath<'a>, ()> {
    tuple((opt_u64(parse_disambiguator), parse_path))
        .map(|(disambiguator, path)| ImplPath { disambiguator, path })
        .parse(context)
}

fn parse_identifier<'a>(context: &mut Context<'a>) -> Result<Identifier<'a>, ()> {
    tuple((opt_u64(parse_disambiguator), parse_undisambiguated_identifier))
        .map(|(disambiguator, name): (_, Cow<'a, _>)| Identifier { disambiguator, name })
        .parse(context)
}

fn parse_disambiguator(context: &mut Context) -> Result<u64, ()> {
    preceded(token(b's'), parse_base62_number).parse(context)
}

fn parse_undisambiguated_identifier<'a>(context: &mut Context<'a>) -> Result<Cow<'a, str>, ()> {
    tuple((token(b'u').opt(), parse_decimal_number, token(b'_').opt()))
        .flat_map(|(punycode, length, _): (Option<_>, _, _)| {
            let is_punycode = punycode.is_some();

            take(length).map_opt(move |_: &mut _, name: &'a str| {
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
        .parse(context)
}

fn parse_generic_arg<'a>(context: &mut Context<'a>) -> Result<GenericArg<'a>, ()> {
    alt((
        parse_lifetime.map(GenericArg::Lifetime),
        parse_type.map(GenericArg::Type),
        preceded(token(b'K'), parse_const).map(GenericArg::Const),
    ))
    .parse(context)
}

fn parse_lifetime(context: &mut Context) -> Result<u64, ()> {
    preceded(token(b'L'), parse_base62_number).parse(context)
}

fn parse_binder(context: &mut Context) -> Result<u64, ()> {
    preceded(token(b'G'), parse_base62_number).parse(context)
}

fn parse_type<'a>(context: &mut Context<'a>) -> Result<Rc<Type<'a>>, ()> {
    back_referenced(
        context.get_cursor(),
        alt((
            parse_basic_type.map(Type::Basic),
            parse_path.map(Type::Named),
            preceded(token(b'A'), tuple((parse_type, parse_const))).map(|(r#type, length)| Type::Array(r#type, length)),
            preceded(token(b'S'), parse_type).map(Type::Slice),
            delimited(token(b'T'), parse_type.many0(), token(b'E')).map(Type::Tuple),
            preceded(
                token(b'R'),
                tuple((parse_lifetime.opt().map(Option::unwrap_or_default), parse_type)),
            )
            .map(|(lifetime, r#type)| Type::Ref { lifetime, r#type }),
            preceded(
                token(b'Q'),
                tuple((parse_lifetime.opt().map(Option::unwrap_or_default), parse_type)),
            )
            .map(|(lifetime, r#type)| Type::RefMut { lifetime, r#type }),
            preceded(token(b'P'), parse_type).map(Type::PtrConst),
            preceded(token(b'O'), parse_type).map(Type::PtrMut),
            preceded(token(b'F'), parse_fn_sig).map(Type::Fn),
            preceded(token(b'D'), tuple((parse_dyn_bounds, parse_lifetime)))
                .map(|(dyn_bounds, lifetime)| Type::DynTrait { dyn_bounds, lifetime }),
        )),
        |context| &mut context.types,
    )
    .parse(context)
}

fn parse_basic_type(context: &mut Context) -> Result<BasicType, ()> {
    take(1)
        .map_opt(|_: &mut _, s: &str| match s.as_bytes()[0] {
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
        .parse(context)
}

fn parse_fn_sig<'a>(context: &mut Context<'a>) -> Result<FnSig<'a>, ()> {
    tuple((
        opt_u64(parse_binder),
        token(b'U').opt(),
        preceded(token(b'K'), parse_abi).opt(),
        terminated(parse_type.many0(), token(b'E')),
        parse_type,
    ))
    .map(
        |(bound_lifetimes, unsafe_tag, abi, argument_types, return_type): (_, Option<_>, Option<_>, _, _)| FnSig {
            bound_lifetimes,
            is_unsafe: unsafe_tag.is_some(),
            abi,
            argument_types,
            return_type,
        },
    )
    .parse(context)
}

fn parse_abi<'a>(context: &mut Context<'a>) -> Result<Abi<'a>, ()> {
    const fn is_abi_name(name: &str) -> bool {
        !name.is_empty() && name.is_ascii()
    }

    alt((
        token(b'C').map(|_| Abi::C),
        parse_undisambiguated_identifier
            .map_opt(|_: &mut _, id: Cow<'a, _>| is_abi_name(&id).then_some(Abi::Named(id))),
    ))
    .parse(context)
}

fn parse_dyn_bounds<'a>(context: &mut Context<'a>) -> Result<DynBounds<'a>, ()> {
    tuple((opt_u64(parse_binder), terminated(parse_dyn_trait.many0(), token(b'E'))))
        .map(|(bound_lifetimes, dyn_traits)| DynBounds {
            bound_lifetimes,
            dyn_traits,
        })
        .parse(context)
}

fn parse_dyn_trait<'a>(context: &mut Context<'a>) -> Result<DynTrait<'a>, ()> {
    tuple((parse_path, parse_dyn_trait_assoc_binding.many0()))
        .map(|(path, dyn_trait_assoc_bindings)| DynTrait {
            path,
            dyn_trait_assoc_bindings,
        })
        .parse(context)
}

fn parse_dyn_trait_assoc_binding<'a>(context: &mut Context<'a>) -> Result<(Cow<'a, str>, Rc<Type<'a>>), ()> {
    preceded(token(b'p'), tuple((parse_undisambiguated_identifier, parse_type))).parse(context)
}

fn parse_const<'a>(context: &mut Context<'a>) -> Result<Rc<Const<'a>>, ()> {
    let index = context.get_cursor();

    back_referenced(
        index,
        alt((
            preceded(token(b'a'), parse_const_int).map(Const::I8),
            preceded(token(b'h'), parse_const_int).map(Const::U8),
            preceded(token(b'i'), parse_const_int).map(Const::Isize),
            preceded(token(b'j'), parse_const_int).map(Const::Usize),
            preceded(token(b'l'), parse_const_int).map(Const::I32),
            preceded(token(b'm'), parse_const_int).map(Const::U32),
            preceded(token(b'n'), parse_const_int).map(Const::I128),
            preceded(token(b'o'), parse_const_int).map(Const::U128),
            preceded(token(b's'), parse_const_int).map(Const::I16),
            preceded(token(b't'), parse_const_int).map(Const::U16),
            preceded(token(b'x'), parse_const_int).map(Const::I64),
            preceded(token(b'y'), parse_const_int).map(Const::U64),
            preceded(token(b'b'), parse_const_int::<u8>).map_opt(|_: &mut _, result| match result {
                0 => Some(Const::Bool(false)),
                1 => Some(Const::Bool(true)),
                _ => None,
            }),
            preceded(token(b'c'), parse_const_int)
                .map_opt(|_: &mut _, result: u32| result.try_into().ok().map(Const::Char)),
            preceded(token(b'e'), parse_const_str).map(Const::Str),
            preceded(token(b'R'), parse_const).map(Const::Ref),
            preceded(token(b'Q'), parse_const).map(Const::RefMut),
            delimited(token(b'A'), parse_const.many0(), token(b'E')).map(Const::Array),
            delimited(token(b'T'), parse_const.many0(), token(b'E')).map(Const::Tuple),
            preceded(token(b'V'), tuple((parse_path, parse_const_fields)))
                .map(|(path, fields)| Const::NamedStruct { path, fields }),
            ParserExt::<Context>::map(token(b'p'), |_| Const::Placeholder),
        )),
        |context| &mut context.consts,
    )
    .parse(context)
}

fn parse_const_fields<'a>(context: &mut Context<'a>) -> Result<ConstFields<'a>, ()> {
    alt((
        ParserExt::<Context>::map(token(b'U'), |_| ConstFields::Unit),
        delimited(token(b'T'), parse_const.many0(), token(b'E')).map(ConstFields::Tuple),
        delimited(token(b'S'), tuple((parse_identifier, parse_const)).many0(), token(b'E')).map(ConstFields::Struct),
    ))
    .parse(context)
}

fn parse_const_int<T>(context: &mut Context) -> Result<T, ()>
where
    T: CheckedNeg + PrimInt,
{
    terminated(
        tuple((token(b'n').opt(), lower_hex_digit0)).map_opt(|_: &mut _, (is_negative, data): (Option<_>, &str)| {
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
        token(b'_'),
    )
    .parse(context)
}

fn parse_const_str(context: &mut Context) -> Result<String, ()> {
    const fn decode_hex_digit(digit: u8) -> Option<u8> {
        match digit {
            b'0'..=b'9' => Some(digit - b'0'),
            b'a'..=b'f' => Some(digit - (b'a' - 10)),
            _ => None,
        }
    }

    terminated(lower_hex_digit0, token(b'_'))
        .map_opt(|_: &mut _, s: &str| {
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
        .parse(context)
}

fn parse_base62_number(context: &mut Context) -> Result<u64, ()> {
    terminated(alphanumeric0, tag("_"))
        .map_opt(|_: &mut _, num: &str| {
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
        .parse(context)
}

fn parse_back_ref(context: &mut Context) -> Result<usize, ()> {
    preceded(token(b'B'), parse_base62_number)
        .map_opt(|_: &mut _, num: u64| num.try_into().ok())
        .parse(context)
}

fn parse_vendor_specific_suffix<'a>(context: &mut Context<'a>) -> Result<&'a str, ()> {
    if matches!(context.data.as_bytes().get(context.index), Some(b'.' | b'$')) {
        let result = &context.data[context.index..];

        context.index = context.data.len();

        Ok(result)
    } else {
        Err(())
    }
}

fn parse_decimal_number<T>(context: &mut Context) -> Result<T, ()>
where
    T: PrimInt,
{
    or(tag("0"), digit1)
        .map_opt(|_: &mut _, num: &str| T::from_str_radix(num, 10).ok())
        .parse(context)
}
