use crate::rust_v0::context::Context;
use crate::rust_v0::{
    Abi, BasicType, Const, DynBounds, DynTrait, DynTraitAssocBinding, FnSig, GenericArg, Identifier, ImplPath, Path,
    Symbol, Type,
};
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alphanumeric0, digit1, hex_digit0};
use nom::combinator::{map_opt, opt, recognize};
use nom::error::ParseError;
use nom::multi::many0;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{IResult, Parser};
use std::borrow::Cow;
use std::convert::TryInto;
use std::rc::Rc;
use std::str;

#[cfg(test)]
mod tests;

fn opt_u64<I: Clone, E: ParseError<I>>(parser: impl Parser<I, u64, E>) -> impl FnMut(I) -> IResult<I, u64, E> {
    map_opt(opt(parser), |num| {
        Some(match num {
            None => 0,
            Some(num) => num.checked_add(1)?,
        })
    })
}

// References:
//
// - https://github.com/alexcrichton/rustc-demangle/blob/main/src/v0.rs.
// - https://github.com/michaelwoerister/std-mangle-rs/blob/master/src/ast_demangle.rs.
// - https://github.com/rust-lang/rust/blob/master/compiler/rustc_symbol_mangling/src/v0.rs.
// - https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html.

pub fn parse_symbol<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Symbol<'a>> {
    tuple((opt(parse_decimal_number), parse_path, opt(parse_path)))
        .map(|(version, path, instantiating_crate)| Symbol {
            version,
            path,
            instantiating_crate,
        })
        .parse(context)
}

fn parse_path<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Rc<Path<'a>>> {
    alt((
        preceded(tag("C"), parse_identifier).map(Path::CrateRoot),
        preceded(tag("M"), parse_impl_path.and(parse_type))
            .map(|(impl_path, type_)| Path::InherentImpl { impl_path, type_ }),
        preceded(tag("X"), tuple((parse_impl_path, parse_type, parse_path))).map(|(impl_path, type_, trait_)| {
            Path::TraitImpl {
                impl_path,
                type_,
                trait_,
            }
        }),
        preceded(tag("Y"), parse_type.and(parse_path)).map(|(type_, trait_)| Path::TraitDefinition { type_, trait_ }),
        preceded(tag("N"), tuple((take(1_usize), parse_path, parse_identifier))).map(|(namespace, path, name)| {
            Path::Nested {
                namespace: namespace.data.as_bytes()[0],
                path,
                name,
            }
        }),
        delimited(tag("I"), parse_path.and(many0(parse_generic_arg)), tag("E"))
            .map(|(path, generic_args)| Path::Generic { path, generic_args }),
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
    .or(map_opt(parse_back_ref, |back_ref| {
        let mut back_ref_table = context.back_ref_table.borrow_mut();
        let result = Rc::clone(back_ref_table.paths.get(&back_ref)?);

        back_ref_table.paths.insert(context.index, Rc::clone(&result));

        Some(result)
    }))
    .parse(context)
}

fn parse_impl_path<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, ImplPath<'a>> {
    opt_u64(parse_disambiguator)
        .and(parse_path)
        .map(|(disambiguator, path)| ImplPath { disambiguator, path })
        .parse(context)
}

fn parse_identifier<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Identifier<'a>> {
    opt_u64(parse_disambiguator)
        .and(parse_undisambiguated_identifier)
        .map(|(disambiguator, name)| Identifier { disambiguator, name })
        .parse(context)
}

fn parse_disambiguator<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
    preceded(tag("s"), parse_base62_number).parse(context)
}

fn parse_undisambiguated_identifier<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Cow<'a, str>> {
    tuple((
        opt(tag("u")).map(|tag| tag.is_some()),
        parse_decimal_number,
        opt(tag("_")),
    ))
    .flat_map(|(is_punycode, length, _)| {
        map_opt(take(length), move |name: Context<'a, 'b>| {
            Some(if is_punycode {
                let mut buffer = name.data.as_bytes().to_vec();

                if let Some(c) = buffer.iter_mut().rfind(|&&mut c| c == b'_') {
                    *c = b'-';
                }

                Cow::Owned(punycode::decode(str::from_utf8(&buffer).ok()?).ok()?)
            } else {
                Cow::Borrowed(name.data)
            })
        })
    })
    .parse(context)
}

fn parse_generic_arg<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, GenericArg<'a>> {
    alt((
        parse_lifetime.map(GenericArg::Lifetime),
        parse_type.map(GenericArg::Type),
        preceded(tag("K"), parse_const).map(GenericArg::Const),
    ))
    .parse(context)
}

fn parse_lifetime<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
    preceded(tag("L"), parse_base62_number).parse(context)
}

fn parse_binder<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
    preceded(tag("G"), parse_base62_number).parse(context)
}

fn parse_type<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Rc<Type<'a>>> {
    alt((
        parse_basic_type.map(Type::Basic),
        parse_path.map(Type::Named),
        preceded(tag("A"), parse_type.and(parse_const)).map(|(type_, length)| Type::Array(type_, length)),
        preceded(tag("S"), parse_type).map(Type::Slice),
        delimited(tag("T"), many0(parse_type), tag("E")).map(Type::Tuple),
        preceded(
            tag("R"),
            opt(parse_lifetime).map(Option::unwrap_or_default).and(parse_type),
        )
        .map(|(lifetime, type_)| Type::Ref { lifetime, type_ }),
        preceded(
            tag("Q"),
            opt(parse_lifetime).map(Option::unwrap_or_default).and(parse_type),
        )
        .map(|(lifetime, type_)| Type::RefMut { lifetime, type_ }),
        preceded(tag("P"), parse_type).map(Type::PtrConst),
        preceded(tag("O"), parse_type).map(Type::PtrMut),
        preceded(tag("F"), parse_fn_sig).map(Type::Fn),
        preceded(tag("D"), parse_dyn_bounds.and(parse_lifetime))
            .map(|(dyn_bounds, lifetime)| Type::DynTrait { dyn_bounds, lifetime }),
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
    .or(map_opt(parse_back_ref, |back_ref| {
        let mut back_ref_table = context.back_ref_table.borrow_mut();
        let result = Rc::clone(back_ref_table.types.get(&back_ref)?);

        back_ref_table.types.insert(context.index, Rc::clone(&result));

        Some(result)
    }))
    .parse(context)
}

fn parse_basic_type<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, BasicType> {
    map_opt(take(1_usize), |s: Context<'a, 'b>| match s.data.as_bytes()[0] {
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

fn parse_fn_sig<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, FnSig<'a>> {
    tuple((
        opt_u64(parse_binder),
        opt(tag("U")).map(|u| u.is_some()),
        opt(preceded(tag("K"), parse_abi)),
        terminated(many0(parse_type), tag("E")),
        parse_type,
    ))
    .map(|(bound_lifetimes, is_unsafe, abi, argument_types, return_type)| FnSig {
        bound_lifetimes,
        is_unsafe,
        abi,
        argument_types,
        return_type,
    })
    .parse(context)
}

fn parse_abi<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Abi<'a>> {
    tag("C")
        .map(|_| Abi::C)
        .or(parse_undisambiguated_identifier.map(Abi::Named))
        .parse(context)
}

fn parse_dyn_bounds<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, DynBounds<'a>> {
    opt_u64(parse_binder)
        .and(terminated(many0(parse_dyn_trait), tag("E")))
        .map(|(bound_lifetimes, dyn_traits)| DynBounds {
            bound_lifetimes,
            dyn_traits,
        })
        .parse(context)
}

fn parse_dyn_trait<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, DynTrait<'a>> {
    parse_path
        .and(many0(parse_dyn_trait_assoc_binding))
        .map(|(path, dyn_trait_assoc_bindings)| DynTrait {
            path,
            dyn_trait_assoc_bindings,
        })
        .parse(context)
}

fn parse_dyn_trait_assoc_binding<'a, 'b>(
    context: Context<'a, 'b>,
) -> IResult<Context<'a, 'b>, DynTraitAssocBinding<'a>> {
    preceded(tag("p"), parse_undisambiguated_identifier.and(parse_type))
        .map(|(name, type_)| DynTraitAssocBinding { name, type_ })
        .parse(context)
}

fn parse_const<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, Rc<Const<'a>>> {
    map_opt(parse_back_ref, |back_ref| {
        let mut back_ref_table = context.back_ref_table.borrow_mut();
        let result = Rc::clone(back_ref_table.consts.get(&back_ref)?);

        back_ref_table.consts.insert(context.index, Rc::clone(&result));

        Some(result)
    })
    .or(alt((
        parse_type
            .and(terminated(recognize(opt(tag("n")).and(hex_digit0)), tag("_")))
            .map(|(type_, data)| Const::Data { type_, data: data.data }),
        tag("p").map(|_| Const::Placeholder),
    ))
    .map(|result| {
        let result = Rc::new(result);

        context
            .back_ref_table
            .borrow_mut()
            .consts
            .insert(context.index, Rc::clone(&result));

        result
    }))
    .parse(context)
}

fn parse_base62_number<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
    map_opt(terminated(alphanumeric0, tag("_")), |num: Context| {
        if num.data.is_empty() {
            Some(0)
        } else {
            let mut value = 0_u64;

            for c in num.data.bytes() {
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

fn parse_back_ref<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, usize> {
    map_opt(preceded(tag("B"), parse_base62_number), |num| num.try_into().ok()).parse(context)
}

fn parse_decimal_number<'a, 'b>(context: Context<'a, 'b>) -> IResult<Context<'a, 'b>, u64> {
    map_opt(tag("0").or(digit1), |num: Context<'a, 'b>| num.data.parse().ok()).parse(context)
}
