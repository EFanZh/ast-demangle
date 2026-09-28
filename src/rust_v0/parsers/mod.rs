use mini_parser::combinators::{alt, delimited, or, preceded, terminated, tuple};
use mini_parser::{Cursor, Parser, ParserExt};
use num_traits::{CheckedNeg, PrimInt};
use std::borrow::Cow;
use std::str;

#[cfg(test)]
mod tests;

const MAX_DEPTH: usize = 100;

pub trait Builder<'a>: 'a {
    type Symbol;
    type Path;
    type ImplPath;
    type Identifier;
    type GenericArg;
    type Type;
    type BasicType;
    type FnSig;
    type Abi;
    type DynBounds;
    type DynTrait;
    type Const;
    type ConstFields;

    // Symbol constructor.

    fn make_symbol(
        &mut self,
        encoding_version: Option<u64>,
        path: Self::Path,
        instantiating_crate: Option<Self::Path>,
        vendor_specific_suffix: Option<&'a str>,
    ) -> Self::Symbol;

    // Path constructors.

    fn make_crate_root_path(&mut self, identifier: Self::Identifier) -> Self::Path;
    fn make_inherent_impl_path(&mut self, impl_path: Self::ImplPath, r#type: Self::Type) -> Self::Path;

    fn make_trait_impl_path(
        &mut self,
        impl_path: Self::ImplPath,
        r#type: Self::Type,
        r#trait: Self::Path,
    ) -> Self::Path;

    fn make_trait_definition_path(&mut self, r#type: Self::Type, r#trait: Self::Path) -> Self::Path;
    fn make_nested_path(&mut self, namespace: u8, path: Self::Path, identifier: Self::Identifier) -> Self::Path;
    fn make_generic_path(&mut self, path: Self::Path, generic_args: Vec<Self::GenericArg>) -> Self::Path;

    // Impl path constructors.

    fn make_impl_path(&mut self, disambiguator: u64, path: Self::Path) -> Self::ImplPath;

    // Identifier constructor.

    fn make_identifier(&mut self, disambiguator: u64, name: Cow<'a, str>) -> Self::Identifier;

    // Generic argument constructors.

    fn make_lifetime_generic_arg(&mut self, lifetime: u64) -> Self::GenericArg;
    fn make_type_generic_arg(&mut self, r#type: Self::Type) -> Self::GenericArg;
    fn make_const_generic_arg(&mut self, value: Self::Const) -> Self::GenericArg;

    // Type constructors.

    fn make_basic_type_type(&mut self, basic_type: Self::BasicType) -> Self::Type;
    fn make_named_type(&mut self, path: Self::Path) -> Self::Type;
    fn make_array_type(&mut self, r#type: Self::Type, length: Self::Const) -> Self::Type;
    fn make_slice_type(&mut self, r#type: Self::Type) -> Self::Type;
    fn make_tuple_type(&mut self, types: Vec<Self::Type>) -> Self::Type;
    fn make_ref_type(&mut self, lifetime: u64, r#type: Self::Type) -> Self::Type;
    fn make_ref_mut_type(&mut self, lifetime: u64, r#type: Self::Type) -> Self::Type;
    fn make_ptr_const_type(&mut self, r#type: Self::Type) -> Self::Type;
    fn make_ptr_mut_type(&mut self, r#type: Self::Type) -> Self::Type;
    fn make_fn_type(&mut self, fn_sig: Self::FnSig) -> Self::Type;
    fn make_dyn_trait_type(&mut self, dyn_bounds: Self::DynBounds, lifetime: u64) -> Self::Type;

    // Basic type constructors.

    fn make_i8_basic_type(&mut self) -> Self::BasicType;
    fn make_bool_basic_type(&mut self) -> Self::BasicType;
    fn make_char_basic_type(&mut self) -> Self::BasicType;
    fn make_f64_basic_type(&mut self) -> Self::BasicType;
    fn make_str_basic_type(&mut self) -> Self::BasicType;
    fn make_f32_basic_type(&mut self) -> Self::BasicType;
    fn make_u8_basic_type(&mut self) -> Self::BasicType;
    fn make_isize_basic_type(&mut self) -> Self::BasicType;
    fn make_usize_basic_type(&mut self) -> Self::BasicType;
    fn make_i32_basic_type(&mut self) -> Self::BasicType;
    fn make_u32_basic_type(&mut self) -> Self::BasicType;
    fn make_i128_basic_type(&mut self) -> Self::BasicType;
    fn make_u128_basic_type(&mut self) -> Self::BasicType;
    fn make_i16_basic_type(&mut self) -> Self::BasicType;
    fn make_u16_basic_type(&mut self) -> Self::BasicType;
    fn make_unit_basic_type(&mut self) -> Self::BasicType;
    fn make_ellipsis_basic_type(&mut self) -> Self::BasicType;
    fn make_i64_basic_type(&mut self) -> Self::BasicType;
    fn make_u64_basic_type(&mut self) -> Self::BasicType;
    fn make_never_basic_type(&mut self) -> Self::BasicType;
    fn make_placeholder_basic_type(&mut self) -> Self::BasicType;

    // Function signature constructor.

    fn make_fn_sig(
        &mut self,
        bound_lifetimes: u64,
        is_unsafe: bool,
        abi: Option<Self::Abi>,
        argument_types: Vec<Self::Type>,
        return_type: Self::Type,
    ) -> Self::FnSig;

    // ABI constructors.

    fn make_c_abi(&mut self) -> Self::Abi;
    fn make_named_abi(&mut self, name: Cow<'a, str>) -> Self::Abi;

    // Dyn bounds constructor.

    fn make_dyn_bounds(&mut self, bound_lifetimes: u64, dyn_traits: Vec<Self::DynTrait>) -> Self::DynBounds;

    // Dyn trait constructor.

    fn make_dyn_trait(&mut self, path: Self::Path, assoc_bindings: Vec<(Cow<'a, str>, Self::Type)>) -> Self::DynTrait;

    // Const constructors.

    fn make_i8_const(&mut self, value: i8) -> Self::Const;
    fn make_u8_const(&mut self, value: u8) -> Self::Const;
    fn make_isize_const(&mut self, value: isize) -> Self::Const;
    fn make_usize_const(&mut self, value: usize) -> Self::Const;
    fn make_i32_const(&mut self, value: i32) -> Self::Const;
    fn make_u32_const(&mut self, value: u32) -> Self::Const;
    fn make_i128_const(&mut self, value: i128) -> Self::Const;
    fn make_u128_const(&mut self, value: u128) -> Self::Const;
    fn make_i16_const(&mut self, value: i16) -> Self::Const;
    fn make_u16_const(&mut self, value: u16) -> Self::Const;
    fn make_i64_const(&mut self, value: i64) -> Self::Const;
    fn make_u64_const(&mut self, value: u64) -> Self::Const;
    fn make_bool_const(&mut self, value: bool) -> Self::Const;
    fn make_char_const(&mut self, value: char) -> Self::Const;
    fn make_str_const(&mut self, value: String) -> Self::Const;
    fn make_ref_const(&mut self, value: Self::Const) -> Self::Const;
    fn make_ref_mut_const(&mut self, value: Self::Const) -> Self::Const;
    fn make_array_const(&mut self, values: Vec<Self::Const>) -> Self::Const;
    fn make_tuple_const(&mut self, values: Vec<Self::Const>) -> Self::Const;
    fn make_named_struct_const(&mut self, path: Self::Path, fields: Self::ConstFields) -> Self::Const;
    fn make_placeholder_const(&mut self) -> Self::Const;

    // Const fields constructors.

    fn make_unit_const_fields(&mut self) -> Self::ConstFields;
    fn make_tuple_const_fields(&mut self, values: Vec<Self::Const>) -> Self::ConstFields;
    fn make_struct_const_fields(&mut self, values: Vec<(Self::Identifier, Self::Const)>) -> Self::ConstFields;

    // Back reference cache.

    fn query_const(&mut self, index: usize) -> Option<Self::Const>;
    fn save_const(&mut self, index: usize, value: &Self::Const);
    fn query_path(&mut self, index: usize) -> Option<Self::Path>;
    fn save_path(&mut self, index: usize, value: &Self::Path);
    fn query_type(&mut self, index: usize) -> Option<Self::Type>;
    fn save_type(&mut self, index: usize, value: &Self::Type);
}

pub struct Context<'a, B>
where
    B: ?Sized,
{
    data: &'a str,
    index: usize,
    depth: usize,
    builder: B,
}

#[cfg(test)]
impl<'a, B> Context<'a, B> {
    const fn new(data: &'a str, builder: B) -> Self {
        Self {
            data,
            index: 0,
            depth: 0,
            builder,
        }
    }
}

impl<B> Cursor for Context<'_, B>
where
    B: ?Sized,
{
    type Cursor = usize;

    fn get_cursor(&mut self) -> Self::Cursor {
        self.index
    }

    fn set_cursor(&mut self, cursor: Self::Cursor) {
        self.index = cursor;
    }
}

// Primitive parsers.

fn digit1<'a, B>(context: &mut Context<'a, B>) -> Result<&'a str, ()>
where
    B: ?Sized,
{
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

fn tag<'a, B>(c: &str) -> impl Parser<Context<'a, B>, Output = &'a str>
where
    B: ?Sized,
{
    move |context: &mut Context<'a, B>| -> Result<&'a str, ()> {
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

fn take<'a, B>(length: usize) -> impl Parser<Context<'a, B>, Output = &'a str>
where
    B: ?Sized,
{
    move |context: &mut Context<'a, B>| -> Result<&'a str, ()> {
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

fn token<'a, B>(c: u8) -> impl Parser<Context<'a, B>, Output = u8>
where
    B: ?Sized,
{
    move |context: &mut Context<B>| {
        if context.data.as_bytes().get(context.index).copied() == Some(c) {
            context.index += 1;

            Ok(c)
        } else {
            Err(())
        }
    }
}

fn take_while<'a, B>(context: &mut Context<'a, B>, mut f: impl FnMut(u8) -> bool) -> Result<&'a str, ()>
where
    B: ?Sized,
{
    let s = context.data.get(context.index..).ok_or(())?;
    let length = s.bytes().take_while(|&c| f(c)).count();

    context.index += length;

    Ok(&s[..length])
}

fn alphanumeric0<'a, B>(context: &mut Context<'a, B>) -> Result<&'a str, ()>
where
    B: ?Sized,
{
    take_while(context, |c| c.is_ascii_alphanumeric())
}

fn lower_hex_digit0<'a, B>(context: &mut Context<'a, B>) -> Result<&'a str, ()>
where
    B: ?Sized,
{
    take_while(context, |c| matches!(c, b'0'..=b'9' | b'a'..=b'z'))
}

// Helper parsers.

fn opt_u64<'a, B>(parser: impl Parser<Context<'a, B>, Output = u64>) -> impl Parser<Context<'a, B>, Output = u64>
where
    B: ?Sized,
{
    parser
        .opt()
        .map_opt(|_, num: Option<u64>| num.map_or(Some(0), |num| num.checked_add(1)))
}

fn limit_recursion_depth<'a, B, T>(
    mut parser: impl Parser<Context<'a, B>, Output = T>,
) -> impl Parser<Context<'a, B>, Output = T>
where
    B: ?Sized,
{
    move |context: &mut Context<'a, B>| {
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

fn back_referenced<'a, B, T>(
    index: usize,
    base_parser: impl Parser<Context<'a, B>, Output = T>,
    mut query_fn: impl for<'b> FnMut(&'b mut B, usize) -> Option<T> + 'a,
    mut save_fn: impl for<'b> FnMut(&'b mut B, usize, &T) + 'a,
) -> impl Parser<Context<'a, B>, Output = T>
where
    B: Builder<'a> + ?Sized,
    T: 'a,
{
    limit_recursion_depth(
        or(
            base_parser,
            parse_back_ref.map_opt(move |context, back_ref| query_fn(&mut context.builder, back_ref)),
        )
        .inspect(move |context, result| save_fn(&mut context.builder, index, result)),
    )
}

// References:
//
// - <https://github.com/michaelwoerister/std-mangle-rs/blob/master/src/ast_demangle.rs>.
// - <https://github.com/rust-lang/rust/blob/main/compiler/rustc_symbol_mangling/src/v0.rs>.
// - <https://github.com/rust-lang/rust/blob/main/src/doc/rustc/src/symbol-mangling/v0.md>.
// - <https://github.com/rust-lang/rustc-demangle/blob/main/src/v0.rs>.
// - <https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html>.

pub fn parse_symbol<'a, B>(input: &'a str, builder: B) -> Result<(B::Symbol, &'a str), ()>
where
    B: Builder<'a>,
{
    let mut context = Context {
        data: input,
        index: 0,
        depth: 0,
        builder,
    };

    parse_symbol_inner(&mut context).map(|symbol| (symbol, &input[context.index..]))
}

fn parse_symbol_inner<'a, B>(context: &mut Context<'a, B>) -> Result<B::Symbol, ()>
where
    B: Builder<'a> + ?Sized,
{
    tuple((
        parse_decimal_number::<B, _>.opt(),
        parse_path,
        parse_path.opt(),
        parse_vendor_specific_suffix.opt(),
    ))
    .map(
        |context, (encoding_version, path, instantiating_crate, vendor_specific_suffix)| {
            context
                .builder
                .make_symbol(encoding_version, path, instantiating_crate, vendor_specific_suffix)
        },
    )
    .parse(context)
}

fn parse_path<'a, B>(context: &mut Context<'a, B>) -> Result<B::Path, ()>
where
    B: Builder<'a> + ?Sized,
{
    back_referenced(
        context.get_cursor(),
        alt((
            preceded(token::<B>(b'C'), parse_identifier)
                .map(|context, identifier| context.builder.make_crate_root_path(identifier)),
            preceded(token::<B>(b'M'), tuple((parse_impl_path, parse_type)))
                .map(|context, (impl_path, r#type)| context.builder.make_inherent_impl_path(impl_path, r#type)),
            preceded(token::<B>(b'X'), tuple((parse_impl_path, parse_type, parse_path))).map(
                |context, (impl_path, r#type, r#trait)| {
                    context.builder.make_trait_impl_path(impl_path, r#type, r#trait)
                },
            ),
            preceded(token::<B>(b'Y'), tuple((parse_type, parse_path)))
                .map(|context, (r#type, r#trait)| context.builder.make_trait_definition_path(r#type, r#trait)),
            preceded(token::<B>(b'N'), tuple((take(1), parse_path, parse_identifier))).map_opt(
                |context, (namespace, path, identifier): (&str, _, _)| {
                    namespace.as_bytes()[0].is_ascii_alphabetic().then(|| {
                        context
                            .builder
                            .make_nested_path(namespace.as_bytes()[0], path, identifier)
                    })
                },
            ),
            delimited(
                token::<B>(b'I'),
                tuple((parse_path, parse_generic_arg.many0())),
                token(b'E'),
            )
            .map(|context, (path, generic_args)| context.builder.make_generic_path(path, generic_args)),
        )),
        B::query_path,
        B::save_path,
    )
    .parse(context)
}

fn parse_impl_path<'a, B>(context: &mut Context<'a, B>) -> Result<B::ImplPath, ()>
where
    B: Builder<'a> + ?Sized,
{
    tuple((opt_u64(parse_disambiguator::<B>), parse_path))
        .map(|context, (disambiguator, path)| context.builder.make_impl_path(disambiguator, path))
        .parse(context)
}

fn parse_identifier<'a, B>(context: &mut Context<'a, B>) -> Result<B::Identifier, ()>
where
    B: Builder<'a> + ?Sized,
{
    tuple((opt_u64(parse_disambiguator::<B>), parse_undisambiguated_identifier))
        .map(|context, (disambiguator, name): (_, Cow<'a, _>)| context.builder.make_identifier(disambiguator, name))
        .parse(context)
}

fn parse_disambiguator<B>(context: &mut Context<B>) -> Result<u64, ()>
where
    B: ?Sized,
{
    preceded(token(b's'), parse_base62_number).parse(context)
}

fn parse_undisambiguated_identifier<'a, B>(context: &mut Context<'a, B>) -> Result<Cow<'a, str>, ()>
where
    B: ?Sized,
{
    tuple((token(b'u').opt(), parse_decimal_number, token(b'_').opt()))
        .flat_map(|(punycode, length, _): (Option<_>, _, _)| {
            let is_punycode = punycode.is_some();

            take(length).map_opt(move |_, name| {
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

fn parse_generic_arg<'a, B>(context: &mut Context<'a, B>) -> Result<B::GenericArg, ()>
where
    B: Builder<'a> + ?Sized,
{
    alt((
        parse_lifetime::<B>.map(|context, lifetime| context.builder.make_lifetime_generic_arg(lifetime)),
        parse_type::<B>.map(|context, r#type| context.builder.make_type_generic_arg(r#type)),
        preceded(token::<B>(b'K'), parse_const).map(|context, value| context.builder.make_const_generic_arg(value)),
    ))
    .parse(context)
}

fn parse_lifetime<B>(context: &mut Context<B>) -> Result<u64, ()>
where
    B: ?Sized,
{
    preceded(token(b'L'), parse_base62_number).parse(context)
}

fn parse_binder<B>(context: &mut Context<B>) -> Result<u64, ()>
where
    B: ?Sized,
{
    preceded(token(b'G'), parse_base62_number).parse(context)
}

fn parse_type<'a, B>(context: &mut Context<'a, B>) -> Result<B::Type, ()>
where
    B: Builder<'a> + ?Sized,
{
    back_referenced(
        context.get_cursor(),
        alt((
            parse_basic_type::<B>.map(|context, basic_type| context.builder.make_basic_type_type(basic_type)),
            parse_path::<B>.map(|context, path| context.builder.make_named_type(path)),
            preceded(token::<B>(b'A'), tuple((parse_type, parse_const)))
                .map(|context, (r#type, length)| context.builder.make_array_type(r#type, length)),
            preceded(token::<B>(b'S'), parse_type).map(|context, r#type| context.builder.make_slice_type(r#type)),
            delimited(token::<B>(b'T'), parse_type.many0(), token(b'E'))
                .map(|context, types| context.builder.make_tuple_type(types)),
            preceded(
                token::<B>(b'R'),
                tuple((parse_lifetime.opt().map(|_, x| x.unwrap_or_default()), parse_type)),
            )
            .map(|context, (lifetime, r#type)| context.builder.make_ref_type(lifetime, r#type)),
            preceded(
                token::<B>(b'Q'),
                tuple((parse_lifetime.opt().map(|_, x| x.unwrap_or_default()), parse_type)),
            )
            .map(|context, (lifetime, r#type)| context.builder.make_ref_mut_type(lifetime, r#type)),
            preceded(token::<B>(b'P'), parse_type).map(|context, r#type| context.builder.make_ptr_const_type(r#type)),
            preceded(token::<B>(b'O'), parse_type).map(|context, r#type| context.builder.make_ptr_mut_type(r#type)),
            preceded(token::<B>(b'F'), parse_fn_sig).map(|context, fn_sig| context.builder.make_fn_type(fn_sig)),
            preceded(token::<B>(b'D'), tuple((parse_dyn_bounds, parse_lifetime)))
                .map(|context, (dyn_bounds, lifetime)| context.builder.make_dyn_trait_type(dyn_bounds, lifetime)),
        )),
        B::query_type,
        B::save_type,
    )
    .parse(context)
}

fn parse_basic_type<'a, B>(context: &mut Context<'a, B>) -> Result<B::BasicType, ()>
where
    B: Builder<'a> + ?Sized,
{
    take::<B>(1)
        .map_opt(|context, s: &str| match s.as_bytes()[0] {
            b'a' => Some(context.builder.make_i8_basic_type()),
            b'b' => Some(context.builder.make_bool_basic_type()),
            b'c' => Some(context.builder.make_char_basic_type()),
            b'd' => Some(context.builder.make_f64_basic_type()),
            b'e' => Some(context.builder.make_str_basic_type()),
            b'f' => Some(context.builder.make_f32_basic_type()),
            b'h' => Some(context.builder.make_u8_basic_type()),
            b'i' => Some(context.builder.make_isize_basic_type()),
            b'j' => Some(context.builder.make_usize_basic_type()),
            b'l' => Some(context.builder.make_i32_basic_type()),
            b'm' => Some(context.builder.make_u32_basic_type()),
            b'n' => Some(context.builder.make_i128_basic_type()),
            b'o' => Some(context.builder.make_u128_basic_type()),
            b's' => Some(context.builder.make_i16_basic_type()),
            b't' => Some(context.builder.make_u16_basic_type()),
            b'u' => Some(context.builder.make_unit_basic_type()),
            b'v' => Some(context.builder.make_ellipsis_basic_type()),
            b'x' => Some(context.builder.make_i64_basic_type()),
            b'y' => Some(context.builder.make_u64_basic_type()),
            b'z' => Some(context.builder.make_never_basic_type()),
            b'p' => Some(context.builder.make_placeholder_basic_type()),
            _ => None,
        })
        .parse(context)
}

fn parse_fn_sig<'a, B>(context: &mut Context<'a, B>) -> Result<B::FnSig, ()>
where
    B: Builder<'a> + ?Sized,
{
    tuple((
        opt_u64(parse_binder::<B>),
        token(b'U').opt(),
        preceded(token(b'K'), parse_abi).opt(),
        terminated(parse_type.many0(), token(b'E')),
        parse_type,
    ))
    .map(
        |context, (bound_lifetimes, unsafe_tag, abi, argument_types, return_type): (_, Option<_>, Option<_>, _, _)| {
            context
                .builder
                .make_fn_sig(bound_lifetimes, unsafe_tag.is_some(), abi, argument_types, return_type)
        },
    )
    .parse(context)
}

fn parse_abi<'a, B>(context: &mut Context<'a, B>) -> Result<B::Abi, ()>
where
    B: Builder<'a> + ?Sized,
{
    const fn is_abi_name(name: &str) -> bool {
        !name.is_empty() && name.is_ascii()
    }

    alt((
        token::<B>(b'C').map(|context, _| context.builder.make_c_abi()),
        parse_undisambiguated_identifier::<B>
            .map_opt(|context, name: Cow<'a, _>| is_abi_name(&name).then_some(context.builder.make_named_abi(name))),
    ))
    .parse(context)
}

fn parse_dyn_bounds<'a, B>(context: &mut Context<'a, B>) -> Result<B::DynBounds, ()>
where
    B: Builder<'a> + ?Sized,
{
    tuple((
        opt_u64(parse_binder::<B>),
        terminated(parse_dyn_trait.many0(), token(b'E')),
    ))
    .map(|context, (bound_lifetimes, dyn_traits)| context.builder.make_dyn_bounds(bound_lifetimes, dyn_traits))
    .parse(context)
}

fn parse_dyn_trait<'a, B>(context: &mut Context<'a, B>) -> Result<B::DynTrait, ()>
where
    B: Builder<'a> + ?Sized,
{
    tuple((parse_path::<B>, parse_dyn_trait_assoc_binding.many0()))
        .map(|context, (path, dyn_trait_assoc_bindings)| context.builder.make_dyn_trait(path, dyn_trait_assoc_bindings))
        .parse(context)
}

fn parse_dyn_trait_assoc_binding<'a, B>(context: &mut Context<'a, B>) -> Result<(Cow<'a, str>, B::Type), ()>
where
    B: Builder<'a> + ?Sized,
{
    preceded(token(b'p'), tuple((parse_undisambiguated_identifier, parse_type))).parse(context)
}

fn parse_const<'a, B>(context: &mut Context<'a, B>) -> Result<B::Const, ()>
where
    B: Builder<'a> + ?Sized,
{
    let index = context.get_cursor();

    back_referenced(
        index,
        alt((
            preceded(token::<B>(b'a'), parse_const_int).map(|context, value| context.builder.make_i8_const(value)),
            preceded(token::<B>(b'h'), parse_const_int).map(|context, value| context.builder.make_u8_const(value)),
            preceded(token::<B>(b'i'), parse_const_int).map(|context, value| context.builder.make_isize_const(value)),
            preceded(token::<B>(b'j'), parse_const_int).map(|context, value| context.builder.make_usize_const(value)),
            preceded(token::<B>(b'l'), parse_const_int).map(|context, value| context.builder.make_i32_const(value)),
            preceded(token::<B>(b'm'), parse_const_int).map(|context, value| context.builder.make_u32_const(value)),
            preceded(token::<B>(b'n'), parse_const_int).map(|context, value| context.builder.make_i128_const(value)),
            preceded(token::<B>(b'o'), parse_const_int).map(|context, value| context.builder.make_u128_const(value)),
            preceded(token::<B>(b's'), parse_const_int).map(|context, value| context.builder.make_i16_const(value)),
            preceded(token::<B>(b't'), parse_const_int).map(|context, value| context.builder.make_u16_const(value)),
            preceded(token::<B>(b'x'), parse_const_int).map(|context, value| context.builder.make_i64_const(value)),
            preceded(token::<B>(b'y'), parse_const_int).map(|context, value| context.builder.make_u64_const(value)),
            preceded(token::<B>(b'b'), parse_const_int::<B, u8>).map_opt(|context, result| match result {
                0 => Some(context.builder.make_bool_const(false)),
                1 => Some(context.builder.make_bool_const(true)),
                _ => None,
            }),
            preceded(token::<B>(b'c'), parse_const_int).map_opt(|context, result: u32| {
                result
                    .try_into()
                    .ok()
                    .map(|value| context.builder.make_char_const(value))
            }),
            preceded(token::<B>(b'e'), parse_const_str).map(|context, value| context.builder.make_str_const(value)),
            preceded(token::<B>(b'R'), parse_const).map(|context, value| context.builder.make_ref_const(value)),
            preceded(token::<B>(b'Q'), parse_const).map(|context, value| context.builder.make_ref_mut_const(value)),
            delimited(token::<B>(b'A'), parse_const.many0(), token(b'E'))
                .map(|context, values| context.builder.make_array_const(values)),
            delimited(token::<B>(b'T'), parse_const.many0(), token(b'E'))
                .map(|context, values| context.builder.make_tuple_const(values)),
            preceded(token::<B>(b'V'), tuple((parse_path, parse_const_fields)))
                .map(|context, (path, fields)| context.builder.make_named_struct_const(path, fields)),
            token::<B>(b'p').map(|context, _| context.builder.make_placeholder_const()),
        )),
        B::query_const,
        B::save_const,
    )
    .parse(context)
}

fn parse_const_fields<'a, B>(context: &mut Context<'a, B>) -> Result<B::ConstFields, ()>
where
    B: Builder<'a> + ?Sized,
{
    alt((
        token::<B>(b'U').map(|context, _| context.builder.make_unit_const_fields()),
        delimited(token::<B>(b'T'), parse_const.many0(), token(b'E'))
            .map(|context, values| context.builder.make_tuple_const_fields(values)),
        delimited(
            token::<B>(b'S'),
            tuple((parse_identifier, parse_const)).many0(),
            token(b'E'),
        )
        .map(|context, values| context.builder.make_struct_const_fields(values)),
    ))
    .parse(context)
}

fn parse_const_int<B, T>(context: &mut Context<B>) -> Result<T, ()>
where
    B: ?Sized,
    T: CheckedNeg + PrimInt,
{
    terminated(
        tuple((token(b'n').opt(), lower_hex_digit0)).map_opt(|_, (is_negative, data): (Option<_>, &str)| {
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

fn parse_const_str<B>(context: &mut Context<B>) -> Result<String, ()>
where
    B: ?Sized,
{
    const fn decode_hex_digit(digit: u8) -> Option<u8> {
        match digit {
            b'0'..=b'9' => Some(digit - b'0'),
            b'a'..=b'f' => Some(digit - (b'a' - 10)),
            _ => None,
        }
    }

    terminated(lower_hex_digit0, token(b'_'))
        .map_opt(|_, s| {
            if s.len().is_multiple_of(2) {
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

fn parse_base62_number<B>(context: &mut Context<B>) -> Result<u64, ()>
where
    B: ?Sized,
{
    terminated(alphanumeric0, tag("_"))
        .map_opt(|_, num| {
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

fn parse_back_ref<B>(context: &mut Context<B>) -> Result<usize, ()>
where
    B: ?Sized,
{
    preceded(token(b'B'), parse_base62_number)
        .map_opt(|_, num: u64| num.try_into().ok())
        .parse(context)
}

fn parse_vendor_specific_suffix<'a, B>(context: &mut Context<'a, B>) -> Result<&'a str, ()>
where
    B: ?Sized,
{
    if matches!(context.data.as_bytes().get(context.index), Some(b'.' | b'$')) {
        let result = &context.data[context.index..];

        context.index = context.data.len();

        Ok(result)
    } else {
        Err(())
    }
}

fn parse_decimal_number<B, T>(context: &mut Context<B>) -> Result<T, ()>
where
    B: ?Sized,
    T: PrimInt,
{
    or(tag("0"), digit1)
        .map_opt(|_, num| T::from_str_radix(num, 10).ok())
        .parse(context)
}
