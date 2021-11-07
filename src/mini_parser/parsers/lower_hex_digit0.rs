use crate::mini_parser::input::{Find, SplitAt};
use crate::mini_parser::{parsers, Parser};
use std::marker::PhantomData;

pub struct LowerHexDigit0<I, C> {
    _phantom: PhantomData<fn(I, &mut C)>,
}

impl<I, C> Default for LowerHexDigit0<I, C> {
    fn default() -> Self {
        Self { _phantom: PhantomData }
    }
}

impl<I, C> Parser<I, C> for LowerHexDigit0<I, C>
where
    I: Find<Item = char> + SplitAt,
{
    type Output = I::Prefix;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        parsers::take_while(|c| matches!(c, '0'..='9' | 'a'..='z')).parse(input, context)
    }
}

pub fn lower_hex_digit0<I, C>(input: I, context: &mut C) -> Result<(I::Prefix, I), ()>
where
    I: Find<Item = char> + SplitAt,
{
    LowerHexDigit0::default().parse(input, context)
}
