use crate::mini_parser::input::{Find, SplitAt};
use crate::mini_parser::{parsers, Parser};
use std::marker::PhantomData;

pub struct Digit1<I, C> {
    _phantom: PhantomData<fn(I, &mut C)>,
}

impl<I, C> Default for Digit1<I, C> {
    fn default() -> Self {
        Self { _phantom: PhantomData }
    }
}

impl<I, C> Parser<I, C> for Digit1<I, C>
where
    I: Find<Item = char> + SplitAt,
{
    type Output = I::Prefix;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        parsers::take_while1(|c: char| c.is_ascii_digit()).parse(input, context)
    }
}

pub fn digit1<I, C>(input: I, context: &mut C) -> Result<(I::Prefix, I), ()>
where
    I: Find<Item = char> + SplitAt,
{
    Digit1::default().parse(input, context)
}
