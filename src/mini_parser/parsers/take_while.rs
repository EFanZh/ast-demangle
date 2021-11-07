use crate::mini_parser::input::{Find, SplitAt};
use crate::mini_parser::Parser;
use std::marker::PhantomData;

pub struct TakeWhile<I, C, F> {
    f: F,
    _phantom: PhantomData<fn(I, &mut C)>,
}

impl<I, C, F> Parser<I, C> for TakeWhile<I, C, F>
where
    I: Find + SplitAt,
    F: FnMut(I::Item) -> bool,
{
    type Output = I::Prefix;

    fn parse(&mut self, input: I, _context: &mut C) -> Result<(Self::Output, I), ()> {
        let index = input.find(|c| !(self.f)(c));

        Ok(input.split_at(index).unwrap())
    }
}

pub fn take_while<I, C, F>(f: F) -> TakeWhile<I, C, F>
where
    I: Find + SplitAt,
    F: FnMut(I::Item) -> bool,
{
    TakeWhile {
        f,
        _phantom: PhantomData,
    }
}
