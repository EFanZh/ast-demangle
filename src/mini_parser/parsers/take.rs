use std::marker::PhantomData;

use crate::mini_parser::input::SplitAt;
use crate::mini_parser::Parser;

pub struct Take<I, C> {
    length: usize,
    _phantom: PhantomData<fn(I, &mut C)>,
}

impl<I, C> Parser<I, C> for Take<I, C>
where
    I: SplitAt,
{
    type Output = I::Prefix;

    fn parse(&mut self, input: I, _context: &mut C) -> Result<(Self::Output, I), ()> {
        input.split_at(self.length).ok_or(())
    }
}

pub fn take<I, C>(length: usize) -> Take<I, C>
where
    I: SplitAt,
{
    Take {
        length,
        _phantom: PhantomData,
    }
}
