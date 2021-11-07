use crate::mini_parser::input::StripPrefix;
use crate::mini_parser::Parser;
use std::marker::PhantomData;

pub struct Tag<I, C, T> {
    tag: T,
    _phantom: PhantomData<fn(I, &mut C)>,
}

impl<I, C, T> Parser<I, C> for Tag<I, C, T>
where
    I: StripPrefix<T>,
    T: Clone,
{
    type Output = I::Prefix;

    fn parse(&mut self, input: I, _context: &mut C) -> Result<(Self::Output, I), ()> {
        input.strip_prefix(self.tag.clone()).ok_or(())
    }
}

pub fn tag<I, C, T>(tag: T) -> Tag<I, C, T> {
    Tag {
        tag,
        _phantom: PhantomData,
    }
}
