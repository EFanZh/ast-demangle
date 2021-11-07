use crate::mini_parser::generic_tuple::{Tuple1, TupleAppend};
use crate::mini_parser::Parser;

pub struct TupleImpl<P, Q> {
    lhs: P,
    rhs: Q,
}

pub trait AccParser<A, I, C> {
    type Output;

    fn parse(&mut self, acc: A, input: I, context: &mut C) -> Result<(Self::Output, I), ()>;
}

impl<A, I, C> AccParser<A, I, C> for () {
    type Output = A;

    fn parse(&mut self, acc: A, input: I, _context: &mut C) -> Result<(Self::Output, I), ()> {
        Ok((acc, input))
    }
}

impl<A, I, C, P, Q> AccParser<A, I, C> for TupleImpl<P, Q>
where
    A: TupleAppend<P::Output>,
    P: Parser<I, C>,
    Q: AccParser<A::Output, I, C>,
{
    type Output = Q::Output;

    fn parse(&mut self, acc: A, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (output, input) = self.lhs.parse(input, context)?;

        self.rhs.parse(acc.append(output), input, context)
    }
}

pub trait TupleBuilder {
    type Output;

    fn build(self) -> Self::Output;
}

impl TupleBuilder for () {
    type Output = ();

    fn build(self) -> Self::Output {}
}

impl<T> TupleBuilder for T
where
    T: Tuple1,
    T::Rest: TupleBuilder,
{
    type Output = TupleImpl<T::First, <T::Rest as TupleBuilder>::Output>;

    fn build(self) -> Self::Output {
        let (lhs, rhs) = self.split_first();

        TupleImpl { lhs, rhs: rhs.build() }
    }
}

pub struct Tuple<T>
where
    T: TupleBuilder,
{
    inner: T::Output,
}

impl<I, C, T> Parser<I, C> for Tuple<T>
where
    T: TupleBuilder,
    T::Output: AccParser<(), I, C>,
{
    type Output = <T::Output as AccParser<(), I, C>>::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        self.inner.parse((), input, context)
    }
}

pub fn tuple<T>(parsers: T) -> Tuple<T>
where
    T: TupleBuilder,
{
    Tuple { inner: parsers.build() }
}
