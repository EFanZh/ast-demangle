use crate::mini_parser::generic_tuple::{Tuple1, Tuple2};
use crate::mini_parser::Parser;

pub struct AltImpl<P, Q> {
    lhs: P,
    rhs: Q,
}

impl<I, C, P, Q> Parser<I, C> for AltImpl<P, Q>
where
    I: Clone,
    P: Parser<I, C>,
    Q: Parser<I, C, Output = P::Output>,
{
    type Output = P::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        self.lhs
            .parse(input.clone(), context)
            .or_else(|()| self.rhs.parse(input, context))
    }
}

pub trait AltBuilder {
    type Output;

    fn build(self) -> Self::Output;
}

impl<T> AltBuilder for (T,) {
    type Output = T;

    fn build(self) -> Self::Output {
        self.0
    }
}

impl<T> AltBuilder for T
where
    T: Tuple2,
    T::Rest: AltBuilder + Tuple1,
{
    type Output = AltImpl<T::First, <T::Rest as AltBuilder>::Output>;

    fn build(self) -> Self::Output {
        let (lhs, rhs) = self.split_first();

        AltImpl { lhs, rhs: rhs.build() }
    }
}

pub struct Alt<T>
where
    T: AltBuilder,
{
    inner: T::Output,
}

impl<I, C, T> Parser<I, C> for Alt<T>
where
    T: AltBuilder,
    T::Output: Parser<I, C>,
{
    type Output = <T::Output as Parser<I, C>>::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        self.inner.parse(input, context)
    }
}

pub fn alt<T>(parsers: T) -> Alt<T>
where
    T: AltBuilder,
{
    Alt { inner: parsers.build() }
}
