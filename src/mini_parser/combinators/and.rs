use crate::mini_parser::combinators::tuple::{self, Tuple};
use crate::mini_parser::Parser;

pub struct And<P, Q> {
    inner: Tuple<(P, Q)>,
}

impl<I, C, P, Q> Parser<I, C> for And<P, Q>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
{
    type Output = <Tuple<(P, Q)> as Parser<I, C>>::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        self.inner.parse(input, context)
    }
}

pub fn and<I, C, P, Q>(lhs: P, rhs: Q) -> And<P, Q>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
{
    And {
        inner: tuple::tuple((lhs, rhs)),
    }
}
