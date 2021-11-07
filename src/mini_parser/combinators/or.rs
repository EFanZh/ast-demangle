use crate::mini_parser::combinators::alt::{self, Alt};
use crate::mini_parser::Parser;

pub struct Or<P, Q> {
    inner: Alt<(P, Q)>,
}

impl<I, C, P, Q> Parser<I, C> for Or<P, Q>
where
    I: Clone,
    P: Parser<I, C>,
    Q: Parser<I, C, Output = P::Output>,
{
    type Output = P::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        Parser::parse(&mut self.inner, input, context)
    }
}

pub fn or<I, C, P, Q>(lhs: P, rhs: Q) -> Or<P, Q>
where
    I: Clone,
    P: Parser<I, C>,
    Q: Parser<I, C, Output = P::Output>,
{
    Or {
        inner: alt::alt((lhs, rhs)),
    }
}
