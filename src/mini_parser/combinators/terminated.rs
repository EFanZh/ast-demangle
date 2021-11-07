use crate::mini_parser::Parser;

pub struct Terminated<P, Q> {
    left: P,
    right: Q,
}

impl<I, C, P, Q> Parser<I, C> for Terminated<P, Q>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
{
    type Output = P::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (output, input) = self.left.parse(input, context)?;
        let (_, input) = self.right.parse(input, context)?;

        Ok((output, input))
    }
}

pub fn terminated<I, C, P, Q>(left: P, right: Q) -> Terminated<P, Q>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
{
    Terminated { left, right }
}
