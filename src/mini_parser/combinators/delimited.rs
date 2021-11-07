use crate::mini_parser::Parser;

pub struct Delimited<P, Q, R> {
    left: P,
    middle: Q,
    right: R,
}

impl<I, C, P, Q, R> Parser<I, C> for Delimited<P, Q, R>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
    R: Parser<I, C>,
{
    type Output = Q::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (_, input) = self.left.parse(input, context)?;
        let (result, input) = self.middle.parse(input, context)?;
        let (_, input) = self.right.parse(input, context)?;

        Ok((result, input))
    }
}

pub fn delimited<I, C, P, Q, R>(left: P, middle: Q, right: R) -> Delimited<P, Q, R>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
    R: Parser<I, C>,
{
    Delimited { left, middle, right }
}
