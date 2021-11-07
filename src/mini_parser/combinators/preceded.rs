use crate::mini_parser::Parser;

pub struct Preceded<P, Q> {
    left: P,
    right: Q,
}

impl<I, C, P, Q> Parser<I, C> for Preceded<P, Q>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
{
    type Output = Q::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (_, input) = self.left.parse(input, context)?;

        self.right.parse(input, context)
    }
}

pub fn preceded<I, C, P, Q>(left: P, right: Q) -> Preceded<P, Q>
where
    P: Parser<I, C>,
    Q: Parser<I, C>,
{
    Preceded { left, right }
}
