use crate::Parser;

pub struct Terminated<P, Q>
where
    P: ?Sized,
{
    right: Q,
    left: P,
}

impl<C, P, Q> Parser<C> for Terminated<P, Q>
where
    C: ?Sized,
    P: Parser<C> + ?Sized,
    Q: Parser<C>,
{
    type Output = P::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        let output = self.left.parse(context)?;

        self.right.parse(context)?;

        Ok(output)
    }
}

pub const fn terminated<C, P, Q>(left: P, right: Q) -> Terminated<P, Q>
where
    C: ?Sized,
    P: Parser<C>,
    Q: Parser<C>,
{
    Terminated { right, left }
}
