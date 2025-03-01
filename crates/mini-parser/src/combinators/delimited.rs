use crate::Parser;

pub struct Delimited<P, Q, R>
where
    Q: ?Sized,
{
    left: P,
    right: R,
    middle: Q,
}

impl<C, P, Q, R> Parser<C> for Delimited<P, Q, R>
where
    C: ?Sized,
    P: Parser<C>,
    Q: Parser<C> + ?Sized,
    R: Parser<C>,
{
    type Output = Q::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        self.left.parse(context)?;

        let output = self.middle.parse(context)?;

        self.right.parse(context)?;

        Ok(output)
    }
}

pub const fn delimited<C, P, Q, R>(left: P, middle: Q, right: R) -> Delimited<P, Q, R>
where
    C: ?Sized,
    P: Parser<C>,
    Q: Parser<C>,
    R: Parser<C>,
{
    Delimited { left, right, middle }
}
