use crate::Parser;

pub struct Preceded<P, Q>
where
    Q: ?Sized,
{
    left: P,
    right: Q,
}

impl<C, P, Q> Parser<C> for Preceded<P, Q>
where
    C: ?Sized,
    P: Parser<C>,
    Q: Parser<C> + ?Sized,
{
    type Output = Q::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        self.left.parse(context)?;

        self.right.parse(context)
    }
}

pub const fn preceded<C, P, Q>(left: P, right: Q) -> Preceded<P, Q>
where
    C: ?Sized,
    P: Parser<C>,
    Q: Parser<C>,
{
    Preceded { left, right }
}
