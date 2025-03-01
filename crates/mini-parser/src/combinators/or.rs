use crate::{Cursor, CursorExt, Parser};

pub struct Or<P, Q>
where
    Q: ?Sized,
{
    lhs: P,
    rhs: Q,
}

impl<C, P, Q> Parser<C> for Or<P, Q>
where
    C: Cursor + ?Sized,
    P: Parser<C>,
    Q: Parser<C, Output = P::Output> + ?Sized,
{
    type Output = Q::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        context
            .transaction(|context: &mut _| self.lhs.parse(context))
            .or_else(|()| self.rhs.parse(context))
    }
}

pub const fn or<C, P, Q>(lhs: P, rhs: Q) -> Or<P, Q>
where
    C: Cursor + ?Sized,
    P: Parser<C>,
    Q: Parser<C, Output = P::Output>,
{
    or_unchecked(lhs, rhs)
}

pub const fn or_unchecked<P, Q>(lhs: P, rhs: Q) -> Or<P, Q> {
    Or { lhs, rhs }
}
