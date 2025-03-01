use crate::{Cursor, CursorExt, Parser};

pub struct Opt<P>
where
    P: ?Sized,
{
    parser: P,
}

impl<C, P> Parser<C> for Opt<P>
where
    C: Cursor + ?Sized,
    P: Parser<C> + ?Sized,
{
    type Output = Option<P::Output>;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        Ok(context.transaction(|context: &mut _| self.parser.parse(context)).ok())
    }
}

pub const fn opt<C, P>(parser: P) -> Opt<P>
where
    C: Cursor + ?Sized,
    P: Parser<C>,
{
    Opt { parser }
}
