use crate::{Cursor, CursorExt, Parser};

pub struct Many0<P>
where
    P: ?Sized,
{
    parser: P,
}

impl<C, P> Parser<C> for Many0<P>
where
    C: Cursor + ?Sized,
    P: Parser<C> + ?Sized,
{
    type Output = Vec<P::Output>;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        let mut output = Vec::new();

        while let Ok(item) = context.transaction(|context: &mut _| self.parser.parse(context)) {
            output.push(item);
        }

        Ok(output)
    }
}

pub const fn many0<C, P>(parser: P) -> Many0<P>
where
    C: Cursor + ?Sized,
    P: Parser<C>,
{
    Many0 { parser }
}
