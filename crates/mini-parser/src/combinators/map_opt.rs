use crate::Parser;

pub struct MapOpt<P, F>
where
    F: ?Sized,
{
    parser: P,
    f: F,
}

impl<C, P, F, U> Parser<C> for MapOpt<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(&mut C, P::Output) -> Option<U> + ?Sized,
{
    type Output = U;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        let output = self.parser.parse(context)?;

        (self.f)(context, output).ok_or(())
    }
}

pub const fn map_opt<C, P, F, U>(parser: P, f: F) -> MapOpt<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(&mut C, P::Output) -> Option<U>,
{
    MapOpt { parser, f }
}
