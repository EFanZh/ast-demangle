use crate::Parser;

pub struct Map<P, F>
where
    F: ?Sized,
{
    parser: P,
    f: F,
}

impl<C, P, F, T> Parser<C> for Map<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(P::Output) -> T + ?Sized,
{
    type Output = T;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        self.parser.parse(context).map(|output| (self.f)(output))
    }
}

pub const fn map<C, P, F, T>(parser: P, f: F) -> Map<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(P::Output) -> T,
{
    Map { parser, f }
}
