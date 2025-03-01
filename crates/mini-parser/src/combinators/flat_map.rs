use crate::Parser;

pub struct FlatMap<P, F>
where
    F: ?Sized,
{
    parser: P,
    f: F,
}

impl<C, P, F, Q> Parser<C> for FlatMap<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(P::Output) -> Q + ?Sized,
    Q: Parser<C>,
{
    type Output = <F::Output as Parser<C>>::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        let output = self.parser.parse(context)?;

        (self.f)(output).parse(context)
    }
}

pub const fn flat_map<C, P, F, Q>(parser: P, f: F) -> FlatMap<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(P::Output) -> Q,
    Q: Parser<C>,
{
    FlatMap { parser, f }
}
