use crate::Parser;

pub struct Inspect<P, F>
where
    F: ?Sized,
{
    parser: P,
    f: F,
}

impl<C, P, F> Parser<C> for Inspect<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(&mut C, &P::Output) + ?Sized,
{
    type Output = P::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        let output = self.parser.parse(context)?;

        (self.f)(context, &output);

        Ok(output)
    }
}

pub const fn inspect<C, P, F>(parser: P, f: F) -> Inspect<P, F>
where
    C: ?Sized,
    P: Parser<C>,
    F: FnMut(&mut C, &P::Output),
{
    Inspect { parser, f }
}
