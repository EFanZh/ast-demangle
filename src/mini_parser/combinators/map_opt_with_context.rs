use crate::mini_parser::Parser;

pub struct MapOptWithContext<P, F> {
    parser: P,
    f: F,
}

impl<I, C, P, F, U> Parser<I, C> for MapOptWithContext<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output, &mut C) -> Option<U>,
{
    type Output = U;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (output, input) = self.parser.parse(input, context)?;

        Ok(((self.f)(output, context).ok_or(())?, input))
    }
}
pub fn map_opt_with_context<I, C, P, F, U>(parser: P, f: F) -> MapOptWithContext<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output, &mut C) -> Option<U>,
{
    MapOptWithContext { parser, f }
}
