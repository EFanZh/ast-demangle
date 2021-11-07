use crate::mini_parser::Parser;

pub struct MapOpt<P, F> {
    parser: P,
    f: F,
}

impl<I, C, P, F, U> Parser<I, C> for MapOpt<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output) -> Option<U>,
{
    type Output = U;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (output, input) = self.parser.parse(input, context)?;

        Ok(((self.f)(output).ok_or(())?, input))
    }
}
pub fn map_opt<I, C, P, F, U>(parser: P, f: F) -> MapOpt<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output) -> Option<U>,
{
    MapOpt { parser, f }
}
