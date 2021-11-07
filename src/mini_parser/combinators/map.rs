use crate::mini_parser::Parser;

pub struct Map<P, F> {
    parser: P,
    f: F,
}

impl<I, C, P, F, U> Parser<I, C> for Map<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output) -> U,
{
    type Output = U;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (output, input) = self.parser.parse(input, context)?;

        Ok(((self.f)(output), input))
    }
}

pub fn map<I, C, P, F, U>(parser: P, f: F) -> Map<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output) -> U,
{
    Map { parser, f }
}
