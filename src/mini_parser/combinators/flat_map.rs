use crate::mini_parser::Parser;

pub struct FlatMap<P, F> {
    parser: P,
    f: F,
}

impl<I, C, P, F, Q> Parser<I, C> for FlatMap<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output) -> Q,
    Q: Parser<I, C>,
{
    type Output = Q::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (output, rest) = self.parser.parse(input, context)?;

        (self.f)(output).parse(rest, context)
    }
}

pub fn flat_map<I, C, P, F, Q>(parser: P, f: F) -> FlatMap<P, F>
where
    P: Parser<I, C>,
    F: FnMut(P::Output) -> Q,
    Q: Parser<I, C>,
{
    FlatMap { parser, f }
}
