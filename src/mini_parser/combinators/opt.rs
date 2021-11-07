use crate::mini_parser::Parser;

pub struct Opt<P> {
    parser: P,
}

impl<I, C, P> Parser<I, C> for Opt<P>
where
    I: Clone,
    P: Parser<I, C>,
{
    type Output = Option<P::Output>;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        Ok(match self.parser.parse(input.clone(), context) {
            Ok((output, input)) => (Some(output), input),
            Err(()) => (None, input),
        })
    }
}

pub fn opt<I, C, P>(parser: P) -> Opt<P>
where
    I: Clone,
    P: Parser<I, C>,
{
    Opt { parser }
}
