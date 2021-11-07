use crate::mini_parser::Parser;

pub struct InspectWithContext<P, F> {
    parser: P,
    f: F,
}

impl<I, C, P, F> Parser<I, C> for InspectWithContext<P, F>
where
    P: Parser<I, C>,
    F: FnMut(&P::Output, &mut C),
{
    type Output = P::Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let (output, input) = self.parser.parse(input, context)?;

        (self.f)(&output, context);

        Ok((output, input))
    }
}

pub fn inspect_with_context<I, C, P, F>(parser: P, f: F) -> InspectWithContext<P, F>
where
    P: Parser<I, C>,
    F: FnMut(&P::Output, &mut C),
{
    InspectWithContext { parser, f }
}
