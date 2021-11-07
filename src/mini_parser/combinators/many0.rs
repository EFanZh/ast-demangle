use crate::mini_parser::Parser;

pub struct Many0<P> {
    parser: P,
}

impl<I, C, P> Parser<I, C> for Many0<P>
where
    I: Clone,
    P: Parser<I, C>,
{
    type Output = Vec<P::Output>;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        let mut output = Vec::new();
        let mut input = input;

        while let Ok((item, next_input)) = self.parser.parse(input.clone(), context) {
            output.push(item);
            input = next_input;
        }

        Ok((output, input))
    }
}

pub fn many0<I, C, P>(parser: P) -> Many0<P>
where
    I: Clone,
    P: Parser<I, C>,
{
    Many0 { parser }
}
