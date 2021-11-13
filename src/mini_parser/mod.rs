use self::combinators::{And, FlatMap, Map, MapOpt, Opt, Or};
use crate::mini_parser::combinators::{InspectWithContext, Many0, MapOptWithContext};

pub mod combinators;
pub mod generic_tuple;
pub mod input;
pub mod parsers;

pub trait Parser<I, C> {
    type Output;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()>;

    fn and<P>(self, rhs: P) -> And<Self, P>
    where
        Self: Sized,
        P: Parser<I, C>,
    {
        combinators::and(self, rhs)
    }

    fn flat_map<F, Q>(self, f: F) -> FlatMap<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output) -> Q,
        Q: Parser<I, C>,
    {
        combinators::flat_map(self, f)
    }

    fn map<F, U>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output) -> U,
    {
        combinators::map(self, f)
    }

    fn map_opt<F, U>(self, f: F) -> MapOpt<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output) -> Option<U>,
    {
        combinators::map_opt(self, f)
    }

    fn map_opt_with_context<F, U>(self, f: F) -> MapOptWithContext<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output, &mut C) -> Option<U>,
    {
        combinators::map_opt_with_context(self, f)
    }

    fn inspect_with_context<F>(self, f: F) -> InspectWithContext<Self, F>
    where
        Self: Sized,
        F: FnMut(&Self::Output, &mut C),
    {
        combinators::inspect_with_context(self, f)
    }

    fn many0(self) -> Many0<Self>
    where
        Self: Sized,
        I: Clone,
    {
        combinators::many0(self)
    }

    fn opt(self) -> Opt<Self>
    where
        Self: Sized,
        I: Clone,
    {
        combinators::opt(self)
    }

    fn or<P>(self, rhs: P) -> Or<Self, P>
    where
        Self: Sized,
        I: Clone,
        P: Parser<I, C, Output = Self::Output>,
    {
        combinators::or(self, rhs)
    }
}

impl<I, C, F, O> Parser<I, C> for F
where
    F: FnMut(I, &mut C) -> Result<(O, I), ()>,
{
    type Output = O;

    fn parse(&mut self, input: I, context: &mut C) -> Result<(Self::Output, I), ()> {
        self(input, context)
    }
}
