#![expect(missing_docs, clippy::missing_errors_doc, reason = "internal create")]

use self::combinators::{FlatMap, Inspect, Many0, Map, MapOpt, Opt};

pub mod combinators;
pub mod generic_tuple;

pub trait Cursor {
    type Cursor;

    fn get_cursor(&mut self) -> Self::Cursor;
    fn set_cursor(&mut self, cursor: Self::Cursor);
}

pub trait CursorExt: Cursor {
    #[expect(clippy::result_unit_err, reason = "internal usage")]
    fn transaction<F, T>(&mut self, f: F) -> Result<T, ()>
    where
        F: FnOnce(&mut Self) -> Result<T, ()>,
    {
        let saved_cursor = self.get_cursor();
        let result = f(self);

        if result.is_err() {
            self.set_cursor(saved_cursor);
        }

        result
    }
}

impl<T> CursorExt for T where T: Cursor + ?Sized {}

pub trait Parser<C>
where
    C: ?Sized,
{
    type Output;

    #[expect(clippy::result_unit_err, reason = "internal usage")]
    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()>;
}

impl<C, F, T> Parser<C> for F
where
    C: ?Sized,
    F: FnMut(&mut C) -> Result<T, ()>,
{
    type Output = T;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        self(context)
    }
}

pub trait ParserExt<C>: Parser<C>
where
    C: ?Sized,
{
    fn flat_map<F, P>(self, f: F) -> FlatMap<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output) -> P,
        P: Parser<C>,
    {
        combinators::flat_map(self, f)
    }

    fn map<F, T>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output) -> T,
    {
        combinators::map(self, f)
    }

    fn map_opt<F, T>(self, f: F) -> MapOpt<Self, F>
    where
        Self: Sized,
        F: FnMut(&mut C, Self::Output) -> Option<T>,
    {
        combinators::map_opt(self, f)
    }

    fn inspect<F>(self, f: F) -> Inspect<Self, F>
    where
        Self: Sized,
        F: FnMut(&mut C, &Self::Output),
    {
        combinators::inspect(self, f)
    }

    fn many0(self) -> Many0<Self>
    where
        Self: Sized,
        C: Cursor,
    {
        combinators::many0(self)
    }

    fn opt(self) -> Opt<Self>
    where
        Self: Sized,
        C: Cursor,
    {
        combinators::opt(self)
    }
}

impl<C, P> ParserExt<C> for P
where
    C: ?Sized,
    P: Parser<C>,
{
}
