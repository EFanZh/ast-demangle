use crate::combinators::or::{self, Or};
use crate::generic_tuple::{FoldFnMut, SplitLast, Tuple};
use crate::Parser;

#[expect(unnameable_types, reason = "internal usage")]
pub struct FoldOr;

impl FoldFnMut for FoldOr {
    type Output<B, T> = Or<T, B>;

    fn call_mut<B, T>(&mut self, init: B, item: T) -> Self::Output<B, T> {
        or::or_unchecked(item, init)
    }
}

type AltImpl<T> = <<T as SplitLast>::Rest as Tuple>::RFold<<T as SplitLast>::Last, FoldOr>;

pub struct Alt<T>
where
    T: SplitLast + ?Sized,
{
    inner: AltImpl<T>,
}

impl<C, T> Parser<C> for Alt<T>
where
    C: ?Sized,
    T: SplitLast + ?Sized,
    AltImpl<T>: Parser<C>,
{
    type Output = <AltImpl<T> as Parser<C>>::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        self.inner.parse(context)
    }
}

pub fn alt<C, T>(parsers: T) -> Alt<T>
where
    C: ?Sized,
    T: SplitLast,
    AltImpl<T>: Parser<C>,
{
    let (last, rest) = parsers.split_last();

    Alt {
        inner: rest.rfold(last, &mut FoldOr),
    }
}
