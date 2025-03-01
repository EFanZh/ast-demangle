use crate::generic_tuple::{FoldFnMut, SplitFirst, Tuple as TupleTrait};
use crate::Parser;

#[expect(unnameable_types, reason = "internal usage")]
pub struct Single<P>
where
    P: ?Sized,
{
    parser: P,
}

impl<C, P> Parser<C> for Single<P>
where
    C: ?Sized,
    P: Parser<C> + ?Sized,
{
    type Output = (P::Output,);

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        self.parser.parse(context).map(|output| (output,))
    }
}

#[expect(unnameable_types, reason = "internal usage")]
pub struct FlattenAnd<P, Q>
where
    Q: ?Sized,
{
    lhs: P,
    rhs: Q,
}

impl<C, P, Q> Parser<C> for FlattenAnd<P, Q>
where
    C: ?Sized,
    P: Parser<C>,
    P::Output: TupleTrait,
    Q: Parser<C> + ?Sized,
{
    type Output = <P::Output as TupleTrait>::Append<Q::Output>;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        let items = self.lhs.parse(context)?;
        let item = self.rhs.parse(context)?;

        Ok(items.append(item))
    }
}

#[expect(unnameable_types, reason = "internal usage")]
pub struct FoldFlattenAnd;

impl FoldFnMut for FoldFlattenAnd {
    type Output<B, T> = FlattenAnd<B, T>;

    fn call_mut<B, T>(&mut self, init: B, item: T) -> Self::Output<B, T> {
        FlattenAnd { lhs: init, rhs: item }
    }
}

type TupleImpl<T> = <<T as SplitFirst>::Rest as TupleTrait>::Fold<Single<<T as SplitFirst>::First>, FoldFlattenAnd>;

pub struct Tuple<T>
where
    T: SplitFirst + ?Sized,
{
    inner: TupleImpl<T>,
}

impl<C, T> Parser<C> for Tuple<T>
where
    C: ?Sized,
    T: SplitFirst + ?Sized,
    TupleImpl<T>: Parser<C>,
{
    type Output = <TupleImpl<T> as Parser<C>>::Output;

    fn parse(&mut self, context: &mut C) -> Result<Self::Output, ()> {
        self.inner.parse(context)
    }
}

pub fn tuple<C, T>(parsers: T) -> Tuple<T>
where
    C: ?Sized,
    T: TupleTrait + SplitFirst,
    TupleImpl<T>: Parser<C>,
{
    let (first, rest) = parsers.split_first();

    Tuple {
        inner: rest.fold(Single { parser: first }, &mut FoldFlattenAnd),
    }
}
