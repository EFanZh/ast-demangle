use nom::error::{ErrorKind, ParseError};
use nom::{Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed, Offset, Slice};

use super::{Const, Path, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::RangeTo;
use std::rc::Rc;

#[derive(Default)]
pub struct BackRefTable<'a> {
    pub paths: HashMap<usize, Rc<Path<'a>>>,
    pub types: HashMap<usize, Rc<Type<'a>>>,
    pub consts: HashMap<usize, Rc<Const<'a>>>,
}

#[derive(Clone, Copy)]
pub struct Context<'a, 'b> {
    pub index: usize,
    pub data: &'a [u8],
    pub back_ref_table: &'b RefCell<BackRefTable<'a>>,
}

impl<'a, 'b> Context<'a, 'b> {
    pub fn new(data: &'a [u8], back_ref_table: &'b RefCell<BackRefTable<'a>>) -> Self {
        Self {
            index: 0,
            data,
            back_ref_table,
        }
    }
}

impl<'a, 'b> PartialEq for Context<'a, 'b> {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(self.data, other.data)
    }
}

impl<'a, 'b, T> Compare<T> for Context<'a, 'b>
where
    &'a [u8]: Compare<T>,
{
    fn compare(&self, t: T) -> nom::CompareResult {
        Compare::compare(&self.data, t)
    }

    fn compare_no_case(&self, t: T) -> nom::CompareResult {
        Compare::compare_no_case(&self.data, t)
    }
}

impl<'a, 'b> InputIter for Context<'a, 'b> {
    type Item = <&'a [u8] as InputIter>::Item;
    type Iter = <&'a [u8] as InputIter>::Iter;
    type IterElem = <&'a [u8] as InputIter>::IterElem;

    fn iter_indices(&self) -> Self::Iter {
        InputIter::iter_indices(&self.data)
    }

    fn iter_elements(&self) -> Self::IterElem {
        InputIter::iter_elements(&self.data)
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        InputIter::position(&self.data, predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        InputIter::slice_index(&self.data, count)
    }
}

impl<'a, 'b> InputLength for Context<'a, 'b> {
    fn input_len(&self) -> usize {
        InputLength::input_len(&self.data)
    }
}

impl<'a, 'b> InputTake for Context<'a, 'b> {
    fn take(&self, count: usize) -> Self {
        Self {
            data: InputTake::take(&self.data, count),
            ..*self
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (suffix, prefix) = InputTake::take_split(&self.data, count);

        (
            Self {
                index: self.index + prefix.len(),
                data: suffix,
                ..*self
            },
            Self { data: prefix, ..*self },
        )
    }
}

impl<'a, 'b> InputTakeAtPosition for Context<'a, 'b> {
    type Item = <&'a [u8] as InputTakeAtPosition>::Item;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(&self, predicate: P, e: ErrorKind) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(*self, e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(&self, predicate: P, e: ErrorKind) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position1(predicate, e) {
            Err(nom::Err::Incomplete(_)) => {
                if self.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(*self, e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
            res => res,
        }
    }
}

impl<'a, 'b> Offset for Context<'a, 'b> {
    fn offset(&self, second: &Self) -> usize {
        Offset::offset(self.data, second.data)
    }
}

impl<'a, 'b> Slice<RangeTo<usize>> for Context<'a, 'b> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            data: Slice::slice(&self.data, range),
            ..*self
        }
    }
}
