pub trait Tuple0 {
    type AppendOutput<T>;

    fn append<T>(self, value: T) -> Self::AppendOutput<T>;
}

pub trait Tuple1: Tuple0 {
    type First;
    type Rest: Tuple0;

    fn split_first(self) -> (Self::First, Self::Rest);
}

pub trait Tuple2: Tuple1
where
    Self::Rest: Tuple1,
{
}

impl<T> Tuple2 for T
where
    T: Tuple1,
    T::Rest: Tuple1,
{
}

impl Tuple0 for () {
    type AppendOutput<T> = (T,);

    fn append<T>(self, value: T) -> Self::AppendOutput<T> {
        (value,)
    }
}

macro_rules! impl_non_empty_tuple {
    ($first_index:tt $first_name:ident $($index:tt $name:ident)*) => {
        impl<$first_name, $($name,)*> Tuple0 for ($first_name, $($name,)*) {
            type AppendOutput<T> = ($first_name, $($name,)* T);

            fn append<T>(self, value: T) -> Self::AppendOutput<T> {
                (self.$first_index, $(self.$index,)* value)
            }
        }

        impl<$first_name, $($name,)*> Tuple1 for ($first_name, $($name,)*) {
            type First = $first_name;
            type Rest = ($($name,)*);

            fn split_first(self) -> (Self::First, Self::Rest) {
                (self.$first_index, ($(self.$index,)*))
            }
        }
    };
}

macro_rules! impl_non_empty_tuples {
    (@helper [$($arg:tt)*] $index:tt $name:ident $($rest:tt)*) => {
        impl_non_empty_tuple! { $($arg)* }
        impl_non_empty_tuples! { @helper [$($arg)* $index $name] $($rest)* }
    };
    (@helper [$($arg:tt)*]) => {
        impl_non_empty_tuple! { $($arg)* }
    };
    (($first_index:tt, $first_name:ident), $(($index:tt, $name:ident),)*) => {
        impl_non_empty_tuples! { @helper [$first_index $first_name] $($index $name)* }
    }
}

impl_non_empty_tuples! {
    (0, T0),
    (1, T1),
    (2, T2),
    (3, T3),
    (4, T4),
    (5, T5),
    (6, T6),
    (7, T7),
    (8, T8),
    (9, T9),
    (10, T10),
    (11, T11),
    (12, T12),
    (13, T13),
    (14, T14),
    (15, T15),
    (16, T16),
    (17, T17),
    (18, T18),
    (19, T19),
    (20, T20),
    (21, T21),
    (22, T22),
    (23, T23),
    (24, T24),
    (25, T25),
    (26, T26),
    (27, T27),
    (28, T28),
    (29, T29),
    (30, T30),
    (31, T31),
}
