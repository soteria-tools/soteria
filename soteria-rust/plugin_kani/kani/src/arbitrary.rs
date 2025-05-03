// Copyright Kani Contributors
// SPDX-License-Identifier: Apache-2.0 OR MIT

pub trait Arbitrary
where
    Self: Sized,
{
    fn any() -> Self;
    fn any_array<const MAX_ARRAY_LENGTH: usize>() -> [Self; MAX_ARRAY_LENGTH] {
        crate::kani::nondet()
    }
}

macro_rules! trivial_arbitrary {
    ( $type: ty ) => {
        impl Arbitrary for $type {
            #[inline(always)]
            fn any() -> Self {
                crate::kani::nondet()
            }
        }
    };
}

macro_rules! nonzero_arbitrary {
    ( $type: ty, $base: ty ) => {
        impl Arbitrary for $type {
            #[inline(always)]
            fn any() -> Self {
                let val = <$base>::any();
                crate::kani::assume(val != 0);
                unsafe { <$type>::new_unchecked(val) }
            }
        }
    };
}

// Generate trivial arbitrary values -- these are handled in the engine directly
trivial_arbitrary!(());

trivial_arbitrary!(u8);
trivial_arbitrary!(u16);
trivial_arbitrary!(u32);
trivial_arbitrary!(u64);
trivial_arbitrary!(u128);
trivial_arbitrary!(usize);

trivial_arbitrary!(i8);
trivial_arbitrary!(i16);
trivial_arbitrary!(i32);
trivial_arbitrary!(i64);
trivial_arbitrary!(i128);
trivial_arbitrary!(isize);

trivial_arbitrary!(f16);
trivial_arbitrary!(f32);
trivial_arbitrary!(f64);
trivial_arbitrary!(f128);

trivial_arbitrary!(bool);
trivial_arbitrary!(char);

use std::num::*;

nonzero_arbitrary!(NonZeroU8, u8);
nonzero_arbitrary!(NonZeroU16, u16);
nonzero_arbitrary!(NonZeroU32, u32);
nonzero_arbitrary!(NonZeroU64, u64);
nonzero_arbitrary!(NonZeroU128, u128);
nonzero_arbitrary!(NonZeroUsize, usize);

nonzero_arbitrary!(NonZeroI8, i8);
nonzero_arbitrary!(NonZeroI16, i16);
nonzero_arbitrary!(NonZeroI32, i32);
nonzero_arbitrary!(NonZeroI64, i64);
nonzero_arbitrary!(NonZeroI128, i128);
nonzero_arbitrary!(NonZeroIsize, isize);

impl<T, const N: usize> Arbitrary for [T; N]
where
    T: Arbitrary,
{
    fn any() -> Self {
        crate::kani::nondet()
    }
}

impl<T> Arbitrary for Option<T>
where
    T: Arbitrary,
{
    fn any() -> Self {
        if bool::any() {
            Some(T::any())
        } else {
            None
        }
    }
}

impl<T, E> Arbitrary for Result<T, E>
where
    T: Arbitrary,
    E: Arbitrary,
{
    fn any() -> Self {
        if bool::any() {
            Ok(T::any())
        } else {
            Err(E::any())
        }
    }
}

use std::mem::MaybeUninit;

impl<T> Arbitrary for MaybeUninit<T>
where
    T: Arbitrary,
{
    fn any() -> Self {
        if crate::kani::any() {
            MaybeUninit::new(T::any())
        } else {
            MaybeUninit::uninit()
        }
    }
}

/// This macro implements `kani::Arbitrary` on a tuple whose elements
/// already implement `kani::Arbitrary` by running `kani::any()` on
/// each index of the tuple.
#[allow(clippy::crate_in_macro_def)]
#[macro_export]
macro_rules! arbitrary_tuple {
    ($($type:ident),*) => {
        impl<$($type : Arbitrary),*>  Arbitrary for ($($type,)*) {
            #[inline(always)]
            fn any() -> Self {
                ($(crate::kani::any::<$type>(),)*)
            }
        }
    }
}

arbitrary_tuple!(A);
arbitrary_tuple!(A, B);
arbitrary_tuple!(A, B, C);
arbitrary_tuple!(A, B, C, D);
arbitrary_tuple!(A, B, C, D, E);
arbitrary_tuple!(A, B, C, D, E, F);
arbitrary_tuple!(A, B, C, D, E, F, G);
arbitrary_tuple!(A, B, C, D, E, F, G, H);
arbitrary_tuple!(A, B, C, D, E, F, G, H, I);
arbitrary_tuple!(A, B, C, D, E, F, G, H, I, J);
arbitrary_tuple!(A, B, C, D, E, F, G, H, I, J, K);
arbitrary_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);

mod range_structures {
    use crate::kani::Arbitrary;
    use std::ops::{Bound, Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive};

    impl<T> Arbitrary for Bound<T>
    where
        T: Arbitrary,
    {
        fn any() -> Self {
            match u8::any() {
                0 => Bound::Included(T::any()),
                1 => Bound::Excluded(T::any()),
                _ => Bound::Unbounded,
            }
        }
    }

    impl<T> Arbitrary for Range<T>
    where
        T: Arbitrary,
    {
        fn any() -> Self {
            T::any()..T::any()
        }
    }

    impl<T> Arbitrary for RangeFrom<T>
    where
        T: Arbitrary,
    {
        fn any() -> Self {
            T::any()..
        }
    }

    impl<T> Arbitrary for RangeInclusive<T>
    where
        T: Arbitrary,
    {
        fn any() -> Self {
            T::any()..=T::any()
        }
    }

    impl<T> Arbitrary for RangeTo<T>
    where
        T: Arbitrary,
    {
        fn any() -> Self {
            ..T::any()
        }
    }

    impl<T> Arbitrary for RangeToInclusive<T>
    where
        T: Arbitrary,
    {
        fn any() -> Self {
            ..=T::any()
        }
    }
}
