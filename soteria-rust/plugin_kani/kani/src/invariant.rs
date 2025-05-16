// Copyright Kani Contributors
// SPDX-License-Identifier: Apache-2.0 OR MIT

pub trait Invariant
where
    Self: Sized,
{
    fn is_safe(&self) -> bool;
}

macro_rules! trivial_invariant {
    ( $type: ty ) => {
        impl Invariant for $type {
            #[inline(always)]
            fn is_safe(&self) -> bool {
                true
            }
        }
    };
}

trivial_invariant!(u8);
trivial_invariant!(u16);
trivial_invariant!(u32);
trivial_invariant!(u64);
trivial_invariant!(u128);
trivial_invariant!(usize);

trivial_invariant!(i8);
trivial_invariant!(i16);
trivial_invariant!(i32);
trivial_invariant!(i64);
trivial_invariant!(i128);
trivial_invariant!(isize);

trivial_invariant!(f32);
trivial_invariant!(f64);
trivial_invariant!(f16);
trivial_invariant!(f128);

trivial_invariant!(());
trivial_invariant!(bool);
trivial_invariant!(char);
