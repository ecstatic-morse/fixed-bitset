use std::ops::*;

use num_traits::{PrimInt, WrappingShl, WrappingSub};
use typenum::consts::*;
use typenum::operator_aliases::*;
use typenum::{Unsigned, bit::B1};

pub trait CeilDiv<Q> {
    type Output;
}

impl<T, Q> CeilDiv<Q> for T
    where Q: Sub<B1>,
          T: Add<Sub1<Q>>,
          Sum<T, Sub1<Q>>: Div<Q>,
{
    type Output = Quot<Sum<T, Sub1<Q>>, Q>;
}

pub trait Primitive: PrimInt
                   + Default
                   + WrappingShl
                   + WrappingSub
                   + BitOrAssign<Self>
                   + BitAndAssign<Self>
                   + BitXorAssign<Self>
{
    type Size: Unsigned;

    const SIZE: usize;
}

macro_rules! primitives {
    ($( $(#[$meta:meta])* $T:ty => $Size:ident ),* $(,)?) => {
        $(
            $( #[$meta] )*
            impl Primitive for $T {
                type Size = $Size;

                const SIZE: usize = $Size::USIZE;
            }
        )*
    }
}

primitives! {
    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,
    u128 => U128,

    #[cfg(target_pointer_width = "16")]
    usize => U16,
    #[cfg(target_pointer_width = "32")]
    usize => U32,
    #[cfg(target_pointer_width = "64")]
    usize => U64,
}

