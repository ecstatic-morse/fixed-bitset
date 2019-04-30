//! A fixed-size, stack-allocated bitset.
//!
//! ```
//! use fixed_bitset::Bitset;
//! use typenum::consts::*;
//!
//! let mut set = Bitset::<U100>::new();
//!
//! set.insert(20);
//! set.insert(70);
//! // set.insert(100); // WILL PANIC!
//!
//! let values: Vec<usize> = set.iter().collect();
//! assert_eq!(values, vec![20, 70]);
//!
//! let mut superset = set.clone();
//! superset.insert(50);
//!
//! assert!(superset.is_superset(&set));
//! assert!(set.is_subset(&superset));
//!
//!
//! let difference = &superset - &set;
//! assert_eq!(difference.iter().collect::<Vec<_>>(), vec![50]);
//! assert!(difference.is_disjoint(&set));
//! ```

#![deny(missing_docs)]

mod primitive;

use std::ops::*;
use std::{fmt, iter};

use generic_array::{GenericArray, ArrayLength};
use typenum::Unsigned;

use self::primitive::{Primitive, CeilDiv};

type CeilQuot<T, Q> = <T as CeilDiv<Q>>::Output;

/// Yields the index of each set bit in this block.
fn bits<B>(mut block: B) -> impl Iterator<Item = usize> + Clone
    where B: Primitive
{
    iter::from_fn(move || {
        if block.is_zero() {
            None
        } else {
            let next_bit = block.trailing_zeros() as usize;
            block ^= B::one() << next_bit;
            Some(next_bit)
        }
    })
}

/// A set of unsigned integers whose size is fixed at compile-time.
///
/// A `Bitset` can only store unsigned integers less than `N`, where `N` is a compile-time integer
/// from `typenum`. A `Bitset` uses a single bit to indicate the presence or absence of each value.
pub struct Bitset<N, B = usize>
    where B: Primitive,
          N: CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{
    blocks: GenericArray<B, CeilQuot<N, B::Size>>,
}

impl<N, B> fmt::Debug for Bitset<N, B>
    where B: Primitive,
          N: Unsigned + CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_set()
            .entries(self.iter())
            .finish()
    }
}

impl<N, B> Default for Bitset<N, B>
    where B: Primitive,
          N: CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{
    fn default() -> Self {
        Bitset {
            blocks: Default::default(),
        }
    }
}

impl<N, B> Clone for Bitset<N, B>
    where B: Primitive,
          N: CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{
    fn clone(&self) -> Self {
        Bitset {
            blocks: self.blocks.clone(),
        }
    }
}

impl<N, B> Copy for Bitset<N, B>
    where B: Primitive,
          N: CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
          GenericArray<B, CeilQuot<N, B::Size>>: Copy,
{}

impl<N, B> PartialEq for Bitset<N, B>
    where B: Primitive,
          N: CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{
    fn eq(&self, other: &Self) -> bool {
        self.blocks == other.blocks
    }
}

impl<N, B> std::cmp::Eq for Bitset<N, B>
    where B: Primitive,
          N: CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{}

impl<N, B> iter::FromIterator<usize> for Bitset<N, B>
    where B: Primitive,
          N: Unsigned + CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{
    fn from_iter<T>(iter: T ) -> Self
        where T: IntoIterator<Item = usize>
    {
        let mut ret = Self::default();
        for n in iter.into_iter() {
            ret.insert(n);
        }

        ret
    }
}

impl<N, B> Bitset<N, B>
    where B: Primitive,
          N: Unsigned + CeilDiv<B::Size>,
          CeilQuot<N, B::Size>: ArrayLength<B>,
{
    /// Returns an empty bitset.
    pub fn new() -> Self {
        Default::default()
    }

    /// Returns the block index and shift required to access a given bit.
    fn loc(bit: usize) -> (usize, usize) {
        (bit / B::SIZE, bit % B::SIZE)
    }

    /// Returns `true` if the bitset contains a value.
    ///
    /// Panics if `value >= N`.
    pub fn contains(&self, value: usize) -> bool {
        assert!(value < N::USIZE);
        let (block, shift) = Self::loc(value);

        (self.blocks[block] >> shift) & B::one() == B::one()
    }

    /// Inserts a value into the bitset.
    ///
    /// If that value already exists in the bitset, this function has no effect.
    ///
    /// Panics if `value >= N`.
    pub fn insert(&mut self, value: usize) {
        assert!(value < N::USIZE);
        let (block, shift) = Self::loc(value);

        self.blocks[block] |= B::one() << shift;
    }

    /// Removes a value from the bitset.
    ///
    /// If that value does not already exist in the bitset, this function has no effect.
    ///
    /// Panics if `value >= N`.
    pub fn remove(&mut self, value: usize) {
        assert!(value < N::USIZE);
        let (block, shift) = Self::loc(value);

        self.blocks[block] &= !(B::one() << shift);
    }

    /// Returns `true` if the bitset contains no bits.
    pub fn is_empty(&self) -> bool {
        self.blocks
            .iter()
            .all(|b| b.is_zero())
    }

    /// Returns the number of values contained in the bitset.
    pub fn len(&self) -> usize {
        self.blocks
            .iter()
            .map(|b| b.count_ones() as usize)
            .sum()
    }

    /// Returns an iterator over the values in the bitset.
    pub fn iter(&self) -> impl '_ + Iterator<Item = usize> + Clone {
        self.blocks
            .iter()
            .cloned()
            .enumerate()
            .flat_map(|(i, b)| bits(b).map(move |j| B::SIZE * i + j))
    }

    /// Clears the bitset, removing all values.
    pub fn clear(&mut self) {
        for b in &mut self.blocks {
            *b = B::zero();
        }
    }

    fn apply_blocks(&mut self, other: &Self, f: impl Fn(&mut B, B)) {
        for (a, &b) in self.blocks.iter_mut().zip(other.blocks.iter()) {
            f(a, b);
        }
    }

    fn iter_blocks<'a>(&'a self, other: &'a Self, f: impl 'a + Fn(B, B) -> B)
        -> impl 'a + Iterator<Item = usize>
    {
        self.blocks
            .iter()
            .zip(other.blocks.iter())
            .map(move |(&a, &b)| f(a, b))
            .enumerate()
            .flat_map(|(i, b)| bits(b).map(move |j| B::SIZE * i + j))
    }

    /// Returns an iterator over `self | other`.
    pub fn union<'a>(&'a self, other: &'a Self) -> impl 'a + Iterator<Item = usize> {
        self.iter_blocks(other, |a, b| a | b)
    }

    /// Returns an iterator over `self & other`.
    pub fn intersection<'a>(&'a self, other: &'a Self) -> impl 'a + Iterator<Item = usize> {
        self.iter_blocks(other, |a, b| a & b)
    }

    /// Returns an iterator over `self ^ other`.
    pub fn symmetric_difference<'a>(&'a self, other: &'a Self) -> impl 'a + Iterator<Item = usize> {
        self.iter_blocks(other, |a, b| a ^ b)
    }

    /// Returns an iterator over `self - other`.
    pub fn difference<'a>(&'a self, other: &'a Self) -> impl 'a + Iterator<Item = usize> {
        self.iter_blocks(other, |a, b| a & !b)
    }

    /// Returns `true` if `self` has no elements in common with `other`.
    ///
    /// This is more efficient than `self.intersection(other).next().is_none()`.
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.blocks
            .iter()
            .zip(other.blocks.iter())
            .all(|(&a, &b)| (a & b).is_zero())
    }

    /// Returns `true` if every element in `self` exists in `other`.
    pub fn is_subset(&self, other: &Self) -> bool {
        self.blocks
            .iter()
            .zip(other.blocks.iter())
            .all(|(&a, &b)| a & b == a)
    }

    /// Returns `true` if every element in `other` exists in `self`.
    pub fn is_superset(&self, other: &Self) -> bool {
        self.blocks
            .iter()
            .zip(other.blocks.iter())
            .all(|(&a, &b)| a & b == b)
    }
}

macro_rules! ops {
    ($( $( #[$meta:meta] )* $OpAssign:ident, $op_assign:ident, $Op:ident, $op:ident => $f:expr ),* $(,)?) => {
        $(
            $(#[$meta])*
            impl<N, B> $OpAssign<&Self> for Bitset<N, B>
                where B: Primitive,
                      N: Unsigned + CeilDiv<B::Size>,
                      CeilQuot<N, B::Size>: ArrayLength<B>,
            {
                fn $op_assign(&mut self, other: &Self) {
                    self.apply_blocks(other, $f);
                }
            }

            $(#[$meta])*
            impl<'a, 'b, N, B> $Op<&'b Bitset<N, B>> for &'a Bitset<N, B>
                where B: Primitive,
                      N: Unsigned + CeilDiv<B::Size>,
                      CeilQuot<N, B::Size>: ArrayLength<B>,
                      Bitset<N, B>: Clone,
            {
                type Output = Bitset<N, B>;

                fn $op(self, other: &'b Bitset<N, B>) -> Self::Output {
                    let mut ret = (*self).clone();
                    (&mut ret).$op_assign(other);
                    ret
                }
            }
        )*
    }
}

ops! {
    /// Union
    BitOrAssign,  bitor_assign,  BitOr,  bitor  => |a, b| *a |= b,
    /// Intersection
    BitAndAssign, bitand_assign, BitAnd, bitand => |a, b| *a &= b,
    /// Symmetric Difference
    BitXorAssign, bitxor_assign, BitXor, bitxor => |a, b| *a ^= b,
    /// Difference
    SubAssign,    sub_assign,    Sub,    sub    => |a, b| *a &= !b,
}
