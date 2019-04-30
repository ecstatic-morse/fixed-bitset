# fixed-bitset

A bitset whose size is fixed at compile-time.

```rust
use fixed_bitset::Bitset;
use typenum::consts::*;

let mut set = Bitset::<U100>::new();

set.insert(20);
set.insert(70);
// set.insert(100); // WILL PANIC!

let values: Vec<usize> = set.iter().collect();
assert_eq!(values, vec![20, 70]);

let mut superset = set.clone();
superset.insert(50);

assert!(superset.is_superset(&set));
assert!(set.is_subset(&superset));


let difference = &superset - &set;
assert_eq!(difference.iter().collect::<Vec<_>>(), vec![50]);
assert!(difference.is_disjoint(&set));
```
