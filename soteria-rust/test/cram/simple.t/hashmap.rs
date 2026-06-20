// HashMap test, relying on the stubbed hasher (a constant hash, so the bucket
// index stays concrete even for a symbolic key -- see Optim.hash_one).
use std::collections::HashMap;

fn main() {
    // concrete keys: insert (with overwrite), get (hit and miss), len
    let mut m: HashMap<i32, i32> = HashMap::new();
    m.insert(1, 10);
    m.insert(2, 20);
    m.insert(1, 11); // overwrite
    assert_eq!(m.get(&1), Some(&11));
    assert_eq!(m.get(&2), Some(&20));
    assert_eq!(m.get(&3), None);
    assert_eq!(m.len(), 2);

    // a symbolic key, which collides with the concrete entry iff k == 7
    let k: i32 = soteria::nondet_bytes();
    let mut s: HashMap<i32, i32> = HashMap::new();
    s.insert(7, 70);
    s.insert(k, 100);
    assert_eq!(*s.get(&k).unwrap(), 100); // always the value just inserted
    let expected = if k == 7 { 100 } else { 70 };
    assert_eq!(*s.get(&7).unwrap(), expected);
}
