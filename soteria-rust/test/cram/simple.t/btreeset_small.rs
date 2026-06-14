use std::collections::BTreeSet;

const SIZE: usize = 2;

fn nondet_treeset() -> BTreeSet<u32> {
    (0..SIZE).map(|_| soteria::nondet_bytes()).collect()
}

#[soteria::test]
fn test_treeset_is_ordered() {
    let set = nondet_treeset();

    let values: Vec<u32> = set.iter().copied().collect();
    let mut sorted = values.clone();
    sorted.sort();

    assert_eq!(
        values, sorted,
        "BTreeSet values should be in ascending order"
    );
}
