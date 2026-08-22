fn check_size<T, C>(predicate: C)
where
    C: Fn(usize) -> bool,
{
    let size = std::mem::size_of::<T>();
    assert!(predicate(size));
}

#[soteria::test]
fn two_generics<T, U>() {
    let size_t = std::mem::size_of::<T>();
    let size_u = std::mem::size_of::<U>();

    check_size::<T, _>(|s| s == size_t);
    check_size::<U, _>(|s| s == size_u);

    if size_t < size_u {
        check_size::<T, _>(|s| s < size_u);
        check_size::<U, _>(|s| s > size_t);
    } else if size_t > size_u {
        check_size::<T, _>(|s| s > size_u);
        check_size::<U, _>(|s| s < size_t);
    } else {
        check_size::<T, _>(|s| s == size_u);
        check_size::<U, _>(|s| s == size_t);
    }
}
