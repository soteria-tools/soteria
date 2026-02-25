#![feature(linked_list_cursors)]
#![feature(linked_list_remove)]
#![feature(allocator_api)]

// Kani: RUSTFLAGS="--edition 2024" kani ./demo/darpa/dll_test.rs --unwind 30 --no-unwinding-checks  --harness "test"
// Soteria Rust: dune exec -- soteria-rust exec demo/darpa/dll_test.rs --kani --rustc="--edition=2024" --summary --compact

// Some tests taken from Rust's test suite, to test doubly linked list

use std::alloc::Global;
use std::collections::LinkedList;
use std::ptr::NonNull;
use std::vec::Vec;

#[kani::proof]
fn test_basic() {
    let mut m = LinkedList::<Box<_>>::new();
    assert_eq!(m.pop_front(), None);
    assert_eq!(m.pop_back(), None);
    assert_eq!(m.pop_front(), None);
    m.push_front(Box::new(1));
    assert_eq!(m.pop_front(), Some(Box::new(1)));
    m.push_back(Box::new(2));
    m.push_back(Box::new(3));
    assert_eq!(m.len(), 2);
    assert_eq!(m.pop_front(), Some(Box::new(2)));
    assert_eq!(m.pop_front(), Some(Box::new(3)));
    assert_eq!(m.len(), 0);
    assert_eq!(m.pop_front(), None);
    m.push_back(Box::new(1));
    m.push_back(Box::new(3));
    m.push_back(Box::new(5));
    m.push_back(Box::new(7));
    assert_eq!(m.pop_front(), Some(Box::new(1)));

    let mut n = LinkedList::new();
    n.push_front(2);
    n.push_front(3);
    {
        assert_eq!(n.front().unwrap(), &3);
        let x = n.front_mut().unwrap();
        assert_eq!(*x, 3);
        *x = 0;
    }
    {
        assert_eq!(n.back().unwrap(), &2);
        let y = n.back_mut().unwrap();
        assert_eq!(*y, 2);
        *y = 1;
    }
    assert_eq!(n.pop_front(), Some(0));
    assert_eq!(n.pop_front(), Some(1));
}

fn generate_test() -> LinkedList<i32> {
    list_from(&[0, 1, 2, 3, 4, 5, 6])
}

fn list_from<T: Clone>(v: &[T]) -> LinkedList<T> {
    v.iter().cloned().collect()
}

#[kani::proof]
fn test_append() {
    // Empty to empty
    {
        let mut m = LinkedList::<i32>::new();
        let mut n = LinkedList::new();
        m.append(&mut n);
        // check_links(&m);
        assert_eq!(m.len(), 0);
        assert_eq!(n.len(), 0);
    }
    // Non-empty to empty
    {
        let mut m = LinkedList::new();
        let mut n = LinkedList::new();
        n.push_back(2);
        m.append(&mut n);
        // check_links(&m);
        assert_eq!(m.len(), 1);
        assert_eq!(m.pop_back(), Some(2));
        assert_eq!(n.len(), 0);
        // check_links(&m);
    }
    // Empty to non-empty
    {
        let mut m = LinkedList::new();
        let mut n = LinkedList::new();
        m.push_back(2);
        m.append(&mut n);
        // check_links(&m);
        assert_eq!(m.len(), 1);
        assert_eq!(m.pop_back(), Some(2));
        // check_links(&m);
    }

    // Non-empty to non-empty
    let v = vec![1, 2, 3, 4, 5];
    let u = vec![9, 8, 1, 2, 3, 4, 5];
    let mut m = list_from(&v);
    let mut n = list_from(&u);
    m.append(&mut n);
    // check_links(&m);
    let mut sum = v;
    sum.extend_from_slice(&u);
    assert_eq!(sum.len(), m.len());
    for elt in sum {
        assert_eq!(m.pop_front(), Some(elt))
    }
    assert_eq!(n.len(), 0);
    // Let's make sure it's working properly, since we
    // did some direct changes to private members.
    n.push_back(3);
    assert_eq!(n.len(), 1);
    assert_eq!(n.pop_front(), Some(3));
    // check_links(&n);
}

#[kani::proof]
fn test_iterator() {
    let m = generate_test();
    for (i, elt) in m.iter().enumerate() {
        assert_eq!(i as i32, *elt);
    }
    let mut n = LinkedList::new();
    assert_eq!(n.iter().next(), None);
    n.push_front(4);
    let mut it = n.iter();
    assert_eq!(it.size_hint(), (1, Some(1)));
    assert_eq!(it.next().unwrap(), &4);
    assert_eq!(it.size_hint(), (0, Some(0)));
    assert_eq!(it.next(), None);
}

#[kani::proof]
fn test_iterator_clone() {
    let mut n = LinkedList::new();
    n.push_back(2);
    n.push_back(3);
    n.push_back(4);
    let mut it = n.iter();
    it.next();
    let mut jt = it.clone();
    assert_eq!(it.next(), jt.next());
    assert_eq!(it.next_back(), jt.next_back());
    assert_eq!(it.next(), jt.next());
}

#[kani::proof]
fn test_iterator_double_end() {
    let mut n = LinkedList::new();
    assert_eq!(n.iter().next(), None);
    n.push_front(4);
    n.push_front(5);
    n.push_front(6);
    let mut it = n.iter();
    assert_eq!(it.size_hint(), (3, Some(3)));
    assert_eq!(it.next().unwrap(), &6);
    assert_eq!(it.size_hint(), (2, Some(2)));
    assert_eq!(it.next_back().unwrap(), &4);
    assert_eq!(it.size_hint(), (1, Some(1)));
    assert_eq!(it.next_back().unwrap(), &5);
    assert_eq!(it.next_back(), None);
    assert_eq!(it.next(), None);
}

#[kani::proof]
fn test_rev_iter() {
    let m = generate_test();
    for (i, elt) in m.iter().rev().enumerate() {
        assert_eq!((6 - i) as i32, *elt);
    }
    let mut n = LinkedList::new();
    assert_eq!(n.iter().rev().next(), None);
    n.push_front(4);
    let mut it = n.iter().rev();
    assert_eq!(it.size_hint(), (1, Some(1)));
    assert_eq!(it.next().unwrap(), &4);
    assert_eq!(it.size_hint(), (0, Some(0)));
    assert_eq!(it.next(), None);
}

#[kani::proof]
fn test_mut_iter() {
    let mut m = generate_test();
    let mut len = m.len();
    for (i, elt) in m.iter_mut().enumerate() {
        assert_eq!(i as i32, *elt);
        len -= 1;
    }
    assert_eq!(len, 0);
    let mut n = LinkedList::new();
    assert!(n.iter_mut().next().is_none());
    n.push_front(4);
    n.push_back(5);
    let mut it = n.iter_mut();
    assert_eq!(it.size_hint(), (2, Some(2)));
    assert!(it.next().is_some());
    assert!(it.next().is_some());
    assert_eq!(it.size_hint(), (0, Some(0)));
    assert!(it.next().is_none());
}

#[kani::proof]
fn test_iterator_mut_double_end() {
    let mut n = LinkedList::new();
    assert!(n.iter_mut().next_back().is_none());
    n.push_front(4);
    n.push_front(5);
    n.push_front(6);
    let mut it = n.iter_mut();
    assert_eq!(it.size_hint(), (3, Some(3)));
    assert_eq!(*it.next().unwrap(), 6);
    assert_eq!(it.size_hint(), (2, Some(2)));
    assert_eq!(*it.next_back().unwrap(), 4);
    assert_eq!(it.size_hint(), (1, Some(1)));
    assert_eq!(*it.next_back().unwrap(), 5);
    assert!(it.next_back().is_none());
    assert!(it.next().is_none());
}

#[kani::proof]
fn test_mut_rev_iter() {
    let mut m = generate_test();
    for (i, elt) in m.iter_mut().rev().enumerate() {
        assert_eq!((6 - i) as i32, *elt);
    }
    let mut n = LinkedList::new();
    assert!(n.iter_mut().rev().next().is_none());
    n.push_front(4);
    let mut it = n.iter_mut().rev();
    assert!(it.next().is_some());
    assert!(it.next().is_none());
}

#[kani::proof]
fn test_clone_from() {
    // Short cloned from long
    {
        let v = vec![1, 2, 3, 4, 5];
        let u = vec![8, 7, 6, 2, 3, 4, 5];
        let mut m = list_from(&v);
        let n = list_from(&u);
        m.clone_from(&n);
        // check_links(&m);
        assert_eq!(m, n);
        for elt in u {
            assert_eq!(m.pop_front(), Some(elt))
        }
    }
    // Long cloned from short
    {
        let v = vec![1, 2, 3, 4, 5];
        let u = vec![6, 7, 8];
        let mut m = list_from(&v);
        let n = list_from(&u);
        m.clone_from(&n);
        // check_links(&m);
        assert_eq!(m, n);
        for elt in u {
            assert_eq!(m.pop_front(), Some(elt))
        }
    }
    // Two equal length lists
    {
        let v = vec![1, 2, 3, 4, 5];
        let u = vec![9, 8, 1, 2, 3];
        let mut m = list_from(&v);
        let n = list_from(&u);
        m.clone_from(&n);
        // check_links(&m);
        assert_eq!(m, n);
        for elt in u {
            assert_eq!(m.pop_front(), Some(elt))
        }
    }
}

#[kani::proof]
fn test_ord() {
    let n = list_from(&[]);
    let m = list_from(&[1, 2, 3]);
    assert!(n < m);
    assert!(m > n);
    assert!(n <= n);
    assert!(n >= n);
}

#[kani::proof]
fn test_ord_nan() {
    let nan = 0.0f64 / 0.0;
    let n = list_from(&[nan]);
    let m = list_from(&[nan]);
    assert!(!(n < m));
    assert!(!(n > m));
    assert!(!(n <= m));
    assert!(!(n >= m));

    let n = list_from(&[nan]);
    let one = list_from(&[1.0f64]);
    assert!(!(n < one));
    assert!(!(n > one));
    assert!(!(n <= one));
    assert!(!(n >= one));

    let u = list_from(&[1.0f64, 2.0, nan]);
    let v = list_from(&[1.0f64, 2.0, 3.0]);
    assert!(!(u < v));
    assert!(!(u > v));
    assert!(!(u <= v));
    assert!(!(u >= v));

    let s = list_from(&[1.0f64, 2.0, 4.0, 2.0]);
    let t = list_from(&[1.0f64, 2.0, 3.0, 2.0]);
    assert!(!(s < t));
    assert!(s > one);
    assert!(!(s <= one));
    assert!(s >= one);
}

#[kani::proof]
fn test_26021() {
    // There was a bug in split_off that failed to null out the RHS's head's prev ptr.
    // This caused the RHS's dtor to walk up into the LHS at drop and delete all of
    // its nodes.
    //
    // https://github.com/rust-lang/rust/issues/26021
    let mut v1 = LinkedList::new();
    v1.push_front(1);
    v1.push_front(1);
    v1.push_front(1);
    v1.push_front(1);
    let _ = v1.split_off(3); // Dropping this now should not cause laundry consumption
    assert_eq!(v1.len(), 3);

    assert_eq!(v1.iter().len(), 3);
    assert_eq!(v1.iter().collect::<Vec<_>>().len(), 3);
}

#[kani::proof]
fn test_split_off() {
    let mut v1 = LinkedList::new();
    v1.push_front(1);
    v1.push_front(1);
    v1.push_front(1);
    v1.push_front(1);

    // test all splits
    for ix in 0..1 + v1.len() {
        let mut a = v1.clone();
        let b = a.split_off(ix);
        // check_links(&a);
        // check_links(&b);
        a.extend(b);
        assert_eq!(v1, a);
    }
}

#[kani::proof]
fn test_split_off_2() {
    // singleton
    {
        let mut m = LinkedList::new();
        m.push_back(1);

        let p = m.split_off(0);
        assert_eq!(m.len(), 0);
        assert_eq!(p.len(), 1);
        assert_eq!(p.back(), Some(&1));
        assert_eq!(p.front(), Some(&1));
    }

    // not singleton, forwards
    {
        let u = vec![1, 2, 3, 4, 5];
        let mut m = list_from(&u);
        let mut n = m.split_off(2);
        assert_eq!(m.len(), 2);
        assert_eq!(n.len(), 3);
        for elt in 1..3 {
            assert_eq!(m.pop_front(), Some(elt));
        }
        for elt in 3..6 {
            assert_eq!(n.pop_front(), Some(elt));
        }
    }
    // not singleton, backwards
    {
        let u = vec![1, 2, 3, 4, 5];
        let mut m = list_from(&u);
        let mut n = m.split_off(4);
        assert_eq!(m.len(), 4);
        assert_eq!(n.len(), 1);
        for elt in 1..5 {
            assert_eq!(m.pop_front(), Some(elt));
        }
        for elt in 5..6 {
            assert_eq!(n.pop_front(), Some(elt));
        }
    }

    // no-op on the last index
    {
        let mut m = LinkedList::new();
        m.push_back(1);

        let p = m.split_off(1);
        assert_eq!(m.len(), 1);
        assert_eq!(p.len(), 0);
        assert_eq!(m.back(), Some(&1));
        assert_eq!(m.front(), Some(&1));
    }
}

#[kani::proof]
fn test_extract_if_test() {
    let mut m: LinkedList<u32> = LinkedList::new();
    m.extend(&[1, 2, 3, 4, 5, 6]);
    let deleted = m.extract_if(|v| *v < 4).collect::<Vec<_>>();

    // check_links(&m);

    assert_eq!(deleted, &[1, 2, 3]);
    assert_eq!(m.into_iter().collect::<Vec<_>>(), &[4, 5, 6]);
}

#[kani::proof]
fn test_drain_to_empty_test() {
    let mut m: LinkedList<u32> = LinkedList::new();
    m.extend(&[1, 2, 3, 4, 5, 6]);
    let deleted = m.extract_if(|_| true).collect::<Vec<_>>();

    // check_links(&m);

    assert_eq!(deleted, &[1, 2, 3, 4, 5, 6]);
    assert_eq!(m.into_iter().collect::<Vec<_>>(), &[]);
}

#[kani::proof]
fn test_cursor_move_peek() {
    let mut m: LinkedList<u32> = LinkedList::new();
    m.extend(&[1, 2, 3, 4, 5, 6]);
    let mut cursor = m.cursor_front();
    assert_eq!(cursor.current(), Some(&1));
    assert_eq!(cursor.peek_next(), Some(&2));
    assert_eq!(cursor.peek_prev(), None);
    assert_eq!(cursor.index(), Some(0));
    cursor.move_prev();
    assert_eq!(cursor.current(), None);
    assert_eq!(cursor.peek_next(), Some(&1));
    assert_eq!(cursor.peek_prev(), Some(&6));
    assert_eq!(cursor.index(), None);
    cursor.move_next();
    cursor.move_next();
    assert_eq!(cursor.current(), Some(&2));
    assert_eq!(cursor.peek_next(), Some(&3));
    assert_eq!(cursor.peek_prev(), Some(&1));
    assert_eq!(cursor.index(), Some(1));

    let mut cursor = m.cursor_back();
    assert_eq!(cursor.current(), Some(&6));
    assert_eq!(cursor.peek_next(), None);
    assert_eq!(cursor.peek_prev(), Some(&5));
    assert_eq!(cursor.index(), Some(5));
    cursor.move_next();
    assert_eq!(cursor.current(), None);
    assert_eq!(cursor.peek_next(), Some(&1));
    assert_eq!(cursor.peek_prev(), Some(&6));
    assert_eq!(cursor.index(), None);
    cursor.move_prev();
    cursor.move_prev();
    assert_eq!(cursor.current(), Some(&5));
    assert_eq!(cursor.peek_next(), Some(&6));
    assert_eq!(cursor.peek_prev(), Some(&4));
    assert_eq!(cursor.index(), Some(4));

    let mut m: LinkedList<u32> = LinkedList::new();
    m.extend(&[1, 2, 3, 4, 5, 6]);
    let mut cursor = m.cursor_front_mut();
    assert_eq!(cursor.current(), Some(&mut 1));
    assert_eq!(cursor.peek_next(), Some(&mut 2));
    assert_eq!(cursor.peek_prev(), None);
    assert_eq!(cursor.index(), Some(0));
    cursor.move_prev();
    assert_eq!(cursor.current(), None);
    assert_eq!(cursor.peek_next(), Some(&mut 1));
    assert_eq!(cursor.peek_prev(), Some(&mut 6));
    assert_eq!(cursor.index(), None);
    cursor.move_next();
    cursor.move_next();
    assert_eq!(cursor.current(), Some(&mut 2));
    assert_eq!(cursor.peek_next(), Some(&mut 3));
    assert_eq!(cursor.peek_prev(), Some(&mut 1));
    assert_eq!(cursor.index(), Some(1));
    let mut cursor2 = cursor.as_cursor();
    assert_eq!(cursor2.current(), Some(&2));
    assert_eq!(cursor2.index(), Some(1));
    cursor2.move_next();
    assert_eq!(cursor2.current(), Some(&3));
    assert_eq!(cursor2.index(), Some(2));
    assert_eq!(cursor.current(), Some(&mut 2));
    assert_eq!(cursor.index(), Some(1));

    let mut m: LinkedList<u32> = LinkedList::new();
    m.extend(&[1, 2, 3, 4, 5, 6]);
    let mut cursor = m.cursor_back_mut();
    assert_eq!(cursor.current(), Some(&mut 6));
    assert_eq!(cursor.peek_next(), None);
    assert_eq!(cursor.peek_prev(), Some(&mut 5));
    assert_eq!(cursor.index(), Some(5));
    cursor.move_next();
    assert_eq!(cursor.current(), None);
    assert_eq!(cursor.peek_next(), Some(&mut 1));
    assert_eq!(cursor.peek_prev(), Some(&mut 6));
    assert_eq!(cursor.index(), None);
    cursor.move_prev();
    cursor.move_prev();
    assert_eq!(cursor.current(), Some(&mut 5));
    assert_eq!(cursor.peek_next(), Some(&mut 6));
    assert_eq!(cursor.peek_prev(), Some(&mut 4));
    assert_eq!(cursor.index(), Some(4));
    let mut cursor2 = cursor.as_cursor();
    assert_eq!(cursor2.current(), Some(&5));
    assert_eq!(cursor2.index(), Some(4));
    cursor2.move_prev();
    assert_eq!(cursor2.current(), Some(&4));
    assert_eq!(cursor2.index(), Some(3));
    assert_eq!(cursor.current(), Some(&mut 5));
    assert_eq!(cursor.index(), Some(4));
}

#[kani::proof]
fn test_cursor_mut_insert() {
    let mut m: LinkedList<u32> = LinkedList::new();
    m.extend(&[1, 2, 3, 4, 5, 6]);
    let mut cursor = m.cursor_front_mut();
    cursor.insert_before(7);
    cursor.insert_after(8);
    // check_links(&m);
    assert_eq!(
        m.iter().cloned().collect::<Vec<_>>(),
        &[7, 1, 8, 2, 3, 4, 5, 6]
    );
    let mut cursor = m.cursor_front_mut();
    cursor.move_prev();
    cursor.insert_before(9);
    cursor.insert_after(10);
    // check_links(&m);
    assert_eq!(
        m.iter().cloned().collect::<Vec<_>>(),
        &[10, 7, 1, 8, 2, 3, 4, 5, 6, 9]
    );
    let mut cursor = m.cursor_front_mut();
    cursor.move_prev();
    assert_eq!(cursor.remove_current(), None);
    cursor.move_next();
    cursor.move_next();
    assert_eq!(cursor.remove_current(), Some(7));
    cursor.move_prev();
    cursor.move_prev();
    cursor.move_prev();
    assert_eq!(cursor.remove_current(), Some(9));
    cursor.move_next();
    assert_eq!(cursor.remove_current(), Some(10));
    // check_links(&m);
    assert_eq!(
        m.iter().cloned().collect::<Vec<_>>(),
        &[1, 8, 2, 3, 4, 5, 6]
    );
    let mut cursor = m.cursor_front_mut();
    let mut p: LinkedList<u32> = LinkedList::new();
    p.extend(&[100, 101, 102, 103]);
    let mut q: LinkedList<u32> = LinkedList::new();
    q.extend(&[200, 201, 202, 203]);
    cursor.splice_after(p);
    cursor.splice_before(q);
    // check_links(&m);
    assert_eq!(
        m.iter().cloned().collect::<Vec<_>>(),
        &[200, 201, 202, 203, 1, 100, 101, 102, 103, 8, 2, 3, 4, 5, 6]
    );
    let mut cursor = m.cursor_front_mut();
    cursor.move_prev();
    let tmp = cursor.split_before();
    assert_eq!(m.into_iter().collect::<Vec<_>>(), &[]);
    m = tmp;
    let mut cursor = m.cursor_front_mut();
    cursor.move_next();
    cursor.move_next();
    cursor.move_next();
    cursor.move_next();
    cursor.move_next();
    cursor.move_next();
    let tmp = cursor.split_after();
    assert_eq!(
        tmp.into_iter().collect::<Vec<_>>(),
        &[102, 103, 8, 2, 3, 4, 5, 6]
    );
    // check_links(&m);
    assert_eq!(
        m.iter().cloned().collect::<Vec<_>>(),
        &[200, 201, 202, 203, 1, 100, 101]
    );
}

#[kani::proof]
fn test_cursor_push_front_back() {
    let mut ll: LinkedList<u32> = LinkedList::new();
    ll.extend(&[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    let mut c = ll.cursor_front_mut();
    assert_eq!(c.current(), Some(&mut 1));
    assert_eq!(c.index(), Some(0));
    c.push_front(0);
    assert_eq!(c.current(), Some(&mut 1));
    assert_eq!(c.peek_prev(), Some(&mut 0));
    assert_eq!(c.index(), Some(1));
    c.push_back(11);
    drop(c);
    let p = ll.cursor_back().front().unwrap();
    assert_eq!(p, &0);
    assert_eq!(ll, (0..12).collect());
    // check_links(&ll);
}

#[kani::proof]
fn test_extend_ref() {
    let mut a = LinkedList::new();
    a.push_back(1);

    a.extend(&[2, 3, 4]);

    assert_eq!(a.len(), 4);
    assert_eq!(a, list_from(&[1, 2, 3, 4]));

    let mut b = LinkedList::new();
    b.push_back(5);
    b.push_back(6);
    a.extend(&b);

    assert_eq!(a.len(), 6);
    assert_eq!(a, list_from(&[1, 2, 3, 4, 5, 6]));
}

#[kani::proof]
fn test_extend() {
    let mut a = LinkedList::new();
    a.push_back(1);
    a.extend(vec![2, 3, 4]); // uses iterator

    assert_eq!(a.len(), 4);
    assert!(a.iter().eq(&[1, 2, 3, 4]));

    let b: LinkedList<_> = [5, 6, 7].into_iter().collect();
    a.extend(b); // specializes to `append`

    assert_eq!(a.len(), 7);
    assert!(a.iter().eq(&[1, 2, 3, 4, 5, 6, 7]));
}

#[kani::proof]
fn test_contains() {
    let mut l = LinkedList::new();
    l.extend(&[2, 3, 4]);

    assert!(l.contains(&3));
    assert!(!l.contains(&1));

    l.clear();

    assert!(!l.contains(&3));
}

#[kani::proof]
fn test_extract_if_empty() {
    let mut list: LinkedList<i32> = LinkedList::new();

    {
        let mut iter = list.extract_if(|_| true);
        assert_eq!(iter.size_hint(), (0, Some(0)));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.size_hint(), (0, Some(0)));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    assert_eq!(list.len(), 0);
    assert_eq!(list.into_iter().collect::<Vec<_>>(), vec![]);
}

#[kani::proof]
fn test_extract_if_zst() {
    let mut list: LinkedList<_> = [(), (), (), (), ()].into_iter().collect();
    let initial_len = list.len();
    let mut count = 0;

    {
        let mut iter = list.extract_if(|_| true);
        assert_eq!(iter.size_hint(), (0, Some(initial_len)));
        while let Some(_) = iter.next() {
            count += 1;
            assert_eq!(iter.size_hint(), (0, Some(initial_len - count)));
        }
        assert_eq!(iter.size_hint(), (0, Some(0)));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    assert_eq!(count, initial_len);
    assert_eq!(list.len(), 0);
    assert_eq!(list.into_iter().collect::<Vec<_>>(), vec![]);
}

#[kani::proof]
fn test_extract_if_false() {
    let mut list: LinkedList<_> = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].into_iter().collect();

    let initial_len = list.len();
    let mut count = 0;

    {
        let mut iter = list.extract_if(|_| false);
        assert_eq!(iter.size_hint(), (0, Some(initial_len)));
        for _ in iter.by_ref() {
            count += 1;
        }
        assert_eq!(iter.size_hint(), (0, Some(0)));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    assert_eq!(count, 0);
    assert_eq!(list.len(), initial_len);
    assert_eq!(
        list.into_iter().collect::<Vec<_>>(),
        vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    );
}

#[kani::proof]
fn test_extract_if_true() {
    let mut list: LinkedList<_> = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].into_iter().collect();

    let initial_len = list.len();
    let mut count = 0;

    {
        let mut iter = list.extract_if(|_| true);
        assert_eq!(iter.size_hint(), (0, Some(initial_len)));
        while let Some(_) = iter.next() {
            count += 1;
            assert_eq!(iter.size_hint(), (0, Some(initial_len - count)));
        }
        assert_eq!(iter.size_hint(), (0, Some(0)));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    assert_eq!(count, initial_len);
    assert_eq!(list.len(), 0);
    assert_eq!(list.into_iter().collect::<Vec<_>>(), vec![]);
}

#[kani::proof]
fn test_extract_if_complex() {
    //             [+xxx++++++xxxxx++++x+x++]
    let mut list = [
        1, 2, 4, 6, 7, 9, 11, 13, 15, 17, 18, 20, 22, 24, 26, 27, 29, 31, 33, 34, 35, 36, 37, 39,
    ]
    .into_iter()
    .collect::<LinkedList<_>>();

    let removed = list.extract_if(|x| *x % 2 == 0).collect::<Vec<_>>();
    assert_eq!(removed.len(), 10);
    assert_eq!(removed, vec![2, 4, 6, 18, 20, 22, 24, 26, 34, 36]);

    assert_eq!(list.len(), 14);
    assert_eq!(
        list.into_iter().collect::<Vec<_>>(),
        vec![1, 7, 9, 11, 13, 15, 17, 27, 29, 31, 33, 35, 37, 39]
    );
}

#[kani::proof]
fn test_allocator() {
    use std::alloc::{AllocError, Allocator, Layout};
    use std::cell::Cell;

    struct A {
        has_allocated: Cell<bool>,
        has_deallocated: Cell<bool>,
    }

    unsafe impl Allocator for A {
        fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
            assert!(!self.has_allocated.get());
            self.has_allocated.set(true);

            Global.allocate(layout)
        }

        unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
            assert!(!self.has_deallocated.get());
            self.has_deallocated.set(true);

            unsafe { Global.deallocate(ptr, layout) }
        }
    }

    let alloc = &A {
        has_allocated: Cell::new(false),
        has_deallocated: Cell::new(false),
    };
    {
        let mut list = LinkedList::new_in(alloc);
        list.push_back(5u32);
        list.remove(0);
    }

    assert!(alloc.has_allocated.get());
    assert!(alloc.has_deallocated.get());
}
