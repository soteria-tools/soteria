use std::marker::PhantomData;
use std::ptr::NonNull;

pub struct LinkedList<T> {
    head: Option<NonNull<Node<T>>>,
    tail: Option<NonNull<Node<T>>>,
    len: usize,
    marker: PhantomData<Box<Node<T>>>,
}

struct Node<T> {
    next: Option<NonNull<Node<T>>>,
    prev: Option<NonNull<Node<T>>>,
    element: T,
}

impl<T> Node<T> {
    fn new(element: T) -> Self {
        Node {
            next: None,
            prev: None,
            element,
        }
    }

    fn into_element(self: Box<Self>) -> T {
        self.element
    }
}

impl<T> LinkedList<T> {
    fn new() -> Self {
        Self {
            head: None,
            tail: None,
            len: 0,
            marker: PhantomData,
        }
    }

    fn push_back_node(&mut self, mut node: Box<Node<T>>) {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        unsafe {
            node.next = None;
            node.prev = self.tail;
            let node = Some(Box::leak(node).into());

            match self.tail {
                None => self.head = node,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(tail) => (*tail.as_ptr()).next = node,
            }

            self.tail = node;
            self.len += 1;
        }
    }

    /// Removes and returns the node at the back of the list.
    fn pop_back_node(&mut self) -> Option<Box<Node<T>>> {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        match self.tail {
            None => None,
            Some(node) => unsafe {
                let node = Box::from_raw(node.as_ptr());
                self.tail = node.prev;

                match self.tail {
                    None => self.head = None,
                    // Not creating new mutable (unique!) references overlapping `element`.
                    Some(tail) => (*tail.as_ptr()).next = None,
                }

                self.len -= 1;
                Some(node)
            },
        }
    }

    fn push_front_node(&mut self, mut node: Box<Node<T>>) {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        unsafe {
            node.next = self.head;
            node.prev = None;
            let node = Some(Box::leak(node).into());

            match self.head {
                None => self.tail = node,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(head) => (*head.as_ptr()).prev = node,
            }

            self.head = node;
            self.len += 1;
        }
    }

    fn pop_front_node(&mut self) -> Option<Box<Node<T>>> {
        // Original function uses map
        match self.head {
            None => None,
            Some(node) => unsafe {
                let node = Box::from_raw(node.as_ptr());
                self.head = node.next;

                match self.head {
                    None => self.tail = None,
                    // Not creating new mutable (unique!) references overlapping `element`.
                    Some(head) => (*head.as_ptr()).prev = None,
                }

                self.len -= 1;
                Some(node)
            },
        }
    }

    pub fn is_empty(&mut self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn front_mut(&mut self) -> Option<&mut T> {
        match self.head.as_mut() {
            None => None,
            Some(node) => unsafe { Some(&mut node.as_mut().element) },
        }
    }

    pub fn pop_front(&mut self) -> Option<T> {
        // Original implementation uses map
        match self.pop_front_node() {
            None => None,
            Some(node) => Some(node.into_element()),
        }
    }

    pub fn push_front(&mut self, elt: T) {
        self.push_front_node(Box::new(Node::new(elt)));
    }

    pub fn pop_back(&mut self) -> Option<T> {
        self.pop_back_node().map(|e| Node::into_element(e))
    }

    pub fn push_back(&mut self, elt: T) {
        self.push_back_node(Box::new(Node::new(elt)));
    }
}

#[kani::proof]
fn main() {
    let mut list: LinkedList<u8> = LinkedList::new();

    // Check empty list behaves right
    assert_eq!(list.pop_front(), None);
    assert_eq!(list.pop_back(), None);

    // Populate list
    let v1 = kani::any();
    let v2 = kani::any();
    let v3 = kani::any();
    list.push_front(v1);
    list.push_front(v2);
    list.push_front(v3);

    // Check normal removal
    assert_eq!(list.pop_back(), Some(v1));
    assert_eq!(list.pop_back(), Some(v2));

    // Push some more just to make sure nothing's corrupted
    let v4 = kani::any();
    let v5 = kani::any();
    list.push_front(v4);
    list.push_front(v5);

    // Check normal removal
    assert_eq!(list.pop_back(), Some(v3));
    assert_eq!(list.pop_back(), Some(v4));

    // Check exhaustion
    assert_eq!(list.pop_back(), Some(v5));
    assert_eq!(list.pop_back(), None);

    // Check the exhaustion case fixed the pointer right
    let v6 = kani::any();
    let v7 = kani::any();
    list.push_front(v6);
    list.push_front(v7);

    // Check normal removal
    assert_eq!(list.pop_back(), Some(v6));
    assert_eq!(list.pop_back(), Some(v7));
    assert_eq!(list.pop_back(), None);
}

#[kani::proof]
#[kani::unwind(11)]
#[cfg_attr(rusteria, rusteria::step_fuel(5000))]
#[cfg_attr(rusteria, rusteria::branch_fuel(10))]
fn do_rotation() {
    let n: u8 = kani::any();
    kani::assume(1 <= n && n <= 10);

    let mut list = LinkedList::new();
    let mut i = 0;
    while i < n {
        list.push_back(i);
        i += 1;
    }

    i = 0;
    while i < n {
        let x = list.pop_front();
        list.push_back(x.unwrap());
        i += 1;
    }

    assert_eq!(list.len(), n as usize);

    i = 0;
    while i < n {
        let x = list.pop_front();
        assert_eq!(x.unwrap(), i);
        i += 1;
    }

    assert_eq!(list.len(), 0);
}
