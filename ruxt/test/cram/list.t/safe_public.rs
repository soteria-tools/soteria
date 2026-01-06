use std::ptr::NonNull;

pub struct LinkedList {
    head: Option<NonNull<Node>>,
    tail: Option<NonNull<Node>>,
}

struct Node {
    next: Option<NonNull<Node>>,
}

impl Node {
    fn new() -> Self {
        Node { next: None }
    }
}

impl LinkedList {
    pub const fn new() -> Self {
        LinkedList {
            head: None,
            tail: None,
        }
    }

    pub fn push_front(&mut self) {
        let node = Box::new(Node::new());
        let node_ptr = NonNull::from(Box::leak(node));
        unsafe {
            (*node_ptr.as_ptr()).next = self.head;
            let node = Some(node_ptr);

            match self.head {
                None => self.tail = node,
                Some(_) => (),
            }

            self.head = node;
        }
    }

    fn pop_front_node(&mut self) -> Option<Box<Node>> {
        self.head.map(|node| unsafe {
            let node = Box::from_raw(node.as_ptr());
            self.head = node.next;

            match self.head {
                None => self.tail = None,
                Some(_) => (),
            }
            node
        })
    }
}

impl Drop for LinkedList {
    fn drop(&mut self) {
        while self.pop_front_node().is_some() {}
    }
}
