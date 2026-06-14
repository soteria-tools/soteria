// Balanced reborrow tree: each level makes two sequential `&mut` reborrows of the
// same location, so one location's borrow tree is bushy (depth 10, 2^10 leaves)
// rather than a single chain. Every leaf write is Foreign to the other subtrees'
// tags, and completed subtrees' tags die — so this exercises the Foreign
// transitions and compaction *reclamation*, on top of the O(borrows) per-access
// walk over the (large) tree.
fn rec(r: &mut u32, depth: u32) {
    if depth == 0 {
        *r += 1;
        return;
    }
    rec(&mut *r, depth - 1);
    rec(&mut *r, depth - 1);
}

fn main() {
    let mut x = 0u32;
    rec(&mut x, 10);
    soteria::assert(x == 1024, "ok");
}
