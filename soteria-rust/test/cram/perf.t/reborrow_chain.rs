// Deep reborrow chain: each call reborrows `&mut`, adding a child tag to the SAME
// location's borrow tree, and every tag stays live on the recursion stack — so the
// tree grows to ~N with nothing reclaimable. Every `*r += 1` is a TB access over
// that whole growing tree, i.e. O(N^2) access work over a large *live* tree. This
// is the sensitive sentinel for `Raw.access`; see reborrow_tree.rs for the bushy,
// reclaiming counterpart where `access` is no longer the bottleneck.
fn deep(r: &mut u32, n: u32) {
    if n == 0 {
        return;
    }
    *r += 1;
    deep(&mut *r, n - 1);
}

fn main() {
    let mut x = 0u32;
    deep(&mut x, 600);
    soteria::assert(x == 600, "ok");
}
