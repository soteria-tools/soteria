module RT = Soteria.Data.Range_tree

let rec check_height (t : _ RT.t) =
  let lo, hi = t.range in
  match t.children with
  | None ->
      Alcotest.(check int)
        (Printf.sprintf "height at [%d..%d)" lo hi)
        0 t.height
  | Some (l, r) ->
      Alcotest.(check int)
        (Printf.sprintf "height at [%d..%d)" lo hi)
        (1 + max l.height r.height)
        t.height;
      check_height l;
      check_height r

let rec check_balanced (t : _ RT.t) =
  match t.children with
  | None -> ()
  | Some (l, r) ->
      let lo, hi = t.range in
      Alcotest.(check bool)
        (Printf.sprintf "balance at [%d..%d) (child heights %d vs %d)" lo hi
           l.height r.height)
        true
        (abs (l.height - r.height) <= 1);
      check_balanced l;
      check_balanced r

let rec check_ranges (t : _ RT.t) =
  match t.children with
  | None -> ()
  | Some (l, r) ->
      let lo, hi = t.range in
      Alcotest.(check int)
        (Printf.sprintf "left child lo at [%d..%d)" lo hi)
        lo (fst l.range);
      Alcotest.(check int)
        (Printf.sprintf "mid point at [%d..%d)" lo hi)
        (snd l.range) (fst r.range);
      Alcotest.(check int)
        (Printf.sprintf "right child hi at [%d..%d)" lo hi)
        hi (snd r.range);
      check_ranges l;
      check_ranges r

let rec check_content (t : (int list, int) RT.t) =
  let lo, hi = t.range in
  let expected = List.init (hi - lo) (fun i -> lo + i) in
  Alcotest.(check (list int))
    (Printf.sprintf "content at [%d..%d)" lo hi)
    expected t.node;
  match t.children with
  | None -> ()
  | Some (l, r) ->
      check_content l;
      check_content r

let check_all t =
  check_height t;
  check_balanced t;
  check_ranges t;
  check_content t

(* Leaf whose node is the list of integers in [lo, hi). *)
let leaf lo hi =
  let node = List.init (hi - lo) (fun i -> lo + i) in
  RT.build ~node ~range:(lo, hi) ~merge:( @ ) ()

(* Inner node covering [lo, hi) with children l and r. *)
let inner lo hi l r =
  let node = List.init (hi - lo) (fun i -> lo + i) in
  RT.build ~node ~range:(lo, hi) ~merge:( @ ) ~children:(l, r) ()

(* Test 1 ─ single leaf: trivially balanced, height 0. *)
let test_leaf () =
  let t = leaf 0 10 in
  Alcotest.(check int) "height" 0 t.height;
  Alcotest.(check bool) "no children" true (Option.is_none t.children);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

(* Test 2 ─ two leaves joined: no rotation needed, height 1. [0──5) and [5──10)
   → root [0──10). *)
let test_balanced_pair () =
  let l = leaf 0 5 and r = leaf 5 10 in
  let t = inner 0 10 l r in
  Alcotest.(check int) "height" 1 t.height;
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

(* Test 3 ─ left-LEFT heavy: left child h=2, right child h=0. left's own left
 * grandchild is the taller one, so a single right rotation suffices and the
 * result must be balanced at height 2.
 *
 *  Before build:
 *                     [0──8) h=?
 *                       / \
 *              [0──6) h=2 [6──8) h=0
 *                / \
 *       [0──4) h=1 [4──6) h=0
 *          / \
 *     [0──2) [2──4) both h=0
 *)
let test_left_left_heavy () =
  let a = leaf 0 2 and b = leaf 2 4 in
  let c = inner 0 4 a b in
  (* h=1 *)
  let d = leaf 4 6 in
  (* h=0 *)
  let left = inner 0 6 c d in
  (* h=2, left-LEFT heavy: c.h > d.h *)
  let right = leaf 6 8 in
  (* h=0 *)
  let t = inner 0 8 left right in
  (* hl=2, hr=0 → rotation *)
  Alcotest.(check int) "height after rotation" 2 t.height;
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

(* Test 4 ─ right-RIGHT heavy: left child h=0, right child h=2. Mirror of test
 *  3: right's own right grandchild is the taller one, so a single left rotation
 *  suffices.
 *
 *  Before build:
 *                        [0──10) h=?
 *                          / \
 *                   [0──4) h=0 [4──10) h=2
 *                                / \
 *                        [4──6) h=0 [6──10) h=1
 *                                     / \
 *                              [6──8) h=0 [8──10) h=0
 *)
let test_right_right_heavy () =
  let b = leaf 6 8 and c = leaf 8 10 in
  let d = inner 6 10 b c in
  (* h=1 *)
  let a = leaf 4 6 in
  (* h=0 *)
  let right = inner 4 10 a d in
  (* h=2, right-RIGHT heavy: d.h > a.h *)
  let left = leaf 0 4 in
  (* h=0 *)
  let t = inner 0 10 left right in
  (* hl=0, hr=2 → rotation *)
  Alcotest.(check int) "height after rotation" 2 t.height;
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

(* Test 5 ─ left-RIGHT heavy: left child h=2, but lr is taller than ll, so a
 *  single right rotation would loop — a double rotation is required.
 *
 *  Before build:
 *                   [0──8) h=?
 *                     / \
 *            [0──6) h=2 [6──8) h=0
 *              / \
 *       [0──2) h=0 [2──6) h=1
 *                    / \
 *               [2──4) [4──6) both h=0
 *)
let test_left_right_heavy () =
  let a = leaf 0 2 in
  let b = leaf 2 4 and c = leaf 4 6 in
  let lr = inner 2 6 b c in
  (* h=1, lr is taller than ll *)
  let left = inner 0 6 a lr in
  (* h=2, left-RIGHT heavy: lr.h > ll.h *)
  let right = leaf 6 8 in
  (* h=0 *)
  let t = inner 0 8 left right in
  Alcotest.(check int) "height after double rotation" 2 t.height;
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

(* Test 6 ─ right-LEFT heavy: right child h=2, but rl is taller than rr, so a
 *  single left rotation would loop — a double rotation is required.
 *
 *  Before build:
 *             [0──10) h=?
 *               / \
 *       [0──4) h=0 [4──10) h=2
 *                    / \
 *            [4──8) h=1 [8──10) h=0
 *              / \
 *         [4──6) [6──8) both h=0
 *)
let test_right_left_heavy () =
  let a = leaf 4 6 and b = leaf 6 8 in
  let rl = inner 4 8 a b in
  (* h=1, rl is taller than rr *)
  let c = leaf 8 10 in
  let right = inner 4 10 rl c in
  (* h=2, right-LEFT heavy: rl.h > rr.h *)
  let left = leaf 0 4 in
  (* h=0 *)
  let t = inner 0 10 left right in
  Alcotest.(check int) "height after double rotation" 2 t.height;
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

(* Test 7 ─ perfectly balanced four-leaf tree: no rotation triggered.
 *
 *  Shape:
 *                        [0──8) h=2
 *                          / \
 *                  [0──4) h=1  [4──8) h=1
 *                    / \         / \
 *              [0──2) [2──4) [4──6)[6──8) all h=0
 *)
let test_full_balanced () =
  let a = leaf 0 2 and b = leaf 2 4 in
  let c = leaf 4 6 and d = leaf 6 8 in
  let ab = inner 0 4 a b in
  (* h=1 *)
  let cd = inner 4 8 c d in
  (* h=1 *)
  let t = inner 0 8 ab cd in
  (* hl=hr=1, no rotation *)
  Alcotest.(check int) "height" 2 t.height;
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

(* Test 8 ─ large imbalance: height-1 tree on the left, height-5 on the right.
   The height difference of 4 requires the rotation to recur through 4 levels
   before sub-trees become balanced. Each recursive call is right-RIGHT heavy
   (the balanced tree's right grandchild equals its left grandchild in height,
   so each step picks the right-right path).

   left : [0──2) h=1 (two leaves) right: [2──34) h=5 (perfectly balanced, 32
   leaves of width 1)

   Expected result height: 6, all three invariants must hold. *)
let test_large_imbalance () =
  (* Build a perfectly balanced tree over [lo, hi) where hi-lo is a power of
     2. *)
  let rec balanced lo hi =
    if hi - lo = 1 then leaf lo hi
    else
      let mid = (lo + hi) / 2 in
      inner lo hi (balanced lo mid) (balanced mid hi)
  in
  let left = inner 0 2 (leaf 0 1) (leaf 1 2) in
  (* h=1 *)
  let right = balanced 2 34 in
  (* h=5, 32 leaves *)
  Alcotest.(check int) "left height pre-check" 1 left.height;
  Alcotest.(check int) "right height pre-check" 5 right.height;
  let t = inner 0 34 left right in
  (* hl=1, hr=5 → 4-level rotation cascade *)
  Alcotest.(check int) "height after rotation cascade" 6 t.height;
  Alcotest.(check (pair int int)) "range" (0, 34) t.range;
  check_all t

(* ── Runner ─────────────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "Range_tree"
    [
      ( "build",
        [
          Alcotest.test_case "leaf" `Quick test_leaf;
          Alcotest.test_case "balanced pair" `Quick test_balanced_pair;
          Alcotest.test_case "left-left heavy" `Quick test_left_left_heavy;
          Alcotest.test_case "right-right heavy" `Quick test_right_right_heavy;
          Alcotest.test_case "left-right heavy" `Quick test_left_right_heavy;
          Alcotest.test_case "right-left heavy" `Quick test_right_left_heavy;
          Alcotest.test_case "perfectly balanced 4-leaf" `Quick
            test_full_balanced;
          Alcotest.test_case "large imbalance h1 vs h5" `Quick
            test_large_imbalance;
        ] );
    ]
