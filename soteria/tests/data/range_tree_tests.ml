module RT = Soteria.Data.Range_tree

let rec check_height (t : _ RT.t) =
  let lo, hi = t.range in
  match t.children with
  | None ->
      Alcotest.(check int)
        (Printf.sprintf "height at [%d..%d)" lo hi)
        0 (RT.height t)
  | Some (l, r) ->
      Alcotest.(check int)
        (Printf.sprintf "height at [%d..%d)" lo hi)
        (1 + max (RT.height l) (RT.height r))
        (RT.height t);
      check_height l;
      check_height r

let rec check_balanced (t : _ RT.t) =
  let lo, hi = t.range in
  Alcotest.(check bool)
    (Printf.sprintf "is_balanced flag at [%d..%d)" lo hi)
    true (RT.is_balanced t);
  match t.children with
  | None -> ()
  | Some (l, r) ->
      Alcotest.(check bool)
        (Printf.sprintf "balance at [%d..%d) (child heights %d vs %d)" lo hi
           (RT.height l) (RT.height r))
        true
        (abs (RT.height l - RT.height r) <= 1);
      (* Invariant: a balanced node implies both children are also balanced *)
      Alcotest.(check bool)
        (Printf.sprintf "left child is_balanced at [%d..%d)" lo hi)
        true (RT.is_balanced l);
      Alcotest.(check bool)
        (Printf.sprintf "right child is_balanced at [%d..%d)" lo hi)
        true (RT.is_balanced r);
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

(* ── Helpers using build (AVL on construction)
   ────────────────────────────── *)

let leaf lo hi =
  let node = List.init (hi - lo) (fun i -> lo + i) in
  RT.build ~node ~range:(lo, hi) ~merge:( @ ) ()

let inner lo hi l r =
  let node = List.init (hi - lo) (fun i -> lo + i) in
  RT.build ~node ~range:(lo, hi) ~merge:( @ ) ~children:(l, r) ()

(* ── Helpers using make_raw (no balancing) + rebuild ──────────────────── *)

let leaf_u lo hi =
  let node = List.init (hi - lo) (fun i -> lo + i) in
  RT.make_raw ~node ~range:(lo, hi) ()

let inner_u lo hi l r =
  let node = List.init (hi - lo) (fun i -> lo + i) in
  RT.make_raw ~node ~range:(lo, hi) ~children:(l, r) ()

let rebuild t = RT.rebuild ~merge:( @ ) t

(* ── build tests ─────────────────────────────────────────────────────────── *)

let test_leaf () =
  let t = leaf 0 10 in
  Alcotest.(check int) "height" 0 (RT.height t);
  Alcotest.(check bool) "no children" true (Option.is_none t.children);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

let test_balanced_pair () =
  let l = leaf 0 5 and r = leaf 5 10 in
  let t = inner 0 10 l r in
  Alcotest.(check int) "height" 1 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

(* left-LEFT heavy: left child h=2, right child h=0.
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
  let d = leaf 4 6 in
  let left = inner 0 6 c d in
  let right = leaf 6 8 in
  let t = inner 0 8 left right in
  Alcotest.(check int) "height after rotation" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

(* right-RIGHT heavy: mirror of left-left.
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
  let a = leaf 4 6 in
  let right = inner 4 10 a d in
  let left = leaf 0 4 in
  let t = inner 0 10 left right in
  Alcotest.(check int) "height after rotation" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

(* left-RIGHT heavy: double rotation required.
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
  let left = inner 0 6 a lr in
  let right = leaf 6 8 in
  let t = inner 0 8 left right in
  Alcotest.(check int) "height after double rotation" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

(* right-LEFT heavy: double rotation required.
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
  let c = leaf 8 10 in
  let right = inner 4 10 rl c in
  let left = leaf 0 4 in
  let t = inner 0 10 left right in
  Alcotest.(check int) "height after double rotation" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

(* Perfectly balanced four-leaf tree: no rotation triggered.
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
  let cd = inner 4 8 c d in
  let t = inner 0 8 ab cd in
  Alcotest.(check int) "height" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

(* Large imbalance: height-1 left, height-5 right. *)
let test_large_imbalance () =
  let rec balanced lo hi =
    if hi - lo = 1 then leaf lo hi
    else
      let mid = (lo + hi) / 2 in
      inner lo hi (balanced lo mid) (balanced mid hi)
  in
  let left = inner 0 2 (leaf 0 1) (leaf 1 2) in
  let right = balanced 2 34 in
  Alcotest.(check int) "left height pre-check" 1 (RT.height left);
  Alcotest.(check int) "right height pre-check" 5 (RT.height right);
  let t = inner 0 34 left right in
  Alcotest.(check int) "height after rotation cascade" 6 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 34) t.range;
  check_all t

(* ── rebuild tests (same shapes, built with make_raw then rebuilt) ─── *)

let test_leaf_rebuild () =
  let t = rebuild (leaf_u 0 10) in
  Alcotest.(check int) "height" 0 (RT.height t);
  Alcotest.(check bool) "no children" true (Option.is_none t.children);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

let test_balanced_pair_rebuild () =
  let l = leaf_u 0 5 and r = leaf_u 5 10 in
  let t = rebuild (inner_u 0 10 l r) in
  Alcotest.(check int) "height" 1 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

let test_left_left_heavy_rebuild () =
  let a = leaf_u 0 2 and b = leaf_u 2 4 in
  let c = inner_u 0 4 a b in
  let d = leaf_u 4 6 in
  let left = inner_u 0 6 c d in
  let right = leaf_u 6 8 in
  let t = rebuild (inner_u 0 8 left right) in
  Alcotest.(check int) "height after rebuild" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

let test_right_right_heavy_rebuild () =
  let b = leaf_u 6 8 and c = leaf_u 8 10 in
  let d = inner_u 6 10 b c in
  let a = leaf_u 4 6 in
  let right = inner_u 4 10 a d in
  let left = leaf_u 0 4 in
  let t = rebuild (inner_u 0 10 left right) in
  Alcotest.(check int) "height after rebuild" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

let test_left_right_heavy_rebuild () =
  let a = leaf_u 0 2 in
  let b = leaf_u 2 4 and c = leaf_u 4 6 in
  let lr = inner_u 2 6 b c in
  let left = inner_u 0 6 a lr in
  let right = leaf_u 6 8 in
  let t = rebuild (inner_u 0 8 left right) in
  Alcotest.(check int) "height after rebuild" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

let test_right_left_heavy_rebuild () =
  let a = leaf_u 4 6 and b = leaf_u 6 8 in
  let rl = inner_u 4 8 a b in
  let c = leaf_u 8 10 in
  let right = inner_u 4 10 rl c in
  let left = leaf_u 0 4 in
  let t = rebuild (inner_u 0 10 left right) in
  Alcotest.(check int) "height after rebuild" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 10) t.range;
  check_all t

let test_full_balanced_rebuild () =
  let a = leaf_u 0 2 and b = leaf_u 2 4 in
  let c = leaf_u 4 6 and d = leaf_u 6 8 in
  let ab = inner_u 0 4 a b in
  let cd = inner_u 4 8 c d in
  let t = rebuild (inner_u 0 8 ab cd) in
  Alcotest.(check int) "height" 2 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 8) t.range;
  check_all t

let test_large_imbalance_rebuild () =
  let rec balanced_u lo hi =
    if hi - lo = 1 then leaf_u lo hi
    else
      let mid = (lo + hi) / 2 in
      inner_u lo hi (balanced_u lo mid) (balanced_u mid hi)
  in
  let left = inner_u 0 2 (leaf_u 0 1) (leaf_u 1 2) in
  let right = balanced_u 2 34 in
  let t = rebuild (inner_u 0 34 left right) in
  Alcotest.(check int) "height after rebuild" 6 (RT.height t);
  Alcotest.(check (pair int int)) "range" (0, 34) t.range;
  check_all t

(* ── is_balanced flag tests ─────────────────────────────────────────────── *)

(* make_raw on an unbalanced tree should report is_balanced = false at root *)
let test_make_raw_unbalanced_flag () =
  let a = leaf_u 0 2 and b = leaf_u 2 4 in
  let c = inner_u 0 4 a b in
  let d = leaf_u 4 6 in
  let left = inner_u 0 6 c d in
  let right = leaf_u 6 8 in
  let t = inner_u 0 8 left right in
  Alcotest.(check bool)
    "root is_balanced false before rebuild" false (RT.is_balanced t)

(* After rebuild the flag must be true everywhere *)
let test_make_raw_balanced_after_rebuild () =
  let a = leaf_u 0 2 and b = leaf_u 2 4 in
  let c = inner_u 0 4 a b in
  let d = leaf_u 4 6 in
  let left = inner_u 0 6 c d in
  let right = leaf_u 6 8 in
  let t = rebuild (inner_u 0 8 left right) in
  check_balanced t

(* A node whose child is internally unbalanced must itself be marked unbalanced,
   even when the immediate child heights differ by at most 1.

   Tree shape (built entirely with make_raw):

   root [0..7) h=3 / \ left [0..4) h=3 right [4..7) h=2 / \ e [0..3) h=2 f
   [3..4) h=0 / \ c [0..2) d [2..3) h=1 h=0 *)
let test_is_balanced_requires_balanced_children () =
  let a = leaf_u 0 1 and b = leaf_u 1 2 in
  let c = inner_u 0 2 a b in
  let d = leaf_u 2 3 in
  let e = inner_u 0 3 c d in
  let f = leaf_u 3 4 in
  let left = inner_u 0 4 e f in
  (* h=3, internally LL-heavy: is_balanced=false *)
  let g = leaf_u 4 5 and h_ = leaf_u 5 6 in
  let gh = inner_u 4 6 g h_ in
  let i = leaf_u 6 7 in
  let right = inner_u 4 7 gh i in
  (* h=2, balanced *)
  (* abs(3-2)=1 so the parent's own heights are fine, but left is not
     balanced *)
  let t = inner_u 0 7 left right in
  Alcotest.(check bool) "is_balanced = false" false (RT.is_balanced t)

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
      ( "rebuild",
        [
          Alcotest.test_case "leaf" `Quick test_leaf_rebuild;
          Alcotest.test_case "balanced pair" `Quick test_balanced_pair_rebuild;
          Alcotest.test_case "left-left heavy" `Quick
            test_left_left_heavy_rebuild;
          Alcotest.test_case "right-right heavy" `Quick
            test_right_right_heavy_rebuild;
          Alcotest.test_case "left-right heavy" `Quick
            test_left_right_heavy_rebuild;
          Alcotest.test_case "right-left heavy" `Quick
            test_right_left_heavy_rebuild;
          Alcotest.test_case "perfectly balanced 4-leaf" `Quick
            test_full_balanced_rebuild;
          Alcotest.test_case "large imbalance h1 vs h5" `Quick
            test_large_imbalance_rebuild;
        ] );
      ( "is_balanced flag",
        [
          Alcotest.test_case "make_raw unbalanced sets flag false" `Quick
            test_make_raw_unbalanced_flag;
          Alcotest.test_case "rebuild sets flag true everywhere" `Quick
            test_make_raw_balanced_after_rebuild;
          Alcotest.test_case "is_balanced requires balanced children" `Quick
            test_is_balanced_requires_balanced_children;
        ] );
    ]
