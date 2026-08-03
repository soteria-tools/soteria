open Test_register

let register = register "PatriciaTree"

module Int_key = struct
  type t = int

  let to_int = Fun.id
  let pp = Fmt.int
end

module M = PatriciaTree.MakeMap (Int_key)
module S = PatriciaTree.MakeSet (Int_key)

(* Traversal order is unspecified, so everything is compared sorted. *)
let bindings m = List.sort compare (M.fold (fun k v acc -> (k, v) :: acc) m [])
let elements s = List.sort compare (S.to_list s)
let of_list l = List.fold_left (fun m (k, v) -> M.add k v m) M.empty l
let set_of_list l = List.fold_left (fun s k -> S.add k s) S.empty l
let check_bindings = Alcotest.(check (list (pair int int))) "same bindings"
let check_elements = Alcotest.(check (list int)) "same elements"
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_opt = Alcotest.(check (option int))
let check_shared msg a b = Alcotest.(check bool) msg true (a == b)
let check_not_shared msg a b = Alcotest.(check bool) msg false (a == b)

(* Keys that straddle the sign bit: the branching bit of the topmost node is
   then [min_int], which is negative and must still count as the most
   significant one. *)
let sign_bit_keys = [ min_int; min_int + 1; -42; -1; 0; 1; 42; max_int - 1 ]

(* ------------------------------------------------------------------ *)
(* Basic map operations                                               *)
(* ------------------------------------------------------------------ *)

let empty_map =
  let@ () = register "empty map" in
  check_bool "is_empty" true (M.is_empty M.empty);
  check_int "cardinal" 0 (M.cardinal M.empty);
  check_bool "mem" false (M.mem 0 M.empty);
  check_opt "find_opt" None (M.find_opt 0 M.empty);
  check_bindings [] (bindings M.empty);
  check_bool "to_seq" true (Seq.is_empty (M.to_seq M.empty));
  check_bool "for_all" true (M.for_all (fun _ _ -> false) M.empty);
  check_bool "exists" false (M.exists (fun _ _ -> true) M.empty)

let singleton_map =
  let@ () = register "singleton" in
  let m = M.singleton 42 "v" in
  check_bool "is_empty" false (M.is_empty m);
  check_int "cardinal" 1 (M.cardinal m);
  check_bool "mem" true (M.mem 42 m);
  check_bool "mem other" false (M.mem 43 m);
  Alcotest.(check string) "find" "v" (M.find 42 m)

let add_find_mem =
  let@ () = register "add, find and mem" in
  let m = of_list [ (1, 10); (2, 20); (3, 30) ] in
  check_int "cardinal" 3 (M.cardinal m);
  List.iter
    (fun (k, v) ->
      check_bool (Fmt.str "mem %d" k) true (M.mem k m);
      check_int (Fmt.str "find %d" k) v (M.find k m);
      check_opt (Fmt.str "find_opt %d" k) (Some v) (M.find_opt k m))
    [ (1, 10); (2, 20); (3, 30) ];
  check_bool "mem absent" false (M.mem 4 m);
  check_opt "find_opt absent" None (M.find_opt 4 m)

let find_raises =
  let@ () = register "find raises Not_found" in
  let m = of_list [ (1, 10) ] in
  Alcotest.check_raises "absent key" Not_found (fun () -> ignore (M.find 2 m));
  Alcotest.check_raises "empty map" Not_found (fun () ->
      ignore (M.find 1 M.empty))

let add_replaces =
  let@ () = register "add replaces" in
  let m = of_list [ (1, 10); (2, 20) ] in
  let m = M.add 1 11 m in
  check_bindings [ (1, 11); (2, 20) ] (bindings m);
  check_int "cardinal unchanged" 2 (M.cardinal m)

let add_assert_new_ =
  let@ () = register "add_assert_new" in
  let m = M.add_assert_new 1 10 M.empty in
  let m = M.add_assert_new 2 20 m in
  check_bindings [ (1, 10); (2, 20) ] (bindings m);
  (match M.add_assert_new 1 99 m with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument on an already bound key");
  (* A removed key can be added again. *)
  let m = M.add_assert_new 1 11 (M.remove 1 m) in
  check_bindings [ (1, 11); (2, 20) ] (bindings m)

let remove_ =
  let@ () = register "remove" in
  let m = of_list [ (1, 10); (2, 20); (3, 30) ] in
  check_bindings [ (1, 10); (3, 30) ] (bindings (M.remove 2 m));
  check_bindings [ (1, 10); (2, 20); (3, 30) ] (bindings (M.remove 4 m));
  let m = List.fold_left (fun m k -> M.remove k m) m [ 1; 2; 3 ] in
  check_bool "empty again" true (M.is_empty m);
  check_bool "remove from empty" true (M.is_empty (M.remove 1 M.empty))

let update_ =
  let@ () = register "update" in
  let m = of_list [ (1, 10); (2, 20) ] in
  (* insert *)
  check_bindings
    [ (1, 10); (2, 20); (3, 30) ]
    (bindings (M.update 3 (function None -> Some 30 | Some _ -> None) m));
  (* modify *)
  check_bindings
    [ (1, 11); (2, 20) ]
    (bindings (M.update 1 (Option.map succ) m));
  (* delete *)
  check_bindings [ (2, 20) ] (bindings (M.update 1 (fun _ -> None) m));
  (* deleting an absent key changes nothing *)
  check_bindings [ (1, 10); (2, 20) ] (bindings (M.update 3 (fun _ -> None) m));
  (* the current binding is passed to [f] *)
  check_opt "sees current binding" (Some 10)
    (let seen = ref None in
     ignore
       (M.update 1
          (fun cur ->
            seen := cur;
            cur)
          m);
     !seen)

let cardinal_ =
  let@ () = register "cardinal" in
  check_int "empty" 0 (M.cardinal M.empty);
  check_int "singleton" 1 (M.cardinal (M.singleton 0 ()));
  let m = of_list (List.init 100 (fun i -> (i * 7, i))) in
  check_int "hundred" 100 (M.cardinal m);
  check_int "after remove" 99 (M.cardinal (M.remove 0 m));
  check_int "after redundant add" 100 (M.cardinal (M.add 7 1 m))

let large_map =
  let@ () = register "large map" in
  let n = 1000 in
  let m = of_list (List.init n (fun i -> (i, i * i))) in
  check_int "cardinal" n (M.cardinal m);
  for i = 0 to n - 1 do
    check_int (Fmt.str "find %d" i) (i * i) (M.find i m)
  done;
  let evens = List.filter (fun i -> i mod 2 = 0) (List.init n Fun.id) in
  let m = List.fold_left (fun m k -> M.remove k m) m evens in
  check_int "cardinal after removing evens" (n / 2) (M.cardinal m);
  check_bool "odds remain" true
    (List.for_all (fun i -> M.mem i m = (i mod 2 = 1)) (List.init n Fun.id))

(* ------------------------------------------------------------------ *)
(* Combining maps                                                     *)
(* ------------------------------------------------------------------ *)

let union_ =
  let@ () = register "union" in
  let m1 = of_list [ (1, 10); (2, 20) ] in
  let m2 = of_list [ (2, 22); (3, 30) ] in
  check_bindings
    [ (1, 10); (2, 20); (3, 30) ]
    (bindings (M.union (fun _ a _ -> a) m1 m2));
  check_bindings
    [ (1, 10); (2, 22); (3, 30) ]
    (bindings (M.union (fun _ _ b -> b) m1 m2));
  check_bindings
    [ (1, 10); (2, 20) ]
    (bindings (M.union (fun _ a _ -> a) m1 M.empty));
  check_bindings
    [ (2, 22); (3, 30) ]
    (bindings (M.union (fun _ a _ -> a) M.empty m2));
  (* The combining function receives the key, then the left then the right
     value. It must be idempotent, hence the [a = b] case. *)
  check_bindings
    [ (1, 10); (2, 2042); (3, 30) ]
    (bindings
       (M.union (fun k a b -> if a = b then a else (k * 1000) + a + b) m1 m2))

let inter_ =
  let@ () = register "inter" in
  let m1 = of_list [ (1, 10); (2, 20); (3, 30) ] in
  let m2 = of_list [ (2, 22); (3, 33); (4, 44) ] in
  check_bindings
    [ (2, 20); (3, 30) ]
    (bindings (M.inter (fun _ a _ -> a) m1 m2));
  check_bindings
    [ (2, 22); (3, 33) ]
    (bindings (M.inter (fun _ _ b -> b) m1 m2));
  check_bindings [] (bindings (M.inter (fun _ a _ -> a) m1 M.empty));
  check_bindings [] (bindings (M.inter (fun _ a _ -> a) M.empty m2));
  check_bindings []
    (bindings (M.inter (fun _ a _ -> a) m1 (of_list [ (5, 50) ])))

let equal_ =
  let@ () = register "equal" in
  let m1 = of_list [ (1, 10); (2, 20) ] in
  (* built in a different order, so a different sequence of intermediate
     tries *)
  let m2 = of_list [ (2, 20); (1, 10) ] in
  check_bool "same bindings" true (M.equal Int.equal m1 m2);
  check_bool "reflexive" true (M.equal Int.equal m1 m1);
  check_bool "different value" false
    (M.equal Int.equal m1 (of_list [ (1, 10); (2, 21) ]));
  check_bool "different key" false
    (M.equal Int.equal m1 (of_list [ (1, 10); (3, 20) ]));
  check_bool "extra binding" false (M.equal Int.equal m1 (M.add 3 30 m1));
  check_bool "empty" true (M.equal Int.equal M.empty M.empty);
  check_bool "empty vs not" false (M.equal Int.equal M.empty m1);
  (* the equality is used, not structural equality on values *)
  check_bool "custom equality" true
    (M.equal (fun a b -> a mod 10 = b mod 10) m1 (of_list [ (1, 20); (2, 30) ]))

let update_from_ =
  let@ () = register "update_from" in
  let m = of_list [ (1, 10); (2, 20); (3, 30) ] in
  let guide = of_list [ (2, 2); (3, 3); (4, 4) ] in
  (* inserts, updates, and leaves keys outside the guide alone *)
  check_bindings
    [ (1, 10); (2, 22); (3, 33); (4, 4) ]
    (bindings
       (M.update_from guide
          (fun _ cur y ->
            match cur with None -> Some y | Some x -> Some (x + y))
          m));
  (* returning [None] removes the binding *)
  check_bindings
    [ (1, 10) ]
    (bindings (M.update_from guide (fun _ _ _ -> None) m));
  (* an empty guide leaves the map alone *)
  check_bindings
    [ (1, 10); (2, 20); (3, 30) ]
    (bindings (M.update_from M.empty (fun _ _ _ -> None) m));
  (* a guide over an empty map only inserts *)
  check_bindings
    [ (2, 2); (3, 3); (4, 4) ]
    (bindings (M.update_from guide (fun _ _ y -> Some y) M.empty));
  (* the key and the current binding are passed to [f] *)
  check_bindings
    [ (1, 10); (2, 2022); (3, 3033); (4, 4004) ]
    (bindings
       (M.update_from guide
          (fun k cur y -> Some ((k * 1000) + Option.value ~default:0 cur + y))
          m));
  (* guides of a different value type *)
  check_bindings
    [ (1, 10); (2, 20); (3, 30); (4, 40) ]
    (bindings
       (M.update_from
          (of_list [ (4, "40") ])
          (fun _ _ y -> Some (int_of_string y))
          m))

(* ------------------------------------------------------------------ *)
(* Traversals                                                         *)
(* ------------------------------------------------------------------ *)

let iter_fold =
  let@ () = register "iter and fold" in
  let l = [ (1, 10); (2, 20); (3, 30) ] in
  let m = of_list l in
  let seen = ref [] in
  M.iter (fun k v -> seen := (k, v) :: !seen) m;
  check_bindings l (List.sort compare !seen);
  check_int "fold sums values" 60 (M.fold (fun _ v acc -> acc + v) m 0);
  check_int "fold sums keys" 6 (M.fold (fun k _ acc -> acc + k) m 0);
  check_int "fold on empty" 7 (M.fold (fun _ _ acc -> acc + 1) M.empty 7);
  let count = ref 0 in
  M.iter (fun _ _ -> incr count) M.empty;
  check_int "iter on empty" 0 !count

let for_all_exists =
  let@ () = register "for_all and exists" in
  let m = of_list [ (1, 10); (2, 20); (3, 30) ] in
  check_bool "for_all true" true (M.for_all (fun k v -> v = k * 10) m);
  check_bool "for_all false" false (M.for_all (fun _ v -> v > 10) m);
  check_bool "exists true" true (M.exists (fun _ v -> v = 20) m);
  check_bool "exists false" false (M.exists (fun _ v -> v = 25) m);
  check_bool "for_all sees keys" true (M.for_all (fun k _ -> k > 0) m);
  check_bool "exists sees keys" true (M.exists (fun k _ -> k = 3) m)

let seq_ =
  let@ () = register "to_seq and add_seq" in
  let l = [ (1, 10); (2, 20); (3, 30) ] in
  let m = of_list l in
  check_bindings l (List.sort compare (List.of_seq (M.to_seq m)));
  check_bindings l (bindings (M.add_seq (List.to_seq l) M.empty));
  (* [add_seq] replaces existing bindings *)
  check_bindings
    [ (1, 99); (2, 20); (3, 30) ]
    (bindings (M.add_seq (List.to_seq [ (1, 99) ]) m));
  check_bindings l (bindings (M.add_seq Seq.empty m));
  (* round trip *)
  check_bool "round trip" true
    (M.equal Int.equal (M.add_seq (M.to_seq m) M.empty) m)

let compact_strong =
  let@ () = register "compact is the identity on a strong map" in
  let m = of_list (List.init 50 (fun i -> (i * 3, i))) in
  check_shared "same map" m (M.compact m);
  check_shared "empty" M.empty (M.compact M.empty)

let pp_ =
  let@ () = register "pp" in
  let pp_map = Fmt.str "%a" (M.pp Fmt.int) in
  Alcotest.(check string) "empty" "{}" (pp_map M.empty);
  Alcotest.(check string) "singleton" "{1 -> 10}" (pp_map (M.singleton 1 10));
  Alcotest.(check string)
    "several" "{1 -> 10; 2 -> 20}"
    (pp_map (of_list [ (1, 10); (2, 20) ]));
  Alcotest.(check string) "set empty" "{}" (Fmt.str "%a" S.pp S.empty);
  Alcotest.(check string)
    "set several" "{1; 2; 3}"
    (Fmt.str "%a" S.pp (set_of_list [ 1; 2; 3 ]))

(* ------------------------------------------------------------------ *)
(* Keys straddling the sign bit                                       *)
(* ------------------------------------------------------------------ *)

let sign_bit_basics =
  let@ () = register "negative keys" in
  let m = of_list (List.map (fun k -> (k, k)) sign_bit_keys) in
  check_int "cardinal" (List.length sign_bit_keys) (M.cardinal m);
  List.iter
    (fun k -> check_opt (Fmt.str "find %d" k) (Some k) (M.find_opt k m))
    sign_bit_keys;
  check_opt "absent" None (M.find_opt 7 m);
  List.iter
    (fun k ->
      let m' = M.remove k m in
      check_int
        (Fmt.str "cardinal after removing %d" k)
        (List.length sign_bit_keys - 1)
        (M.cardinal m');
      check_bool (Fmt.str "%d is gone" k) false (M.mem k m'))
    sign_bit_keys

(* Merging tries whose topmost branching bit is the sign bit used to duplicate
   bindings when the branching bits were compared as signed integers. *)
let sign_bit_union =
  let@ () = register "union across the sign bit" in
  let a = set_of_list [ min_int; 0 ] in
  let b = set_of_list [ min_int; 1 ] in
  check_elements [ min_int; 0; 1 ] (elements (S.union a b));
  check_elements [ min_int; 0; 1 ] (elements (S.union b a));
  check_elements [ min_int ] (elements (S.inter a b));
  check_int "no duplicate min_int" 3 (List.length (S.to_list (S.union a b)));
  (* every pair drawn from the sign-straddling keys *)
  List.iter
    (fun k1 ->
      List.iter
        (fun k2 ->
          let l1 = [ k1; 0; 1 ] and l2 = [ k2; 0; 2 ] in
          let s1 = set_of_list l1 and s2 = set_of_list l2 in
          check_elements
            (List.sort_uniq compare (l1 @ l2))
            (elements (S.union s1 s2));
          check_elements
            (List.sort_uniq compare (List.filter (fun k -> List.mem k l2) l1))
            (elements (S.inter s1 s2)))
        sign_bit_keys)
    sign_bit_keys

(* ------------------------------------------------------------------ *)
(* Structural sharing                                                 *)
(* ------------------------------------------------------------------ *)

let sharing_no_op =
  let@ () = register "operations that change nothing return the same map" in
  let m = of_list (List.init 200 (fun i -> (i * 7, i))) in
  check_shared "add of the same value" m (M.add 70 10 m);
  check_shared "remove of an absent key" m (M.remove 3 m);
  check_shared "update returning its argument" m (M.update 70 (fun x -> x) m);
  check_shared "update of an absent key" m (M.update 3 (fun x -> x) m);
  check_shared "union with itself" m (M.union (fun _ a _ -> a) m m);
  check_shared "inter with itself" m (M.inter (fun _ a _ -> a) m m);
  check_shared "union with a submap" m
    (M.union (fun _ a _ -> a) m (M.remove 70 m));
  check_shared "union with a submap, flipped" m
    (M.union (fun _ a _ -> a) (M.remove 70 m) m);
  check_shared "inter with a supermap" m
    (M.inter (fun _ a _ -> a) m (M.add 1 1 m));
  check_shared "union with empty" m (M.union (fun _ a _ -> a) m M.empty);
  check_shared "update_from returning the current binding" m
    (M.update_from m (fun _ cur _ -> cur) m);
  check_shared "update_from with an empty guide" m
    (M.update_from M.empty (fun _ _ _ -> None) m);
  check_shared "update_from inserting nothing" m
    (M.update_from (of_list [ (3, 0) ]) (fun _ cur _ -> cur) m);
  (* a real change must not be silently shared away *)
  check_not_shared "add of a new key" m (M.add 3 3 m);
  check_not_shared "add of a new value" m (M.add 70 99 m);
  check_not_shared "remove of a bound key" m (M.remove 70 m)

let sharing_subtrees =
  let@ () = register "untouched subtrees are shared" in
  (* Keys 0-127 give a perfectly balanced trie: touching one leaf must leave the
     whole other half of the trie physically untouched. *)
  let m = of_list (List.init 128 (fun i -> (i, i))) in
  let low =
    M.inter (fun _ a _ -> a) m (of_list (List.init 64 (fun i -> (i, i))))
  in
  let m' = M.add 100 999 m in
  check_shared "lower half is shared" low (M.inter (fun _ a _ -> a) m' low);
  check_int "the new value is visible" 999 (M.find 100 m');
  check_int "the old map is untouched" 100 (M.find 100 m)

(* ------------------------------------------------------------------ *)
(* Sets                                                               *)
(* ------------------------------------------------------------------ *)

let set_basics =
  let@ () = register "set operations" in
  check_bool "empty is empty" true (S.is_empty S.empty);
  check_elements [] (elements S.empty);
  let s = S.singleton 3 in
  check_bool "singleton is not empty" false (S.is_empty s);
  check_bool "singleton mem" true (S.mem 3 s);
  check_bool "singleton mem other" false (S.mem 4 s);
  let s = set_of_list [ 3; 1; 2; 1 ] in
  check_elements [ 1; 2; 3 ] (elements s);
  check_bool "mem" true (S.mem 2 s);
  check_bool "mem absent" false (S.mem 4 s);
  check_elements [ 1; 2; 3; 4 ] (elements (S.union s (S.singleton 4)));
  check_elements [ 2; 3 ] (elements (S.inter s (set_of_list [ 2; 3; 5 ])));
  check_elements [] (elements (S.inter s S.empty));
  check_elements [ 1; 2; 3 ] (elements (S.union s S.empty));
  check_bool "equal" true (S.equal s (set_of_list [ 2; 3; 1 ]));
  check_bool "not equal" false (S.equal s (set_of_list [ 1; 2 ]));
  let seen = ref [] in
  S.iter (fun e -> seen := e :: !seen) s;
  check_elements [ 1; 2; 3 ] (List.sort compare !seen);
  check_shared "compact" s (S.compact s);
  check_shared "add of a present element" s (S.add 2 s)

(* ------------------------------------------------------------------ *)
(* Keys that are not integers                                         *)
(* ------------------------------------------------------------------ *)

module Var = struct
  type t = { id : int; name : string }

  let to_int v = v.id
  let pp ft v = Fmt.string ft v.name
end

module VM = PatriciaTree.MakeMap (Var)

let non_int_keys =
  let@ () = register "keys identified by to_int" in
  let x = Var.{ id = 1; name = "x" } in
  let y = Var.{ id = 2; name = "y" } in
  let m = VM.add y 20 (VM.add x 10 VM.empty) in
  check_opt "find x" (Some 10) (VM.find_opt x m);
  check_opt "find y" (Some 20) (VM.find_opt y m);
  (* Keys with the same [to_int] are interchangeable. *)
  let x' = Var.{ id = 1; name = "another name for x" } in
  check_opt "find an equivalent key" (Some 10) (VM.find_opt x' m);
  check_int "cardinal after rebinding with x'" 2 (VM.cardinal (VM.add x' 11 m));
  Alcotest.(check string)
    "pp uses Key.pp" "{x -> 10; y -> 20}"
    (Fmt.str "%a" (VM.pp Fmt.int) m)

(* ------------------------------------------------------------------ *)
(* Weak maps and sets                                                 *)
(* ------------------------------------------------------------------ *)

(* A boxed key, so that it can actually become unreachable. *)
module Boxed = struct
  type t = int ref

  let to_int r = !r
  let pp ft r = Fmt.int ft !r
end

module WM = PatriciaTree.MakeWeak (Boxed)
module WS = PatriciaTree.MakeWeakSet (Boxed)

let live_count = 50
let dead_count = 2000

let weak_map_behaves_like_a_map =
  let@ () = register "weak map: normal map behaviour" in
  let a = ref 1 and b = ref 2 and c = ref 3 in
  let m = List.fold_left (fun m k -> WM.add k !k m) WM.empty [ a; b; c ] in
  check_int "cardinal" 3 (WM.cardinal m);
  check_opt "find" (Some 2) (WM.find_opt b m);
  check_bool "mem" true (WM.mem c m);
  check_int "after remove" 2 (WM.cardinal (WM.remove b m));
  check_bool "equal" true
    (WM.equal Int.equal m (WM.add a 1 (WM.add c 3 (WM.singleton b 2))));
  check_shared "add of the same value" m (WM.add a 1 m);
  (* the keys are still reachable through this list *)
  ignore (Sys.opaque_identity [ a; b; c ])

let weak_map_collects =
  let@ () = register "weak map: unreachable keys are collected" in
  let live = List.init live_count (fun i -> ref (i * 2)) in
  let m = ref WM.empty in
  List.iter (fun k -> m := WM.add k !k !m) live;
  for i = 1 to dead_count do
    m := WM.add (ref (1_000_000 + i)) i !m
  done;
  Gc.full_major ();
  (* Live bindings survive. *)
  List.iter
    (fun k -> check_opt (Fmt.str "find %d" !k) (Some !k) (WM.find_opt k !m))
    live;
  (* The unreachable ones are gone; a handful may still be pinned by the stack,
     so this only checks that the bulk of them was reclaimed. *)
  check_bool "unreachable bindings were dropped" true
    (WM.cardinal !m < live_count + (dead_count / 10));
  (* [compact] keeps exactly the live bindings, and traversals only see them. *)
  let c = WM.compact !m in
  check_int "compact preserves the live bindings" (WM.cardinal !m)
    (WM.cardinal c);
  check_int "fold only sees live bindings" (WM.cardinal c)
    (WM.fold (fun _ _ n -> n + 1) c 0);
  List.iter
    (fun k ->
      check_opt (Fmt.str "find %d after compact" !k) (Some !k) (WM.find_opt k c))
    live;
  check_bool "equal to its compaction" true (WM.equal Int.equal !m c);
  check_shared "compact is idempotent" c (WM.compact c);
  ignore (Sys.opaque_identity live)

let weak_set_collects =
  let@ () = register "weak set: unreachable elements are collected" in
  let live = List.init live_count (fun i -> ref (i * 2)) in
  let s = ref WS.empty in
  List.iter (fun e -> s := WS.add e !s) live;
  for i = 1 to dead_count do
    s := WS.add (ref (1_000_000 + i)) !s
  done;
  Gc.full_major ();
  List.iter (fun e -> check_bool (Fmt.str "mem %d" !e) true (WS.mem e !s)) live;
  let c = WS.compact !s in
  check_int "compact keeps the live elements" live_count
    (List.length (WS.to_list c));
  check_bool "equal to its compaction" true (WS.equal !s c);
  ignore (Sys.opaque_identity live)

(* ------------------------------------------------------------------ *)
(* Randomised comparison against a reference implementation           *)
(* ------------------------------------------------------------------ *)

(* The reference is an association list with unique keys. *)
module Model = struct
  let add k v l = (k, v) :: List.remove_assoc k l
  let sorted l = List.sort compare l
  let union l r = List.fold_left (fun acc (k, v) -> add k v acc) r l |> sorted
  let inter l r = List.filter (fun (k, _) -> List.mem_assoc k r) l |> sorted
end

let random_key st =
  match Random.State.int st 8 with
  | 0 -> min_int
  | 1 -> max_int
  | 2 -> -1
  | 3 -> 0
  | 4 -> Random.State.int st 16
  | 5 -> -Random.State.int st 16
  | 6 -> Random.State.bits st
  | _ -> -Random.State.bits st

let random_map st =
  let l = ref [] in
  for _ = 1 to Random.State.int st 12 do
    l := Model.add (random_key st) (Random.State.int st 100) !l
  done;
  (of_list !l, Model.sorted !l)

(* The seed is fixed so that a failure is always reproducible. *)
let seed = [| 0x50a7; 0x1c1a; 0x7ee |]

let random_add_remove_update =
  let@ () = register "randomised: add, remove and update" in
  let st = Random.State.make seed in
  for _ = 1 to 300 do
    let m = ref M.empty and l = ref [] in
    for _ = 1 to 40 do
      let k = random_key st in
      let v = Random.State.int st 100 in
      match Random.State.int st 4 with
      | 0 | 1 ->
          m := M.add k v !m;
          l := Model.add k v !l
      | 2 ->
          m := M.remove k !m;
          l := List.remove_assoc k !l
      | _ ->
          (* insert if absent, delete if present *)
          m := M.update k (function None -> Some v | Some _ -> None) !m;
          l :=
            if List.mem_assoc k !l then List.remove_assoc k !l
            else Model.add k v !l
    done;
    check_bindings (Model.sorted !l) (bindings !m);
    check_int "cardinal" (List.length !l) (M.cardinal !m);
    List.iter (fun (k, v) -> check_opt "find_opt" (Some v) (M.find_opt k !m)) !l;
    check_bindings (Model.sorted !l)
      (List.sort compare (List.of_seq (M.to_seq !m)))
  done

let random_union_inter =
  let@ () = register "randomised: union and inter" in
  let st = Random.State.make seed in
  for _ = 1 to 2000 do
    let m1, l1 = random_map st in
    let m2, l2 = random_map st in
    check_bindings (Model.union l1 l2)
      (bindings (M.union (fun _ a _ -> a) m1 m2));
    check_bindings (Model.inter l1 l2)
      (bindings (M.inter (fun _ a _ -> a) m1 m2));
    check_bool "equal agrees with the model"
      (Model.sorted l1 = Model.sorted l2)
      (M.equal Int.equal m1 m2);
    (* union and inter are commutative when the values are taken consistently *)
    check_bindings
      (bindings (M.union (fun _ a _ -> a) m1 m2))
      (bindings (M.union (fun _ _ b -> b) m2 m1))
  done

let random_update_from =
  let@ () = register "randomised: update_from" in
  let st = Random.State.make seed in
  (* Rebind to the sum when both are bound, insert when only the guide is, and
     drop the binding when the two values agree. *)
  let f _ cur y =
    match cur with
    | None -> Some y
    | Some x -> if x = y then None else Some (x + y)
  in
  for _ = 1 to 2000 do
    let m, l = random_map st in
    let guide, lg = random_map st in
    let expected =
      List.fold_left
        (fun acc (k, y) ->
          match f k (List.assoc_opt k acc) y with
          | None -> List.remove_assoc k acc
          | Some v -> Model.add k v acc)
        l lg
    in
    check_bindings (Model.sorted expected) (bindings (M.update_from guide f m));
    (* a guide that returns the current binding everywhere is a no-op *)
    check_shared "no-op guide" m (M.update_from guide (fun _ cur _ -> cur) m)
  done
