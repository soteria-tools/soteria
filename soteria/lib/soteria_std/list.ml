(** Extensions to [Stdlib.List] with additional utility functions. *)

include Stdlib.List

(** [take_count n l] takes the prefix [l'] of at most [n] from the beginning of
    the list [l], or the entire list if the length of [l] is smaller than [n].
    It returns the pair [(k, l')], where [k] is the length of [l']. *)
let take_count n l =
  let taken = ref 0 in
  let[@tail_mod_cons] rec aux n l =
    match (n, l) with
    | 0, _ | _, [] -> []
    | n, x :: l ->
        incr taken;
        x :: aux (n - 1) l
  in
  if n < 0 then invalid_arg "List.take_count";
  let result = aux n l in
  (result, !taken)

(** [combine_opt l1 l2] combines two lists into a list of pairs, returning
    [Some] result if the lists have equal length, or [None] if they differ. *)
let combine_opt l1 l2 =
  try Some (combine l1 l2) with Invalid_argument _ -> None

(** [join_results outcomes] partitions a list of results into successful
    values and errors. Returns [Ok oks] if all results are successful,
    or [Error errors] if any errors are present. *)
let join_results outcomes =
  let oks, errors =
    partition_map
      (function Ok v -> Either.Left v | Error e -> Either.Right e)
      outcomes
  in
  if is_empty errors then Ok oks else Error errors

(** [combine3 l1 l2 l3] combines three lists into a list of triples.
    Raises [Invalid_argument] if the lists have different lengths. *)
let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
  | a :: l1, b :: l2, c :: l3 -> (a, b, c) :: combine3 l1 l2 l3
  | [], [], [] -> []
  | _ -> raise (Invalid_argument "combine3")

(** [combine3_opt l1 l2 l3] combines three lists into a list of triples,
    returning [Some] result if the lists have equal length, or [None] if
    they differ. *)
let combine3_opt l1 l2 l3 =
  try Some (combine3 l1 l2 l3) with Invalid_argument _ -> None

let[@tail_mod_cons] rec map2i i f l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | [ a1 ], [ b1 ] ->
      let r1 = f i a1 b1 in
      [ r1 ]
  | a1 :: a2 :: l1, b1 :: b2 :: l2 ->
      let r1 = f i a1 b1 in
      let r2 = f (i + 1) a2 b2 in
      r1 :: r2 :: map2i (i + 2) f l1 l2
  | _, _ -> invalid_arg "List_ex.map2i"

(** [map2i f l1 l2] maps function [f] over two lists simultaneously,
    passing the current index as the first argument to [f]. The function
    [f] receives [(index, element1, element2)] for each pair of elements.
    Raises [Invalid_argument] if the lists have different lengths. *)
let map2i f l1 l2 = map2i 0 f l1 l2

(** [map_changed f l] maps function [f] over list [l] and returns both
    the resulting list and a boolean indicating whether any element was
    changed (using physical equality [!=]). *)
let map_changed f l =
  let changed = ref false in
  let res =
    map
      (fun x ->
        let r = f x in
        if r != x then changed := true;
        r)
      l
  in
  (res, !changed)

(** [last l] returns the last element of list [l].
    Raises [Invalid_argument] if the list is empty. *)
let rec last l =
  match l with [] -> invalid_arg "List.last" | [ x ] -> x | _ :: l -> last l

(** [last_opt l] returns [Some] of the last element of list [l],
    or [None] if the list is empty. *)
let rec last_opt l =
  match l with [] -> None | [ x ] -> Some x | _ :: l -> last_opt l

(** [concat_map2 f l1 l2] applies [f] to each pair of elements from [l1]
    and [l2], concatenating all resulting lists. Raises [Invalid_argument]
    if the lists have different lengths. *)
let[@tail_mod_cons] rec concat_map2 f l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | x :: xs, y :: ys -> prepend_concat_map2 (f x y) f xs ys
  | _, _ -> invalid_arg "List_ex.concat_map2"

and[@tail_mod_cons] prepend_concat_map2 zs f xs ys =
  match zs with
  | [] -> concat_map2 f xs ys
  | z :: zs -> z :: prepend_concat_map2 zs f xs ys
