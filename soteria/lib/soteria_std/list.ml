(** Extensions to [Stdlib.List] with utility functions. *)

include Stdlib.List

(** Take a prefix of a list of at most [n] elements, or the entire list if the
    list is shorter than [n].

    @return
      [(prefix, count)] where [prefix] is the prefix of the list and [count] is
      the number of elements taken.
    @raise Invalid_argument if [n] is negative. *)
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

(** Combine two lists into a list of pairs. *)
let combine_opt l1 l2 =
  try Some (combine l1 l2) with Invalid_argument _ -> None

(** Aggregates results, returning all errors if any, otherwise all successful
    values. *)
let join_results outcomes =
  let oks, errors =
    partition_map
      (function Ok v -> Either.Left v | Error e -> Either.Right e)
      outcomes
  in
  if is_empty errors then Ok oks else Error errors

(** Combine three lists into a list of triples. *)
let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
  | a :: l1, b :: l2, c :: l3 -> (a, b, c) :: combine3 l1 l2 l3
  | [], [], [] -> []
  | _ -> raise (Invalid_argument "combine3")

(** Combine three lists into a list of triples. *)
let combine3_opt l1 l2 l3 =
  try Some (combine3 l1 l2 l3) with Invalid_argument _ -> None

let rec combinei i l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | a1 :: l1, a2 :: l2 -> (i, a1, a2) :: combinei (i + 1) l1 l2
  | _, _ -> invalid_arg "List.combinei"

let combinei l1 l2 = combinei 0 l1 l2

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

(** Map a function over two lists simultaneously, passing the current index as
    the first argument to the function. The function [f] receives arguments
    [index element1 element2].

    @return the resulting list.
    @raise Invalid_argument if the lists have different lengths. *)
let map2i f l1 l2 = map2i 0 f l1 l2

(** Map a function over a list and return both the resulting list and a boolean
    indicating whether any element was changed (using physical equality [!=]).

    @return
      [(res, changed)] where [res] is the resulting list and [changed] is a
      boolean indicating whether any element was changed. *)
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

(** Return the last element of a list. *)
let rec last l =
  match l with [] -> invalid_arg "List.last" | [ x ] -> x | _ :: l -> last l

(** Return the last element of a list, if any. *)
let rec last_opt l =
  match l with [] -> None | [ x ] -> Some x | _ :: l -> last_opt l

(** Apply a function to each pair of elements from two lists, concatenating all
    resulting lists.

    @return the resulting list.
    @raise Invalid_argument if the lists have different lengths. *)
let[@tail_mod_cons] rec concat_map2 f l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | x :: xs, y :: ys -> prepend_concat_map2 (f x y) f xs ys
  | _, _ -> invalid_arg "List_ex.concat_map2"

and[@tail_mod_cons] prepend_concat_map2 zs f xs ys =
  match zs with
  | [] -> concat_map2 f xs ys
  | z :: zs -> z :: prepend_concat_map2 zs f xs ys

(** [sub ~from ~len l] returns the sublist of [l], from index [from]
    (inclusive), of length [n]. *)
let sub ~from ~len l = l |> drop from |> take len

(** Like {!find_opt} but returns both the found value, and the rest of the list.
*)
let rec find_with_rest f l =
  match l with
  | [] -> None
  | x :: xs -> (
      if f x then Some (x, xs)
      else
        match find_with_rest f xs with
        | None -> None
        | Some (found, rest) -> Some (found, x :: rest))
