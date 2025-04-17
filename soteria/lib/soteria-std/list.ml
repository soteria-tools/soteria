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

let%test "take_count" =
  let l = [ 1; 2; 3; 4; 5 ] in
  take_count 3 l = ([ 1; 2; 3 ], 3)
  && take_count 0 l = ([], 0)
  && take_count 5 l = (l, 5)
  && take_count 6 l = (l, 5)
  && take_count 10 l = (l, 5)
