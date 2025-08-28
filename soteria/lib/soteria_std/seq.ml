include Stdlib.Seq

(** An iteration over the cross-product of [seq] with itself, excluding pairs
    (x, x) of the same element. For instance, [self_cross_product [1; 2; 3]]
    will iterate over [(1, 2), (1, 3), (2, 3)] Note that the sequence needs to
    be persistent, otherwise result is unspecified. *)
let self_cross_product seq =
  let rec aux seq =
    match seq () with
    | Nil -> empty
    | Cons (x, seq) -> append (map (fun y -> (x, y)) seq) (aux seq)
  in
  aux seq

let init_z n f =
  let rec init_aux_z f i j () =
    if Z.lt i j then Cons (f i, init_aux_z f (Z.succ i) j) else Nil
  in
  if Z.lt n Z.zero then invalid_arg "Seq.init_z" else init_aux_z f Z.zero n
