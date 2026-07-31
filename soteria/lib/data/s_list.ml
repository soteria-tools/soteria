(** Operations on lists of symbolic values. *)

(** Operations on {i semi-concrete} lists: lists whose structure (length and
    order) is concrete, but whose elements are symbolic. *)
module Semi_concrete (Symex : Symex.Base) = struct
  open Symex.Syntax

  (** [sort ~leq l] sorts the list [l] according to the symbolic comparison
      [leq], using an insertion sort; if comparisons are symbolic, this may
      branch. *)
  let sort ~leq l =
    let rec insert x = function
      | [] -> Symex.return [ x ]
      | y :: rest as l ->
          if%sat leq x y then Symex.return (x :: l)
          else
            let+ rest = insert x rest in
            y :: rest
    in
    let rec sort = function
      | [] -> Symex.return []
      | x :: rest ->
          let* rest = sort rest in
          insert x rest
    in
    sort l
end
