include Stdlib.Array

(** [count f arr] counts the number of elements in [arr] satisfying the
    predicate [f]. *)
let count f arr = fold_left (fun acc x -> if f x then acc + 1 else acc) 0 arr
