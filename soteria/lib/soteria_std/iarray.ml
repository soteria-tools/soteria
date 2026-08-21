include Stdlib.Iarray

module Infix = struct
  let ( .%() ) = get
end

(* mirrors the stdlib's binding to the compiler primitive; unsafe! *)
external unsafe_of_array : 'a array -> 'a iarray = "%opaque"

let fold = fold_left

let copy_and_update f vs =
  let vs = to_array vs in
  f vs;
  unsafe_of_array vs

let copy_and_set idx x vs = copy_and_update (fun vs -> vs.(idx) <- x) vs

let map_changed f vs =
  let changed = ref false in
  let res =
    map
      (fun x ->
        let r = f x in
        if r != x then changed := true;
        r)
      vs
  in
  (res, !changed)

let hash_combine a b = (31 * a) + b
let hash hash_elt = fold_left (fun acc v -> hash_combine acc (hash_elt v)) 0
let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
