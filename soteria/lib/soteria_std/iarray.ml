include Stdlib.Iarray

module Infix = struct
  let ( .%() ) = get
end

(* copied from stdlib, unsafe! *)
external unsafe_of_array : 'a array -> 'a iarray = "%opaque"

let fold = fold_left

let copy_and_update f vs =
  let vs = to_array vs in
  f vs;
  unsafe_of_array vs

let copy_and_set idx x vs = copy_and_update (fun vs -> vs.(idx) <- x) vs
let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
