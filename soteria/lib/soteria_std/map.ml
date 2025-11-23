(** A wrapper around [Stdlib.Map] that includes a pretty-printer. 

    This module extends the standard map functor with a [pp] function 
    compatible with the [Fmt] library. *)

include Stdlib.Map

module MakePp (Key : Ordered_type.S) = struct
  include Make (Key)

  let pp pp_v ft m =
    let pp_pair = Fmt.pair ~sep:(Fmt.any " -> ") Key.pp pp_v in
    Fmt.iter_bindings ~sep:Fmt.cut iter pp_pair ft m
end
