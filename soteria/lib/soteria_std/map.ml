(** Extensions to [Stdlib.Map] with pretty-printing support. *)

include Stdlib.Map

module MakePp (Key : Ordered_type.S) = struct
  include Make (Key)

  let pp pp_v ft m =
    let pp_pair = Fmt.pair ~sep:(Fmt.any " -> ") Key.pp pp_v in
    Fmt.iter_bindings ~sep:Fmt.cut iter pp_pair ft m
end
