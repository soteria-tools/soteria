(** Extensions to [Stdlib.Map] with pretty-printing support. *)

include Stdlib.Map

module MakePp (Key : Ordered_type.S) = struct
  include Make (Key)

  let add_if_not_exists key value map =
    update key
      (function
        | None -> Some value
        | Some _ ->
            raise (Invalid_argument "Map.add_if_not_exists: key already exists"))
      map

  let pp pp_v ft m =
    let pp_pair = Fmt.pair ~sep:(Fmt.any " -> ") Key.pp pp_v in
    Fmt.iter_bindings ~sep:Fmt.cut iter pp_pair ft m
end
