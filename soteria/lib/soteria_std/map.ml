(** Extensions to [Stdlib.Map] with pretty-printing support. *)

include Stdlib.Map

module MakePp (Key : Ordered_type.S) = struct
  include Make (Key)

  (** Adds an element to the map, but throws [Invalid_argument] if the key
      already exists. *)
  let add_assert_new key value map =
    update key
      (function
        | None -> Some value
        | Some _ ->
            raise (Invalid_argument "Map.add_assert_new: key already exists"))
      map

  let pp pp_v ft m =
    let pp_pair = Fmt.pair ~sep:(Fmt.any " -> ") Key.pp pp_v in
    Fmt.vbox ~indent:0 (Fmt.iter_bindings ~sep:Fmt.cut iter pp_pair) ft m
end
