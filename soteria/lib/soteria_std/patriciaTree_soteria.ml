include PatriciaTree

module type S = SET

module type SET = sig
  include PatriciaTree.SET
  include Sigs.Printable with type t := t
end

module MakeMap (Key : [%mixins PatriciaTree.KEY + Sigs.Printable]) = struct
  include PatriciaTree.MakeMap (Key)

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

module MakeSet (Key : [%mixins PatriciaTree.KEY + Sigs.Printable]) = struct
  include PatriciaTree.MakeSet (Key)

  let pp = Fmt.braces @@ Fmt.iter ~sep:Fmt.comma iter Key.pp
end
