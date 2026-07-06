type t = Tag of int [@@ocaml.boxed]

let[@inline] equal (Tag t1) (Tag t2) = Int.equal t1 t2
let[@inline] compare (Tag t1) (Tag t2) = Int.compare t1 t2
let[@inline] hash (Tag tag) = tag
let pp fmt (Tag tag) = Fmt.pf fmt "‖%d‖" tag
let show = Fmt.to_to_string pp
let zero = Tag 0
let tag_counter = ref 0

let fresh_tag () =
  incr tag_counter;
  Tag !tag_counter

module Key = struct
  type nonrec t = t

  let[@inline] to_int (Tag tag) = tag
end

module MapNode =
  PatriciaTree.WeakNode
    (struct
      type 'k t = Key.t
    end)
    (PatriciaTree.WrappedHomogeneousValue)

module SetNode = PatriciaTree.WeakSetNode (struct
  type 'k t = Key.t
end)

module WeakMap = PatriciaTree.MakeCustomMap (Key) (PatriciaTree.Value) (MapNode)

module WeakSet = struct
  include Weak.Make (struct
    type nonrec t = t

    let[@inline] hash (Tag tag) = tag
    let equal = equal
  end)

  let pp = Fmt.iter ~sep:(Fmt.any ", ") iter pp
end
