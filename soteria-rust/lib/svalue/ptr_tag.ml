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
  let pp = pp
end

module type TagMap = sig
  type tag := t
  type 'a t

  val update : tag -> ('a option -> 'a option) -> 'a t -> 'a t
end

module type TagSet = sig
  type tag := t
  type t

  val singleton : tag -> t
  val pp : t Fmt.t
  val copy_and_add : t -> tag -> t
  val mem : t -> tag -> bool
end

module StrongMap = PatriciaTree.MakeMap (Key)

module StrongSet = struct
  include PatriciaTree.MakeSet (Key)

  let copy_and_add set tag = add tag set
  let mem set tag = mem tag set
end

module WeakMap = PatriciaTree.MakeWeak (Key)

module WeakSet = struct
  include Weak.Make (struct
    type nonrec t = t

    let[@inline] hash (Tag tag) = tag
    let equal = equal
  end)

  let singleton tag =
    let s = create 1 in
    add s tag;
    s

  let copy_and_add set tag =
    let s = create (count set + 1) in
    add s tag;
    iter (add s) set;
    s

  let pp = Fmt.iter ~sep:(Fmt.any ", ") iter pp
end
