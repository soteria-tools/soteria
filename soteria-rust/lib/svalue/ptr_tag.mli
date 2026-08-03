type t [@@deriving show, eq, ord, hash]

val fresh_tag : unit -> t
val zero : t

module type TagSet = sig
  type tag := t
  type t

  val singleton : tag -> t
  val pp : t Fmt.t
  val copy_and_add : t -> tag -> t
  val mem : t -> tag -> bool
end

module StrongMap : PatriciaTree.Map with type key = t
module StrongSet : TagSet
module WeakMap : PatriciaTree.Map with type key = t
module WeakSet : TagSet
