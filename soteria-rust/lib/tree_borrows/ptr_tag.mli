type t [@@deriving show, eq]

val fresh_tag : unit -> t
val zero : t

module WeakMap : PatriciaTree.MAP with type key = t

module WeakSet : sig
  include Weak.S with type data = t
  include Sigs.Printable with type t := t
end
