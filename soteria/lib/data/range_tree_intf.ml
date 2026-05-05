module type Info_S = sig
  type t

  val make : height:int -> is_balanced:bool -> unit -> t

  (** Height of the tree (maximum depth) *)
  val height : t -> int

  (** [is_balanced] says whether we know {e for sure} that the tree is balanced.
  *)
  val is_balanced : t -> bool

  (** Information associated with a leaf node (height = 0; balanced = true) *)
  val leaf : t
end
