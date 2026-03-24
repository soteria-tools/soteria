open Symex
open Data

module Key (Symex : Symex.Base) = struct
  module type S = sig
    include S_map.Key(Symex).S

    val fresh : unit -> t Symex.t
    val subst : (Var.t -> Var.t) -> t -> t
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end

  module type S_patricia_tree = sig
    include S

    val to_int : t -> int
  end
end

module M
    (Symex : Symex.Base)
    (Key : sig
      type t
    end) =
struct
  module type S = sig
    type codom
    type codom_serialized

    include Base.M(Symex).S with type serialized = Key.t * codom_serialized

    type ('a, 'err) res := ('a, 'err, serialized list) SM.Result.t

    type ('a, 'err) codom_res :=
      codom option ->
      (('a, 'err, codom_serialized list) Compo_res.t * codom option) Symex.t

    val empty : t
    val syntactic_bindings : t -> (Key.t * codom) Seq.t
    val syntactic_mem : Key.t -> t -> bool

    val pp' :
      ?codom:(Format.formatter -> codom -> unit) ->
      ?key:(Format.formatter -> Key.t -> unit) ->
      ?ignore:(Key.t * codom -> bool) ->
      Format.formatter ->
      t ->
      unit

    val of_opt : t option -> t
    val to_opt : t -> t option
    val alloc : new_codom:codom -> (Key.t, 'err) res

    val allocs :
      fn:('a -> Key.t -> ('k * codom) Symex.t) ->
      els:'a list ->
      ('k list, 'err) res

    val wrap : Key.t -> ('a, 'err) codom_res -> ('a, 'err) res

    val fold :
      ('acc -> Key.t * codom -> 'acc Symex.t) ->
      'acc ->
      t option ->
      'acc Symex.t
  end
end
