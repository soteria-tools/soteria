module type MapS = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val to_seq : 'a t -> (key * 'a) Seq.t
end

module Key (Symex : Symex.Base) = struct
  module type S = sig
    include Stdlib.Map.OrderedType

    val pp : Format.formatter -> t -> unit
    val sem_eq : t -> t -> Symex.Value.(sbool t)
    val simplify : t -> t Symex.t
    val distinct : t list -> Symex.Value.(sbool t)
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
    type 'a t

    val empty : 'a t
    val is_empty : 'a t -> bool
    val syntactic_bindings : 'a t -> (Key.t * 'a) Seq.t
    val syntactic_mem : Key.t -> 'a t -> bool
    val syntactic_add : Key.t -> 'a -> 'a t -> 'a t
    val syntactic_add_opt : Key.t -> 'a option -> 'a t -> 'a t

    val pp' :
      ?key:(Format.formatter -> Key.t -> unit) ->
      ?ignore:(Key.t * 'a -> bool) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit

    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

    val show : ('a -> string) -> 'a t -> string
    val find_opt : Key.t -> 'a t -> (Key.t * 'a option) Symex.t

    val fold :
      ('acc -> Key.t * 'a -> 'acc Symex.t) ->
      'acc ->
      'a t option ->
      'acc Symex.t

    val fold_res :
      ('acc -> Key.t * 'a -> ('acc, 'e, 'f) Symex.Result.t) ->
      'acc ->
      'a t option ->
      ('acc, 'e, 'f) Symex.Result.t
  end
end
