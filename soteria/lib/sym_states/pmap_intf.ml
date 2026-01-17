open Symex

module Key (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    type t
    type syn [@@deriving show { with_path = false }]

    include Stdlib.Map.OrderedType with type t := t

    type sbool_v := Symex.Value.sbool Symex.Value.t

    val pp : Format.formatter -> t -> unit
    val sem_eq : t -> t -> sbool_v
    val fresh : unit -> t Symex.t
    val simplify : t -> t Symex.t
    val distinct : t list -> sbool_v
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
    val to_syn : t -> syn
    val subst : (Value.Syn.t -> 'a Value.t) -> syn -> t
    val exprs_syn : syn -> Symex.Value.Syn.t list
  end

  module type S_patricia_tree = sig
    include S

    val to_int : t -> int
  end
end

module M (Symex : Symex.Base) = struct
  open Symex

  module type S = sig
    type key
    type key_syn
    type 'a t
    type 'a syn = key_syn * 'a

    val empty : 'a t
    val syntactic_bindings : 'a t -> (key * 'a) Seq.t
    val syntactic_mem : key -> 'a t -> bool

    val ins_outs :
      ('a -> Value.Syn.t list * Value.Syn.t list) ->
      'a syn ->
      Value.Syn.t list * Value.Syn.t list

    val pp :
      ?ignore:(key * 'a -> bool) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit

    val pp_syn :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a syn -> unit

    val to_syn : ('a -> 'syn list) -> 'a t -> 'syn syn list
    val of_opt : 'a t option -> 'a t
    val to_opt : 'a t -> 'a t option

    val alloc :
      new_codom:'a ->
      'a t option ->
      (key * 'a t option, 'err, 'fix list) Symex.Result.t

    val allocs :
      fn:('b -> key -> ('a * 'k) Symex.t) ->
      els:'b list ->
      'a t option ->
      ('k list * 'a t option, 'err, 'fix list) Symex.Result.t

    val wrap :
      ('a option -> ('b * 'a option, 'err, 'fix list) Symex.Result.t) ->
      key ->
      'a t option ->
      ('b * 'a t option, 'err, 'fix syn list) Symex.Result.t

    val fold :
      ('acc -> key * 'a -> ('acc, 'err, 'fix) Symex.Result.t) ->
      'acc ->
      'a t option ->
      ('acc, 'err, 'fix) Symex.Result.t

    val produce :
      ('inner_syn -> 'inner_st option -> 'inner_st option Producer.t) ->
      'inner_syn syn ->
      'inner_st t option ->
      'inner_st t option Producer.t

    val consume :
      ('inner_syn ->
      'inner_st option ->
      ('inner_st option, 'inner_syn list) Consumer.t) ->
      'inner_syn syn ->
      'inner_st t option ->
      ('inner_st t option, 'inner_syn syn list) Consumer.t
  end
end
