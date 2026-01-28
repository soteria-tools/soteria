open Aux

type err = [ `Interp of string | `UseAfterFree ]

module type S = sig
  type t [@@deriving show]
  type serialized

  type 'a state_res :=
    t option ->
    (('a, err, serialized list) Soteria.Symex.Compo_res.t * t option) Symex.t

  val load : S_int.t -> S_val.t state_res
  val store : S_int.t -> S_val.t -> unit state_res
  val alloc : unit -> S_int.t state_res
  val free : S_int.t -> unit state_res
end
