open Aux

module type S = sig
  type t [@@deriving show]
  type serialized

  module SM :
    Soteria.Sym_states.State_monad.S
      with type 'a Symex.t = 'a Symex.t
       and type st = t option
       and module Symex.Value = Symex.Value
       and module Value = Symex.Value

  type 'a res := ('a, Error.t, serialized list) SM.Result.t

  val load : S_int.t -> S_val.t res
  val store : S_int.t -> S_val.t -> unit res
  val alloc : unit -> S_int.t res
  val free : S_int.t -> unit res
end
