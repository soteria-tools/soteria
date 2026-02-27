open Aux

module type S = sig
  include Soteria.Sym_states.Base.M(Symex).S

  type 'a res := ('a, Error.t, syn list) SM.Result.t

  val load : S_int.t -> S_val.t res
  val store : S_int.t -> S_val.t -> unit res
  val alloc : unit -> S_int.t res
  val free : S_int.t -> unit res
end
