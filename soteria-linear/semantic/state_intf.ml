open Aux

module type S = sig
  type t [@@deriving show]
  type err
  type syn = Excl_val.syn Freeable.syn PMap.syn [@@deriving show]
  type 'a state_res := t option -> ('a * t option, err, syn list) Symex.Result.t

  val load : S_int.t -> S_val.t state_res
  val store : S_int.t -> S_val.t -> unit state_res
  val alloc : S_int.t state_res
  val free : S_int.t -> unit state_res
  val error : string -> t option -> err
  val produce : syn -> t option -> t option Symex.Producer.t
  val consume : syn -> t option -> (t option, syn list) Symex.Consumer.t
end
