open Typed
open T
module Agv = Aggregate_val
module Compo_res = Soteria.Symex.Compo_res

type syn = State.syn =
  | Ser_heap of (Expr.t * Block.syn)
  | Ser_globs of (Symbol_std.t * Expr.t)
[@@deriving show { with_path = false }]

module type S = sig
  include Soteria.Sym_states.Base.M(Csymex).S with type syn = syn

  type 'a res := ('a, Error.with_trace, syn list) SM.Result.t

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> Format.formatter -> t -> unit

  val empty : t option
  val load : [< sptr ] Typed.t -> Ctree_block.Ctype.ctype -> Agv.t res
  val store : sptr Typed.t -> Ctree_block.Ctype.ctype -> Agv.t -> unit res
  val zero_range : [< sptr ] Typed.t -> sint Typed.t -> unit res
  val alloc : ?zeroed:bool -> sint Typed.t -> [> sptr ] Typed.t res
  val alloc_ty : Ctree_block.Ctype.ctype -> [> sptr ] Typed.t res
  val free : [< sptr ] Typed.t -> unit res

  val get_global :
    Cerb_frontend.Symbol.sym ->
    t option ->
    ([> sptr ] Typed.t * t option) Csymex.t

  val copy_nonoverlapping :
    dst:[< sptr ] Typed.t ->
    src:[< sptr ] Typed.t ->
    size:sint Typed.t ->
    unit res

  val produce_aggregate :
    Expr.t ->
    Cerb_frontend.Ctype.ctype ->
    Aggregate_val.syn ->
    t option ->
    t option Csymex.Producer.t
end
