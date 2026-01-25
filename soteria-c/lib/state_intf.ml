open Typed
open T
open Cerb_frontend
module Agv = Aggregate_val
module Compo_res = Soteria.Symex.Compo_res

type serialized =
  | Ser_heap of (sloc Typed.t * Block.serialized)
  | Ser_globs of (Symbol_std.t * sloc Typed.t)
[@@deriving show { with_path = false }]

module type S = sig
  type t
  type nonrec serialized = serialized

  type 'a res :=
    t option ->
    (('a, Error.with_trace, serialized list) Compo_res.t * t option) Csymex.t

  val pp : Format.formatter -> t -> unit
  val show : t -> string

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

  val error : Error.t -> 'ok res
  val produce : serialized -> t option -> (unit * t option) Csymex.t

  val produce_aggregate :
    sptr Typed.t ->
    Ctype.ctype ->
    Aggregate_val.t ->
    t option ->
    (unit * t option) Csymex.t

  (* val consume :
    serialized -> t -> (t, [> Csymex.lfail ] err, serialized) Csymex.Result.t *)
end
