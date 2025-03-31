open Typed
open T

module Template = struct
  type ('a, 'b) t = { heap : 'a; globs : 'b }
  [@@deriving show { with_path = false }]
end

module type S = sig
  type t

  type serialized =
    ( (sloc Typed.t * Tree_block.serialized Csymex.Freeable.serialized) list,
      (Cerb_frontend.Symbol.sym * sloc Typed.t) list )
    Template.t

  type 'a err

  val add_to_call_trace : 'a err -> Call_trace.element -> 'a err
  val pp : Format.formatter -> t -> unit

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> Format.formatter -> t -> unit

  val empty : t

  val load :
    [< sptr ] Typed.t ->
    Tree_block.Ctype.ctype ->
    t ->
    ( cval Typed.t * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree ]
      err,
      serialized list )
    Csymex.Result.t

  val store :
    [< sptr ] Typed.t ->
    Tree_block.Ctype.ctype ->
    cval Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Csymex.Result.t

  val alloc :
    sint Typed.t ->
    t ->
    ([> sptr ] Typed.t * t, [> ] err, serialized list) Csymex.Result.t

  val alloc_ty :
    Tree_block.Ctype.ctype ->
    t ->
    ([> sptr ] Typed.t * t, [> ] err, serialized list) Csymex.Result.t

  val free :
    [< sptr ] Typed.t ->
    t ->
    ( unit * t,
      [> `InvalidFree | `UseAfterFree ] err,
      serialized list )
    Csymex.Result.t

  val copy_nonoverlapping :
    dst:[< sptr ] Typed.t ->
    src:[< sptr ] Typed.t ->
    size:sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Csymex.Result.t

  val error : 'a -> t -> ('ok * t, 'a err, serialized list) Csymex.Result.t
  val produce : serialized -> t -> t Csymex.t

  val consume :
    serialized -> t -> (t, [> ] err, serialized list) Csymex.Result.t
end
