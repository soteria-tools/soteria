open Charon_util
open Typed
open T

module type S = sig
  module Sptr : Sptr.S

  type full_ptr := Sptr.t full_ptr

  (* state *)
  type t
  type serialized
  type 'a err

  val add_to_call_trace : 'a err -> Call_trace.element -> 'a err
  val pp : t Fmt.t

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> t Fmt.t

  val empty : t

  val load :
    ?is_move:bool ->
    full_ptr ->
    Charon.Types.ty ->
    t ->
    ( Sptr.t rust_val * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree
      | `UBTransmute
      | `UBTreeBorrow ]
      err,
      serialized list )
    Rustsymex.Result.t

  val store :
    full_ptr ->
    Charon.Types.ty ->
    Sptr.t rust_val ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UBTreeBorrow | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val alloc :
    sint Typed.t ->
    t ->
    (full_ptr * t, [> ] err, serialized list) Rustsymex.Result.t

  val alloc_ty :
    Charon.Types.ty ->
    t ->
    (full_ptr * t, [> ] err, serialized list) Rustsymex.Result.t

  val free :
    full_ptr ->
    t ->
    ( unit * t,
      [> `InvalidFree | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val uninit :
    full_ptr ->
    Charon.Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err,
      serialized list )
    Rustsymex.Result.t

  val error : 'a -> t -> ('ok, 'a err, serialized list) Rustsymex.Result.t

  val store_str_global :
    string ->
    full_ptr ->
    t ->
    (unit * t, [> ] err, serialized list) Rustsymex.Result.t

  val store_global :
    Charon.Types.global_decl_id ->
    full_ptr ->
    t ->
    (unit * t, [> ] err, serialized list) Rustsymex.Result.t

  val load_str_global :
    string ->
    t ->
    (full_ptr option * t, [> ] err, serialized list) Rustsymex.Result.t

  val load_global :
    Charon.Types.global_decl_id ->
    t ->
    (full_ptr option * t, [> ] err, serialized list) Rustsymex.Result.t
end
