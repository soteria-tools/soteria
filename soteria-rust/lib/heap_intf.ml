open Charon_util
open Typed
open T
open Rustsymex
open Charon

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
    ?ignore_borrow:bool ->
    full_ptr ->
    Types.ty ->
    t ->
    ( Sptr.t rust_val * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree
      | `UBTransmute
      | `UBTreeBorrow
      | `MisalignedPointer
      | `RefToUninhabited ]
      err
      * t,
      serialized list )
    Result.t

  val tb_load :
    full_ptr ->
    Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference
      | `OutOfBounds
      | `UseAfterFree
      | `UBTreeBorrow
      | `MisalignedPointer ]
      err
      * t,
      serialized list )
    Result.t

  val store :
    full_ptr ->
    Types.ty ->
    Sptr.t rust_val ->
    t ->
    ( unit * t,
      [> `NullDereference
      | `OutOfBounds
      | `UBTreeBorrow
      | `UseAfterFree
      | `MisalignedPointer ]
      err
      * t,
      serialized list )
    Result.t

  val alloc_ty :
    Types.ty -> t -> (full_ptr * t, [> ] err * t, serialized list) Result.t

  val alloc_tys :
    Types.ty list ->
    t ->
    (full_ptr list * t, [> ] err * t, serialized list) Result.t

  val free :
    full_ptr ->
    t ->
    ( unit * t,
      [> `InvalidFree | `UseAfterFree ] err * t,
      serialized list )
    Result.t

  val is_valid_ptr : t -> full_ptr -> Types.ty -> bool Rustsymex.t

  val check_ptr_align :
    Sptr.t ->
    Types.ty ->
    t ->
    (unit, [> `MisalignedPointer ] err * t, serialized list) Result.t

  val copy_nonoverlapping :
    dst:full_ptr ->
    src:full_ptr ->
    size:sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err * t,
      serialized list )
    Result.t

  val uninit :
    full_ptr ->
    Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err * t,
      serialized list )
    Result.t

  val zeros :
    full_ptr ->
    sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree ] err * t,
      serialized list )
    Result.t

  val error : 'a -> t -> ('ok, 'a err * t, serialized list) Result.t

  val lift_err :
    t -> ('ok, 'err, 'f) Result.t -> ('ok, 'err err * t, 'f) Result.t

  val raw_err : 'a err -> 'a

  val store_str_global :
    string ->
    full_ptr ->
    t ->
    (unit * t, [> ] err * t, serialized list) Result.t

  val store_global :
    Types.global_decl_id ->
    full_ptr ->
    t ->
    (unit * t, [> ] err * t, serialized list) Result.t

  val load_str_global :
    string -> t -> (full_ptr option * t, [> ] err * t, serialized list) Result.t

  val load_global :
    Types.global_decl_id ->
    t ->
    (full_ptr option * t, [> ] err * t, serialized list) Result.t

  val borrow :
    full_ptr ->
    Charon.Types.ty ->
    Expressions.borrow_kind ->
    t ->
    ( full_ptr * t,
      [> `NullDereference | `UseAfterFree ] err * t,
      serialized list )
    Result.t

  val protect :
    full_ptr ->
    Charon.Types.ty ->
    Charon.Types.ref_kind ->
    t ->
    ( full_ptr * t,
      [> `NullDereference | `UseAfterFree | `OutOfBounds | `UBTreeBorrow ] err
      * t,
      serialized list )
    Result.t

  val unprotect :
    full_ptr ->
    Charon.Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference | `UseAfterFree | `OutOfBounds ] err * t,
      serialized list )
    Result.t

  val leak_check :
    t -> (unit * t, [> `MemoryLeak ] err * t, serialized list) Result.t

  val add_error :
    [< Error.t ] err -> t -> (unit * t, [> ] err * t, serialized list) Result.t

  val pop_error : t -> ('a, Error.t err * t, serialized list) Result.t
end
