open Typed
open T
open Rustsymex
open Charon
open Rust_val

module type S = sig
  module Sptr : Sptr.S

  type full_ptr := Sptr.t full_ptr

  (* state *)
  type t
  type serialized
  type 'a err

  val add_to_call_trace :
    'a err -> Meta.span Soteria.Terminal.Call_trace.element -> 'a err

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
      | `UBTransmute of string
      | `AliasingError
      | `MisalignedPointer
      | `RefToUninhabited
      | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val tb_load :
    full_ptr ->
    Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference
      | `OutOfBounds
      | `UseAfterFree
      | `AliasingError
      | `MisalignedPointer
      | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val load_discriminant :
    full_ptr ->
    Types.ty ->
    t ->
    ( Types.variant_id * t,
      [> `NullDereference
      | `OutOfBounds
      | `UninitializedMemoryAccess
      | `UseAfterFree
      | `UBTransmute of string
      | `AliasingError
      | `MisalignedPointer
      | `RefToUninhabited
      | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val store :
    full_ptr ->
    Types.ty ->
    Sptr.t rust_val ->
    t ->
    ( unit * t,
      [> `NullDereference
      | `OutOfBounds
      | `AliasingError
      | `UseAfterFree
      | `MisalignedPointer
      | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val alloc_untyped :
    ?zeroed:bool ->
    size:sint Typed.t ->
    align:nonzero Typed.t ->
    t ->
    (full_ptr * t, [> ] err * t, serialized) Result.t

  val alloc_ty :
    Types.ty -> t -> (full_ptr * t, [> ] err * t, serialized) Result.t

  val alloc_tys :
    Types.ty list -> t -> (full_ptr list * t, [> ] err * t, serialized) Result.t

  val free :
    full_ptr ->
    t ->
    (unit * t, [> `InvalidFree | `UseAfterFree ] err * t, serialized) Result.t

  val is_valid_ptr : t -> full_ptr -> Types.ty -> bool Rustsymex.t

  val check_ptr_align :
    Sptr.t ->
    Types.ty ->
    t ->
    (unit * t, [> `MisalignedPointer ] err * t, serialized) Result.t

  val copy_nonoverlapping :
    dst:full_ptr ->
    src:full_ptr ->
    size:sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val uninit :
    full_ptr ->
    Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val zeros :
    full_ptr ->
    sint Typed.t ->
    t ->
    ( unit * t,
      [> `NullDereference | `OutOfBounds | `UseAfterFree | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val error :
    ([< Error.t ] as 'a) -> t -> ('ok, 'a err * t, serialized) Result.t

  val lift_err :
    t ->
    ('ok, ([< Error.t ] as 'err), 'f) Result.t ->
    ('ok, 'err err * t, 'f) Result.t

  val assert_ :
    sbool Typed.t ->
    ([< Error.t ] as 'a) ->
    t ->
    (unit, 'a err * t, serialized) Result.t

  val store_str_global :
    string -> full_ptr -> t -> (unit * t, [> ] err * t, serialized) Result.t

  val store_global :
    Types.global_decl_id ->
    full_ptr ->
    t ->
    (unit * t, [> ] err * t, serialized) Result.t

  val load_str_global :
    string -> t -> (full_ptr option * t, [> ] err * t, serialized) Result.t

  val load_global :
    Types.global_decl_id ->
    t ->
    (full_ptr option * t, [> ] err * t, serialized) Result.t

  val borrow :
    full_ptr ->
    Charon.Types.ty ->
    Expressions.borrow_kind ->
    t ->
    ( full_ptr * t,
      [> `NullDereference | `UseAfterFree | `UBDanglingPointer ] err * t,
      serialized )
    Result.t

  val protect :
    full_ptr ->
    Charon.Types.ty ->
    Charon.Types.ref_kind ->
    t ->
    ( full_ptr * t,
      [> `NullDereference
      | `UseAfterFree
      | `OutOfBounds
      | `AliasingError
      | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val unprotect :
    full_ptr ->
    Charon.Types.ty ->
    t ->
    ( unit * t,
      [> `NullDereference
      | `RefInvalidatedEarly
      | `OutOfBounds
      | `AliasingError
      | `UBDanglingPointer ]
      err
      * t,
      serialized )
    Result.t

  val leak_check :
    t -> (unit * t, [> `MemoryLeak ] err * t, serialized) Result.t

  val add_error :
    [< Error.t ] err -> t -> (unit * t, [> ] err * t, serialized) Result.t

  val pop_error : t -> ('a, Error.t err * t, serialized) Result.t

  val unwind_with :
    f:('a -> ('b, ([> Error.t ] as 'e) err * t, serialized) Result.t) ->
    fe:('e err * t -> ('b, 'e err * t, serialized) Result.t) ->
    ('a, 'e err * t, serialized) Result.t ->
    ('b, 'e err * t, serialized) Result.t

  val declare_fn :
    Charon.Types.fn_ptr ->
    t ->
    (full_ptr * t, [> ] err * t, serialized) Result.t

  val lookup_fn :
    full_ptr ->
    t ->
    ( Charon.Types.fn_ptr * t,
      [> `MisalignedFnPointer
      | `NotAFnPointer
      | `NullDereference
      | `UseAfterFree
      | `UBDanglingPointer ]
      err
      * t,
      serialized )
    SYMEX.Result.t
end
