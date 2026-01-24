open Typed
open T
open Rustsymex
open Charon
open Rust_val
open Sptr

module type S = sig
  module Sptr : Sptr.S

  type full_ptr := Sptr.t full_ptr
  type rust_val := Sptr.t rust_val

  (* state *)
  type t
  type serialized
  type 'a err
  type 'a ret := ('a * t, Error.t err * t, serialized) Result.t

  val add_to_call_trace :
    'a err -> Meta.span_data Soteria.Terminal.Call_trace.element -> 'a err

  val pp : t Fmt.t

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> t Fmt.t

  val empty : t
  val load : ?ignore_borrow:bool -> full_ptr -> Types.ty -> t -> rust_val ret
  val tb_load : full_ptr -> Types.ty -> t -> unit ret
  val load_discriminant : full_ptr -> Types.ty -> t -> Types.variant_id ret
  val store : full_ptr -> Types.ty -> rust_val -> t -> unit ret

  val alloc_untyped :
    ?kind:Alloc_kind.t ->
    ?span:Meta.span_data ->
    zeroed:bool ->
    size:sint Typed.t ->
    align:nonzero Typed.t ->
    t ->
    full_ptr ret

  val alloc_ty :
    ?kind:Alloc_kind.t -> ?span:Meta.span_data -> Types.ty -> t -> full_ptr ret

  val alloc_tys :
    ?kind:Alloc_kind.t ->
    ?span:Meta.span_data ->
    Types.ty list ->
    t ->
    full_ptr list ret

  val free : full_ptr -> t -> unit ret
  val fake_read : full_ptr -> Types.ty -> t -> (Error.t option * t) Rustsymex.t
  val check_ptr_align : full_ptr -> Types.ty -> t -> unit ret

  val copy_nonoverlapping :
    dst:full_ptr -> src:full_ptr -> size:sint Typed.t -> t -> unit ret

  val uninit : full_ptr -> Types.ty -> t -> unit ret
  val zeros : full_ptr -> sint Typed.t -> t -> unit ret

  val error :
    ([< Error.t ] as 'a) -> t -> ('ok, 'a err * t, serialized) Result.t

  val lift_err :
    t ->
    ('ok, ([< Error.t ] as 'err), 'f) Result.t ->
    ('ok, 'err err * t, 'f) Result.t

  val with_decay_map : 'a DecayMapMonad.t -> t -> ('a * t) Rustsymex.t

  val assert_ :
    sbool Typed.t ->
    ([< Error.t ] as 'a) ->
    t ->
    (unit, 'a err * t, serialized) Result.t

  val store_str_global : string -> full_ptr -> t -> unit ret
  val store_global : Types.global_decl_id -> full_ptr -> t -> unit ret
  val load_str_global : string -> t -> full_ptr option ret
  val load_global : Types.global_decl_id -> t -> full_ptr option ret

  val borrow :
    full_ptr -> Types.ty -> Expressions.borrow_kind -> t -> full_ptr ret

  val protect : full_ptr -> Types.ty -> Types.ref_kind -> t -> full_ptr ret
  val unprotect : full_ptr -> Types.ty -> t -> unit ret
  val with_exposed : [< sint ] Typed.t -> t -> full_ptr ret
  val leak_check : t -> unit ret
  val add_error : [< Error.t ] err -> t -> unit ret
  val pop_error : t -> ('a, Error.t err * t, serialized) Result.t

  val unwind_with :
    f:('a -> ('b, ([> Error.t ] as 'e) err * t, serialized) Result.t) ->
    fe:('e err * t -> ('b, 'e err * t, serialized) Result.t) ->
    ('a, 'e err * t, serialized) Result.t ->
    ('b, 'e err * t, serialized) Result.t

  val declare_fn : Types.fun_decl_ref -> t -> full_ptr ret
  val lookup_fn : full_ptr -> t -> Types.fun_decl_ref ret
  val register_thread_exit : (unit -> t -> unit ret) -> t -> unit ret
  val run_thread_exits : t -> unit ret
end
