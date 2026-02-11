open Typed
open T
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

  module SM :
    Soteria.Sym_states.State_monad.S
      with type 'a Symex.t = 'a Rustsymex.t
       and type st = t option
       and module Symex.Value = Rustsymex.Value
       and module Value = Rustsymex.Value

  type 'a ret := ('a, Error.with_trace, serialized list) SM.Result.t

  val pp : t Fmt.t

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> t Fmt.t

  val empty : t option
  val load : ?ignore_borrow:bool -> full_ptr -> Types.ty -> rust_val ret
  val tb_load : full_ptr -> Types.ty -> unit ret
  val load_discriminant : full_ptr -> Types.ty -> Types.variant_id ret
  val store : full_ptr -> Types.ty -> rust_val -> unit ret

  val alloc_untyped :
    ?kind:Alloc_kind.t ->
    ?span:Meta.span_data ->
    zeroed:bool ->
    size:sint Typed.t ->
    align:nonzero Typed.t ->
    full_ptr ret

  val alloc_ty :
    ?kind:Alloc_kind.t -> ?span:Meta.span_data -> Types.ty -> full_ptr ret

  val alloc_tys :
    ?kind:Alloc_kind.t ->
    ?span:Meta.span_data ->
    Types.ty list ->
    full_ptr list ret

  val free : full_ptr -> unit ret
  val fake_read : full_ptr -> Types.ty -> Error.t option SM.t
  val check_ptr_align : full_ptr -> Types.ty -> unit ret

  val copy_nonoverlapping :
    dst:full_ptr -> src:full_ptr -> size:sint Typed.t -> unit ret

  val uninit : full_ptr -> Types.ty -> unit ret
  val zeros : full_ptr -> sint Typed.t -> unit ret
  val with_decay_map : 'a DecayMapMonad.t -> 'a SM.t
  val store_str_global : string -> full_ptr -> unit ret
  val store_global : Types.global_decl_id -> full_ptr -> unit ret
  val load_str_global : string -> full_ptr option ret
  val load_global : Types.global_decl_id -> full_ptr option ret

  val borrow :
    ?protect:bool ->
    full_ptr ->
    Types.ty ->
    Expressions.borrow_kind ->
    full_ptr ret

  val unprotect : full_ptr -> Types.ty -> unit ret
  val with_exposed : [< sint ] Typed.t -> full_ptr ret
  val leak_check : unit -> unit ret
  val add_error : Error.with_trace -> unit ret
  val pop_error : unit -> 'a ret
  val declare_fn : Fun_kind.t -> full_ptr ret
  val lookup_fn : full_ptr -> Fun_kind.t ret

  val lookup_const_generic :
    Types.const_generic_var_id -> Types.ty -> rust_val ret

  val register_thread_exit : (unit -> unit ret) -> unit ret
  val run_thread_exits : unit -> unit ret
end
