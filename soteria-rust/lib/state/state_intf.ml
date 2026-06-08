open Svalue
open Typed
open T
open Charon
open Common

module type S = sig
  (* state *)
  include Soteria.Sym_states.Base.M(Rustsymex).S

  type 'a ret := ('a, Error.with_trace, syn list) SM.Result.t

  module Sptr : sig
    include module type of Sptr

    (** [offset ?check ?ty ~signed ptr off] Offsets [ptr] by the size of [ty] *
        [off], interpreting [off] as a [signed] integer. [ty] defaults to u8.
        May result in a dangling pointer error if the pointer goes over the
        allocation limit. This check can be disabled with [~check:false]. *)
    val offset :
      ?check:bool ->
      ?ty:Charon.Types.ty ->
      signed:bool ->
      [< sint ] Typed.t ->
      t ->
      t ret

    (** Checks this pointer isn't dangling for the given type, i.e. it points to
        an allocation and doesn't range outside of it. This also checks the
        allocation is live. *)
    val check_non_dangling : Typed.([< T.sptr_f ] t) -> Types.ty -> unit ret

    (** Same as {!check_non_dangling}, but for untyped ranges, where only a size
        is known. The size is signed: if less than 0, the range preceding the
        pointer is checked. *)
    val check_non_dangling_untyped :
      Typed.([< T.sptr_f ] t) -> Typed.(T.sint t) -> unit ret

    (** Checks this pointer is sufficiently aligned for the given type. This
        takes into account the metadata: for a [&dyn Trait], it will access the
        VTable to check alignment. *)
    val check_aligned : Typed.([< T.sptr_f ] t) -> Types.ty -> unit ret
  end

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> t Fmt.t

  val empty : t option

  val load :
    ?ignore_borrow:bool ->
    Typed.([< T.sptr_f ] t) ->
    Types.ty ->
    [> T.any ] Typed.t ret

  val tb_load : Typed.([< T.sptr_f ] t) -> Types.ty -> unit ret

  val load_discriminant :
    Typed.([< T.sptr_f ] t) -> Types.ty -> Types.variant_id ret

  val store :
    Typed.([< T.sptr_f ] t) -> Types.ty -> [< T.any ] Typed.t -> unit ret

  val alloc_untyped :
    ?span:Meta.span_data ->
    zeroed:bool ->
    size:sint Typed.t ->
    align:nonzero Typed.t ->
    Typed.([> T.sptr_f ] t) ret

  val alloc_ty : ?span:Meta.span_data -> Types.ty -> Typed.([> T.sptr_f ] t) ret

  val alloc_tys :
    ?span:Meta.span_data -> Types.ty list -> Typed.([> T.sptr_f ] t) list ret

  val free : Typed.([< T.sptr_f ] t) -> unit ret

  val size_and_align_of_val :
    Types.ty ->
    T.ptr_meta Typed.t option ->
    (T.sint Typed.t * T.nonzero Typed.t) ret

  val fake_read : Typed.([< T.sptr_f ] t) -> Types.ty -> unit ret

  val copy_nonoverlapping :
    src:Typed.([< T.sptr_f ] t) ->
    dst:Typed.([< T.sptr_f ] t) ->
    size:sint Typed.t ->
    unit ret

  val transmute :
    from:Types.ty ->
    to_:Types.ty ->
    [< T.any ] Typed.t ->
    [> T.any ] Typed.t ret

  val uninit : Typed.([< T.sptr_f ] t) -> Types.ty -> unit ret
  val zeros : Typed.([< T.sptr_f ] t) -> sint Typed.t -> unit ret
  val with_pointers_sym : 'a Sptr.DecayMap.SM.t -> 'a SM.t
  val store_str_global : string -> Typed.([< T.sptr_f ] t) -> unit ret
  val store_global : Types.global_decl_id -> Typed.([< T.sptr_f ] t) -> unit ret
  val load_str_global : string -> Typed.([> T.sptr_f ] t) option ret
  val load_global : Types.global_decl_id -> Typed.([> T.sptr_f ] t) option ret

  val borrow :
    ?protect:bool ->
    Typed.([< T.sptr_f ] t) ->
    Types.ty ->
    Typed.([> T.sptr_f ] t) ret

  val unprotect : Typed.([< T.sptr_f ] t) -> Types.ty -> unit ret
  val with_exposed : [< sint ] Typed.t -> Typed.([> T.sptr_f ] t) ret
  val leak_check : unit -> unit ret
  val add_error : Error.with_trace -> unit ret
  val pop_error : unit -> 'a ret
  val declare_fn : Fun_kind.t -> Typed.([> T.sptr_f ] t) ret
  val lookup_fn : Typed.([< T.sptr_f ] t) -> Fun_kind.t ret

  val lookup_const_generic :
    Types.const_generic_var_id -> Types.ty -> [< T.any ] Typed.t ret

  val register_thread_exit : (unit -> unit ret) -> unit ret
  val run_thread_exits : unit -> unit ret
end
