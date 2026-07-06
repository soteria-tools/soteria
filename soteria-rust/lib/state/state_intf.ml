open Svalue
open Typed
open T
open Charon
open Common

module type S = sig
  (* state *)
  include Soteria.Sym_states.Base.M(Rustsymex).S

  type 'a ret := ('a, Error.with_trace, syn list) SM.Result.t
  type 'a v := 'a Typed.t

  module Sptr : sig
    include module type of Sptr

    (** [offset ?check_signed ?ty ptr off] Offsets [ptr] by the size of [ty] *
        [off]. [ty] defaults to u8.

        If [check_signed] is [Some s], the offset is checked for overflows and
        dangling pointers. [s] represents whether the offset is signed or
        unsigned (this matters for overflow checks on the multiplication with
        the pointee type). *)
    val offset :
      ?check_signed:bool -> ?ty:Charon.Types.ty -> [< sint ] v -> t -> t ret

    (** Checks this pointer isn't dangling for the given pointee type, i.e. it
        points to an allocation and doesn't range outside of it. This also
        checks the allocation is live, and that the type is inhabited, *)
    val check_non_dangling : [< sptr_f ] v -> Types.ty -> unit ret

    (** Same as {!check_non_dangling}, but for untyped ranges, where only a size
        is known. The size is signed: if less than 0, the range preceding the
        pointer is checked. *)
    val check_non_dangling_untyped : [< sptr_t ] v -> sint v -> unit ret

    (** Checks this pointer is sufficiently aligned for the given type. This
        takes into account the metadata: for a [&dyn Trait], it will access the
        VTable to check alignment. *)
    val check_aligned : [< sptr_f ] v -> Types.ty -> unit ret
  end

  (** Prettier but expensive printing. *)
  val pp_pretty : ignore_freed:bool -> t Fmt.t

  val empty : t option
  val load : ?ignore_borrow:bool -> [< sptr_f ] v -> Types.ty -> [> any ] v ret
  val tb_load : [< sptr_f ] v -> Types.ty -> unit ret
  val load_discriminant : [< sptr_f ] v -> Types.ty -> [> sint ] v ret
  val store : [< sptr_f ] v -> Types.ty -> [< any ] v -> unit ret

  val alloc_untyped :
    ?span:Meta.span_data ->
    zeroed:bool ->
    size:sint Typed.t ->
    align:nonzero Typed.t ->
    [> sptr_f ] v ret

  val alloc_ty : ?span:Meta.span_data -> Types.ty -> [> sptr_f ] v ret
  val free : [< sptr_f ] v -> unit ret

  val size_and_align_of_val :
    Types.ty -> [< ptr_meta ] v option -> ([> sint ] v * [> nonzero ] v) ret

  val fake_read : [< sptr_f ] v -> Types.ty -> unit ret

  val copy_nonoverlapping :
    src:[< sptr_f ] v -> dst:[< sptr_f ] v -> size:sint Typed.t -> unit ret

  val transmute : from:Types.ty -> to_:Types.ty -> [< any ] v -> [> any ] v ret

  val transmute_raw :
    to_:Types.ty ->
    ([< any ] v * [< sint ] v * [< nonzero ] v) list ->
    [> any ] v ret

  val uninit : [< sptr_f ] v -> Types.ty -> unit ret
  val zeros : [< sptr_f ] v -> sint Typed.t -> unit ret
  val with_pointers_sym : 'a Sptr.DecayMap.SM.t -> 'a SM.t
  val store_str_global : string -> [< sptr_f ] v -> unit ret
  val store_global : Types.global_decl_id -> [< sptr_f ] v -> unit ret
  val load_str_global : string -> [> sptr_f ] v option ret
  val load_global : Types.global_decl_id -> [> sptr_f ] v option ret
  val borrow : ?protect:bool -> [< sptr_f ] v -> Types.ty -> [> sptr_f ] v ret
  val unprotect : [< sptr_f ] v -> Types.ty -> unit ret
  val with_exposed : [< sint ] Typed.t -> [> sptr_f ] v ret
  val leak_check : unit -> unit ret
  val add_error : Error.with_trace -> unit ret
  val pop_error : unit -> 'a ret
  val declare_fn : Fun_kind.t -> [> sptr_f ] v ret
  val lookup_fn : [< sptr_f ] v -> Fun_kind.t ret

  val lookup_const_generic :
    Types.const_generic_var_id -> Types.ty -> [> any ] v ret

  val register_thread_exit : (unit -> unit ret) -> unit ret
  val run_thread_exits : unit -> unit ret
end
