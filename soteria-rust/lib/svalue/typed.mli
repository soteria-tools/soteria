open Charon
include module type of Typed_core

(* T *)

module T : sig
  include module type of T

  type sptr_f = [ `FullPtr ]
  type sptr_t = [ `ThinPtr ]
  type adt = [ `Adt ]
  type ptr_meta = [ sptr_t | sint ]

  type any =
    [ sint_ovf
    | sfloat
    | sbool
    | sptr
    | sloc
    | any sseq
    | sptr_f
    | sptr_t
    | adt ]

  val pp_sptr_f : Format.formatter -> sptr_f -> unit
  val pp_sptr_t : Format.formatter -> sptr_t -> unit
  val pp_adt : Format.formatter -> adt -> unit
  val pp_any : Format.formatter -> any -> unit
end

(* types *)

val t_ptr : unit -> [> T.sptr ] ty
val t_loc : unit -> [> T.sloc ] ty
val t_float : Types.float_type -> T.sfloat ty
val t_usize : unit -> [> T.sint ] ty
val t_lit : Types.literal_type -> [> T.sint ] ty
val t_ptr_f : unit -> [> T.sptr_f ] ty
val t_ptr_t : unit -> [> T.sptr_t ] ty

(* casts *)

val cast_checked : ty:([< T.any ] as 'a) ty -> [< T.any ] t -> 'a t
val cast_float : [< T.any ] t -> [> T.sfloat ] t
val cast_i : Values.u_int_ty -> [< T.any ] t -> [> T.sint ] t
val cast_f : Types.float_type -> [< T.any ] t -> T.sfloat t
val cast_lit : Types.literal_type -> [< T.any ] t -> [> T.sint ] t
val cast_ptr : [< T.any ] t -> [< T.sptr ] t
val cast_ptr_f : [< T.any ] t -> [< T.sptr_f ] t
val cast_ptr_t : [< T.any ] t -> [< T.sptr_t ] t
val cast_adt : Types.type_decl_ref -> [< T.any ] t -> [> T.adt ] t
val cast_any_adt : [< T.any ] t -> [> T.adt ] t

(** Reinterprets an integer as known to be non-zero. The caller is responsible
    for ensuring the value is indeed non-zero (e.g. an alignment). *)
val cast_nonzero : [< T.sint ] t -> [> T.nonzero ] t

(** Widens any value to the open [[> T.any]] type. Unlike {!cast}, this is a
    checked coercion: it cannot change the underlying value's kind. *)
val as_any : [< T.any ] t -> [> T.any ] t

(* helpers *)

val float_precision : Values.float_type -> FloatPrecision.t

module BitVec : sig
  include module type of BitVec

  (* constructors *)

  val of_bool : sbool t -> [> T.sint ] t
  val of_literal : Values.literal -> [> T.sint ] t
  val of_constant_expr : Types.constant_expr -> [> T.sint ] t
  val of_scalar : Values.scalar_value -> [> T.sint ] t

  (* from Z/int *)

  val mk_lit : Types.literal_type -> Z.t -> [> T.sint ] t
  val mki_lit : Types.literal_type -> int -> [> T.sint ] t
  val u8i : int -> [> T.sint ] t
  val u32i : int -> [> T.sint ] t
  val u64i : int -> [> T.sint ] t
  val u128i : int -> [> T.sint ] t
  val usize : Z.t -> [> T.sint ] t
  val usizei : int -> [> T.sint ] t
  val usizeinz : int -> [> T.nonzero ] t

  (* helpers *)

  val min : signed:bool -> ([< T.sint ] as 'a) t -> 'a t -> 'a t
  val max : signed:bool -> ([< T.sint ] as 'a) t -> 'a t -> 'a t
  val sure_is_zero : [< T.sint ] t -> bool
end

module BV = BitVec

module Float : sig
  include module type of Float

  val mk : Types.float_type -> string -> [> T.sfloat ] t
end

module Ptr : sig
  include module type of Ptr

  (* redefined to avoid the size parameter *)
  val loc_of_int : int -> [> T.sloc ] t
  val null : unit -> [> T.sptr ] t
  val null_loc : unit -> [> T.sloc ] t

  (* lifted to sptr_t and accessors *)
  val mk_ptr_t :
    ptr:[< T.sptr ] t ->
    size:[< T.sint ] t ->
    align:[< T.nonzero ] t ->
    tag:Ptr_tag.t option ->
    [> T.sptr_t ] t

  val is_null' : [< T.sptr_t ] t -> [> T.sbool ] t
  val is_at_null_loc' : [< T.sptr_t ] t -> [> T.sbool ] t
  val decompose' : [< T.sptr_t ] t -> [> T.sloc ] t * [> T.sint ] t
  val loc' : [< T.sptr_t ] t -> [> T.sloc ] t
  val ofs' : [< T.sptr_t ] t -> [> T.sint ] t
  val add_ofs' : [< T.sptr_t ] t -> [< T.sint ] t -> [> T.sptr_t ] t
  val ptr_inner : [< T.sptr_t ] t -> [> T.sptr ] t
  val with_inner : [< T.sptr_t ] t -> [< T.sptr ] t -> [> T.sptr_t ] t
  val align_of : [< T.sptr_t ] t -> [> T.nonzero ] t
  val size_of : [< T.sptr_t ] t -> [> T.sint ] t
  val tag_of : [< T.sptr_t ] t -> Ptr_tag.t option
  val with_tag : [< T.sptr_t ] t -> Ptr_tag.t option -> [> T.sptr_t ] t

  (* full pointers *)
  val mk_ptr_f : [< T.sptr_t ] t -> [< T.ptr_meta ] t option -> [> T.sptr_f ] t
  val split : [< T.sptr_f ] t -> [< T.sptr_t ] t * [< T.ptr_meta ] t option
  val meta_of : [< T.sptr_f ] t -> [> T.ptr_meta ] t option
  val ptr_of : [< T.sptr_f ] t -> [> T.sptr_t ] t
end

module Adt : sig
  (** Creates a tuple ADT with the given value blocks. This may be an array,
      struct, or tuple. *)
  val mk_tuple : [< T.any ] t list -> [> T.adt ] t

  (** Creates an enum ADT with the given discriminant and values. *)
  val mk_enum :
    Types.type_decl_ref -> [< T.sint ] t -> [< T.any ] t list -> [> T.adt ] t

  (** Creates a union ADT with the given value blocks. *)
  val mk_union :
    Types.type_decl_ref -> ([< T.any ] t * [< T.sint ] t) list -> [> T.adt ] t

  (** Creates an unknown polymorphic value. {b HACK: what does this even mean?}
  *)
  val mk_poly : Types.type_var_id -> [> T.adt ] t

  (** Gets the blocks of this adt as a union; returns [None] if the ADT is not a
      union. *)
  val as_union : [< T.adt ] t -> ([> T.any ] t * [> T.sint ] t) list

  val as_tuple : [< T.adt ] t -> [> T.any ] t list
  val as_enum : [< T.adt ] t -> [> T.sint ] t * [> T.any ] t list

  (**  *)
  val discriminant_of : [< T.adt ] t -> [< T.sint ] t

  val field_of : [< T.adt ] t -> int -> [> T.any ] t

  module Checked : sig
    val mk_enum :
      Types.type_decl_ref -> string -> [< T.any ] t list -> [> T.adt ] t
  end
end

module Syntax : sig
  module U8 : sig
    module Sym_int_syntax : sig
      val mk_nonzero : int -> [> T.nonzero ] t
      val zero : unit -> [> T.sint ] t
      val one : unit -> [> T.nonzero ] t
    end
  end

  module U32 : module type of U8
  module Usize : module type of U8
end
