open Charon
include module type of Typed_core

(* T *)

module T : sig
  include module type of T

  type sptr_f = [ `FullPtr ]
  type adt = [ `Adt ]
  type ptr_meta = [ sptr | sint ]

  type any =
    [ sint_ovf | sfloat | sbool | sptr | sloc | any sseq | sptr_f | adt ]

  val pp_sptr_f : Format.formatter -> sptr_f -> unit
  val pp_adt : Format.formatter -> adt -> unit
  val pp_any : Format.formatter -> any -> unit
end

(* types *)

type ptr = {
  ptr : T.sptr t;
  size : T.sint t;
  align : T.nonzero t;
  tag : Ptr_tag.t option;
}

type full_ptr = ptr * T.ptr_meta t option

val t_ptr : unit -> [> T.sptr ] ty
val t_loc : unit -> [> T.sloc ] ty
val t_float : Types.float_type -> T.sfloat ty
val t_usize : unit -> [> T.sint ] ty
val t_lit : Types.literal_type -> [> T.sint | T.sfloat ] ty
val t_fptr : unit -> [> T.sptr_f ] ty

(* casts *)

val cast_checked : ty:([< T.any ] as 'a) ty -> [< T.any ] t -> 'a t
val cast_float : [< T.any ] t -> [> T.sfloat ] t
val cast_i : Values.u_int_ty -> [< T.any ] t -> [> T.sint ] t
val cast_f : Types.float_type -> [< T.any ] t -> T.sfloat t
val cast_lit : Types.literal_type -> [< T.any ] t -> [> T.sint ] t
val cast_ptr : [< T.any ] t -> [< T.sptr ] t
val cast_fptr : [< T.any ] t -> [< T.sptr_f ] t
val cast_adt : Types.type_decl_ref -> [< T.any ] t -> [> T.adt ] t

(* helpers *)

(** [fields_of v] returns the fields of [v], if [v] is
    - an ADT, which is either a struct or an enum (i.e. not a union)
    - a full pointer with {i non-unit} metadata

    Otherwise errors! *)
val fields_of : [< T.any ] t -> [< T.any ] t list

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
end

module BV = BitVec

module Float : sig
  include module type of Float

  val mk : Types.float_type -> string -> [> T.sfloat ] t
end

module Ptr : sig
  include module type of Ptr

  val loc_of_int : int -> [> T.sloc ] t
  val null : unit -> [> T.sptr ] t
  val null_loc : unit -> [> T.sloc ] t
  val mk_ptr_f : ptr -> [< T.ptr_meta ] t option -> [> T.sptr_f ] t
  val split : [< T.sptr_f ] t -> full_ptr
  val cast_meta : [< T.any ] t -> [> T.ptr_meta ] t option
  val meta_kind_of : [< T.ptr_meta ] t -> [ `VTable | `Len ] option
end

module Adt : sig
  (** Creates a tuple ADT with the given value blocks. This may be an array,
      struct, or tuple. *)
  val mk_tuple : [< T.any ] t list -> [> T.adt ] t

  (** Creates a zero-sized ADT, containing no fields. Equivalent to
      [mk_tuple []]. *)
  val mk_zst : unit -> [> T.adt ] t

  (** Creates an enum ADT with the given discriminant and values. *)
  val mk_enum : [< T.sint ] t -> [< T.any ] t list -> [> T.adt ] t

  (** Creates a union ADT with the given value blocks. *)
  val mk_union : ([< T.any ] t * [< T.sint ] t) list -> [> T.adt ] t

  (** Creates an unknown polymorphic value. {b HACK: what does this even mean?}
  *)
  val mk_poly : Types.type_var_id -> [> T.adt ] t

  (** Gets the blocks of this adt as a union; returns [None] if the ADT is not a
      union. *)
  val as_union : [< T.adt ] t -> ([< T.any ] t * [< T.sint ] t) list option

  (**  *)
  val discriminant_of : [< T.adt ] t -> [< T.sint ] t
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
