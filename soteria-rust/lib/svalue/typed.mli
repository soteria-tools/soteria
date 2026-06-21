open Charon

(* The base, non-extended ghost-typed module ([Typed_intf.S], with the base
   [T.any]). It is used to instantiate the solver and symex, which require
   exactly that signature. *)
module Solver_value : Soteria.Bv_values.Typed.S with module Ext = Ext.Rust_ext

(* The extended ghost-typed interface, sharing [Solver_value]'s [t]/[ty] so
   values flow between the interpreter and the symex monad. *)
include
  Soteria.Bv_values.Typed.S
    with module Ext = Ext.Rust_ext
     and type 'a t = 'a Solver_value.t
     and type 'a ty = 'a Solver_value.ty

(* T *)

module T : sig
  include module type of T

  type sptr_f = [ `FullPtr ]
  type sptr_t = [ `ThinPtr ]
  type tuple = [ `Tuple ]
  type enum = [ `Enum ]
  type union = [ `Union ]
  type poly = [ `Poly ]
  type ptr_meta = [ sptr_t | sint ]

  (** The values that are allowed within the Rust interpreter as standalone
      values. *)
  type any = [ sint | sfloat | sptr_f | tuple | enum | union | poly ]

  val pp_sptr_f : Format.formatter -> sptr_f -> unit
  val pp_sptr_t : Format.formatter -> sptr_t -> unit
  val pp_tuple : Format.formatter -> tuple -> unit
  val pp_enum : Format.formatter -> enum -> unit
  val pp_union : Format.formatter -> union -> unit
  val pp_poly : Format.formatter -> poly -> unit
  val pp_any : Format.formatter -> any -> unit
end

(* types *)

val t_loc : unit -> [> T.sloc ] ty

(** The raw pointer type. Only meant for {!Ptr.of_raw}; prefer thin/full
    pointers everywhere else. *)
val t_ptr : unit -> [> T.sptr ] ty

val t_float : Types.float_type -> T.sfloat ty
val t_usize : unit -> [> T.sint ] ty
val t_lit : Types.literal_type -> [> T.sint ] ty
val t_ptr_f : unit -> [> T.sptr_f ] ty
val t_ptr_t : unit -> [> T.sptr_t ] ty

(* casts *)

(* Casts reinterpret a value and verify the result kind at runtime, so they
   accept any input — including non-standalone values such as pointer metadata
   (a vtable is a thin pointer, which is not part of [T.any]). *)
val cast_checked : ty:([< T.any ] as 'a) ty -> _ t -> 'a t
val cast_float : _ t -> [> T.sfloat ] t
val cast_i : Values.u_int_ty -> _ t -> [> T.sint ] t
val cast_f : Types.float_type -> _ t -> T.sfloat t
val cast_lit : Types.literal_type -> _ t -> [> T.sint ] t
val cast_ptr_f : _ t -> [< T.sptr_f ] t
val cast_ptr_t : _ t -> [< T.sptr_t ] t
val cast_tuple : _ t -> [> T.tuple ] t
val cast_enum : ?adt:Types.type_decl_ref -> _ t -> [> T.enum ] t
val cast_union : ?adt:Types.type_decl_ref -> _ t -> [> T.union ] t

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
  val of_constant_expr_opt : Types.constant_expr -> [> T.sint ] t option
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
  (* locations *)
  val loc_of_int : int -> [> T.sloc ] t
  val null_loc : unit -> [> T.sloc ] t
  val is_null_loc : [< T.sloc ] t -> [> T.sbool ] t

  (* thin pointers *)
  val mk_ptr_t :
    loc:[< T.sloc ] t ->
    ofs:[< T.sint ] t ->
    size:[< T.sint ] t ->
    align:[< T.nonzero ] t ->
    tag:Ptr_tag.t option ->
    [> T.sptr_t ] t

  val of_raw :
    ptr:[< T.sptr ] t ->
    size:[< T.sint ] t ->
    align:[< T.nonzero ] t ->
    tag:Ptr_tag.t option ->
    [> T.sptr_t ] t

  val null : unit -> [> T.sptr_t ] t
  val of_address : [< T.sint ] t -> [> T.sptr_t ] t
  val loc : [< T.sptr_t ] t -> [> T.sloc ] t
  val ofs : [< T.sptr_t ] t -> [> T.sint ] t
  val decompose : [< T.sptr_t ] t -> [> T.sloc ] t * [> T.sint ] t
  val add_ofs : [< T.sptr_t ] t -> [< T.sint ] t -> [> T.sptr_t ] t
  val set_ofs : [< T.sptr_t ] t -> [< T.sint ] t -> [> T.sptr_t ] t
  val is_null : [< T.sptr_t ] t -> [> T.sbool ] t
  val is_at_null_loc : [< T.sptr_t ] t -> [> T.sbool ] t
  val has_provenance : [< T.sptr_t ] t -> [> T.sbool ] t

  val have_same_provenance :
    [< T.sptr_t ] t -> [< T.sptr_t ] t -> [> T.sbool ] t

  val in_bound : [< T.sptr_t ] t -> [> T.sbool ] t
  val align_of : [< T.sptr_t ] t -> [> T.nonzero ] t
  val size_of : [< T.sptr_t ] t -> [> T.sint ] t
  val allocation_info : [< T.sptr_t ] t -> [> T.sint ] t * [> T.nonzero ] t
  val tag_of : [< T.sptr_t ] t -> Ptr_tag.t option
  val with_tag : [< T.sptr_t ] t -> Ptr_tag.t option -> [> T.sptr_t ] t
  val as_id : [< T.sptr_t ] t -> [> T.sint ] t

  (* full pointers *)
  val mk_ptr_f : [< T.sptr_t ] t -> [< T.ptr_meta ] t option -> [> T.sptr_f ] t
  val null_f : unit -> [> T.sptr_f ] t
  val of_address_f : [< T.sint ] t -> [> T.sptr_f ] t
  val split : [< T.sptr_f ] t -> [> T.sptr_t ] t * [> T.ptr_meta ] t option
  val meta_of : [< T.sptr_f ] t -> [> T.ptr_meta ] t option
  val ptr_of : [< T.sptr_f ] t -> [> T.sptr_t ] t
end

module Adt : sig
  (** Creates a tuple value with the given fields. This may be an array, struct,
      or tuple. *)
  val mk_tuple : [< T.any ] t list -> [> T.tuple ] t

  (** Creates an enum value with the given discriminant and fields. *)
  val mk_enum :
    Types.type_decl_ref -> [< T.sint ] t -> [< T.any ] t list -> [> T.enum ] t

  (** Creates a union value with the given value blocks. *)
  val mk_union :
    Types.type_decl_ref ->
    ([< T.any ] t * [< T.sint ] t * [< T.nonzero ] t) list ->
    [> T.union ] t

  (** Creates an unknown polymorphic value. {b HACK: what does this even mean?}
  *)
  val mk_poly : Types.type_var_id -> [> T.poly ] t

  (** Gets the value blocks of a union. *)
  val as_union :
    [< T.union ] t -> ([> T.any ] t * [> T.sint ] t * [> T.nonzero ] t) list

  val as_tuple : [< T.tuple ] t -> [> T.any ] t list

  (** Like {!as_tuple}, but casts to a fixed-arity tuple, failing if the tuple
      does not have exactly that many fields. Avoids partial list pattern
      matches at call sites. *)
  val as_tuple1 : [< T.tuple ] t -> [> T.any ] t

  val as_tuple2 : [< T.tuple ] t -> [> T.any ] t * [> T.any ] t
  val as_tuple3 : [< T.tuple ] t -> [> T.any ] t * [> T.any ] t * [> T.any ] t
  val as_enum : [< T.enum ] t -> [> T.sint ] t * [> T.any ] t list
  val as_type_var : [< T.poly ] t -> Types.type_var_id
  val discriminant_of : [< T.enum ] t -> [> T.sint ] t

  (* Field access is split by kind: tuples/structs/arrays and enum variants
     store their fields differently (an enum also keeps its discriminant). *)
  val field_of : int -> [< T.tuple ] t -> [> T.any ] t

  val field_of_variant :
    Types.variant_id -> int -> [< T.enum ] t -> [> T.any ] t

  val set_field : int -> [< T.any ] t -> [< T.tuple ] t -> [> T.tuple ] t

  val set_field_of_variant :
    Types.variant_id -> int -> [< T.any ] t -> [< T.enum ] t -> [> T.enum ] t

  val update_field :
    int -> ([< T.any ] t -> [> T.any ] t) -> [< T.tuple ] t -> [> T.tuple ] t

  val update_field_of_variant :
    Types.variant_id ->
    int ->
    ([< T.any ] t -> [> T.any ] t) ->
    [< T.enum ] t ->
    [> T.enum ] t

  module Checked : sig
    val mk_enum :
      Types.type_decl_ref -> string -> [< T.any ] t list -> [> T.enum ] t
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
