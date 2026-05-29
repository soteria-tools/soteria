open Charon
include module type of Typed_core

(* types *)

val t_ptr : unit -> [> T.sptr ] ty
val t_loc : unit -> [> T.sloc ] ty
val t_float : Types.float_type -> T.sfloat ty
val t_usize : unit -> [> T.sint ] ty
val t_lit : Types.literal_type -> [> T.sint | T.sfloat ] ty

(* casts *)

val cast_checked : ty:([< T.any ] as 'a) ty -> [< T.any ] t -> 'a t
val cast_float : [< T.any ] t -> [> T.sfloat ] t
val cast_i : Values.u_int_ty -> [< T.any ] t -> [> T.sint ] t
val cast_f : Types.float_type -> [< T.any ] t -> T.sfloat t
val cast_lit : Types.literal_type -> [< T.any ] t -> [> T.sint ] t

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
