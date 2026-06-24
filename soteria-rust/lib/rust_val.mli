open Charon
open Typed

type ('v, 'ptr) meta_raw = Thin | Len of 'v | VTable of 'ptr
type meta = (T.sint Typed.t, Sptr.t) meta_raw
type full_ptr = Sptr.t * meta
type ('sint, 'nz, 'float, 'ptr) raw
type t = (T.sint Typed.t, T.nonzero Typed.t, T.sfloat Typed.t, Sptr.t) raw

type rust_val =
  (T.sint Typed.t, T.nonzero Typed.t, T.sfloat Typed.t, Sptr.t) raw

type syn

val pp : t Fmt.t
val pp_full_ptr : full_ptr Fmt.t
val pp_syn : syn Fmt.t
val pp_meta_kind : _ meta_raw Fmt.t
val exprs_syn : syn -> Typed.Expr.t list
val to_syn : t -> syn
val subst : (Typed.Expr.t -> 'a Typed.t) -> syn -> t

module Learn_eq : sig
  val learn_eq : syn -> t -> (unit, _) Sptr.DecayMap.SM.Consumer.t
end

(* TEMP: this is a temporary function for the rust_val -> typed transition, to
   emulate Typed.get_ty *)
val get_ty : t -> [ `BitVec | `Float | `Ptr | `Tuple | `Enum | `Union | `Poly ]

(* Literals *)

val mk_int : [< T.sint ] Typed.t -> t
val mk_float : [< T.sfloat ] Typed.t -> t
val as_base : Types.literal_type -> t -> [> T.sint ] Typed.t
val as_base_i : Values.u_int_ty -> t -> [> T.sint ] Typed.t
val as_base_f : Values.float_type -> t -> [> T.sfloat ] Typed.t
val as_any_int : t -> [> T.sint ] Typed.t
val as_any_float : t -> [> T.sfloat ] Typed.t

(* Pointers *)

val as_ptr : t -> full_ptr
val mk_ptr : Sptr.t -> meta -> t
val mk_ptr' : full_ptr -> t

(* Tuples / Structs *)

val mk_tuple : t list -> t
val as_tuple : t -> t list
val as_tuple1 : t -> t
val as_tuple2 : t -> t * t
val as_tuple3 : t -> t * t * t
val field_of : int -> t -> t
val set_field : int -> t -> t -> t
val update_field : int -> (t -> t) -> t -> t

(* Enums *)

val mk_enum : Types.type_decl_ref -> [< T.sint ] Typed.t -> t list -> t
val discriminant_of : t -> [> T.sint ] Typed.t
val as_enum_of_variant : Types.variant_id -> t -> t list
val field_of_variant : Types.variant_id -> int -> t -> t
val set_field_of_variant : Types.variant_id -> int -> t -> t -> t
val update_field_of_variant : Types.variant_id -> int -> (t -> t) -> t -> t

(* Unions *)

val mk_union :
  Types.type_decl_ref -> Typed.(rust_val * T.sint t * T.nonzero t) list -> t

val as_union : t -> Typed.(rust_val * T.sint t * T.nonzero t) list

(* Poly val *)

val mk_poly : Types.type_var_id -> t
val as_type_var : t -> Types.type_var_id

module Checked : sig
  val mk_enum : Types.type_decl_ref -> string -> t list -> t
  val mk_tuple : Types.type_decl_ref -> t list -> t
end
