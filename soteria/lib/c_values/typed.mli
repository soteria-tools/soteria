(** {2 Phantom types} *)

module T : sig
  type sint = [ `NonZero | `MaybeZero ]
  type sfloat = [ `Float ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type sptr = [ `Ptr ]
  type sloc = [ `Loc ]
  type 'a sseq = [ `List of 'a ]
  type cval = [ sint | sptr | sfloat ]

  type any =
    [ `Bool | `Ptr | `Loc | `List of any | `NonZero | `MaybeZero | `Float ]

  val pp_sint : Format.formatter -> sint -> unit
  val pp_nonzero : Format.formatter -> nonzero -> unit
  val pp_sbool : Format.formatter -> sbool -> unit
  val pp_sptr : Format.formatter -> sptr -> unit
  val pp_sloc : Format.formatter -> sloc -> unit
  val pp_cval : Format.formatter -> cval -> unit

  val pp_sseq :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a sseq -> unit

  val pp_any : Format.formatter -> any -> unit
end

open T

(** {2 Types} *)
type +'a ty

val pp_ty :
  (Format.formatter -> 'a ty -> unit) -> Format.formatter -> 'a ty -> unit

val ppa_ty : Format.formatter -> 'a ty -> unit
val equal_ty : 'a ty -> 'b ty -> bool
val t_bool : [> sbool ] ty
val t_int : [> sint ] ty
val t_ptr : [> sptr ] ty
val t_loc : [> sloc ] ty
val t_seq : ([< any ] as 'a) ty -> [> 'a sseq ] ty
val t_f16 : [> sfloat ] ty
val t_f32 : [> sfloat ] ty
val t_f64 : [> sfloat ] ty
val t_f128 : [> sfloat ] ty

(** {2 Typed svalues} *)

type +'a t
type sbool = T.sbool

(** Basic value operations *)

val get_ty : 'a t -> Svalue.ty
val untype_type : 'a ty -> Svalue.ty
val kind : 'a t -> Svalue.t_kind
val mk_var : Svalue.Var.t -> 'a ty -> 'a t
val iter_vars : 'a t -> (Svalue.Var.t * 'b ty -> unit) -> unit
val subst : (Svalue.Var.t -> Svalue.Var.t) -> 'a t -> 'a t
val type_ : Svalue.t -> 'a t
val type_checked : Svalue.t -> 'a ty -> 'a t option
val cast : 'a t -> 'b t
val cast_checked : 'a t -> 'b ty -> 'b t option
val cast_checked2 : 'a t -> 'b t -> ('c t * 'c t * 'c ty) option
val cast_float : 'a t -> [> sfloat ] t option
val is_float : 'a ty -> bool
val untyped : 'a t -> Svalue.t
val untyped_list : 'a t list -> Svalue.t list
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val ppa : Format.formatter -> 'a t -> unit
val equal : ([< any ] as 'a) t -> 'a t -> bool
val compare : ([< any ] as 'a) t -> 'a t -> int

(** Typed constructors *)

val sem_eq : 'a t -> 'a t -> sbool t
val sem_eq_untyped : 'a t -> 'a t -> [> sbool ] t
val v_true : [> sbool ] t
val v_false : [> sbool ] t
val bool : bool -> [> sbool ] t
val as_bool : 'a t -> bool option
val and_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
val conj : [< sbool ] t list -> [> sbool ] t
val split_ands : [< sbool ] t -> ([> sbool ] t -> unit) -> unit
val or_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
val not : sbool t -> sbool t
val not_int_bool : [< sint ] t -> [> sint ] t
val distinct : 'a t list -> [> sbool ] t
val ite : [< sbool ] t -> 'a t -> 'a t -> 'a t
val int_z : Z.t -> [> sint ] t
val int : int -> [> sint ] t
val nonzero_z : Z.t -> [> nonzero ] t
val nonzero : int -> [> nonzero ] t
val float : Svalue.FloatPrecision.t -> string -> [> sfloat ] t
val int_of_bool : [< sbool ] t -> [> sint ] t
val bool_of_int : [< sint ] t -> [> sbool ] t
val int_of_float : int -> [< sfloat ] t -> [> sint ] t
val float_of_int : Svalue.FloatPrecision.t -> [< sint ] t -> [> sfloat ] t
val zero : [> sint ] t
val one : [> nonzero ] t
val f16 : float -> [> sfloat ] t
val f32 : float -> [> sfloat ] t
val f64 : float -> [> sfloat ] t
val f128 : float -> [> sfloat ] t
val float_like : [> sfloat ] t -> float -> [> sfloat ] t
val fp_of : [< sfloat ] t -> Svalue.FloatPrecision.t

(** Integer operations *)

val geq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val gt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val leq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val lt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val plus : [< sint ] t -> [< sint ] t -> [> sint ] t
val minus : [< sint ] t -> [< sint ] t -> [> sint ] t
val times : [< sint ] t -> [< sint ] t -> [> sint ] t
val div : [< sint ] t -> [< nonzero ] t -> [> sint ] t
val rem : [< sint ] t -> [< nonzero ] t -> [> sint ] t
val mod_ : [< sint ] t -> nonzero t -> [> sint ] t
val neg : ([< sint | sfloat ] as 'a) t -> 'a t

val bit_and :
  size:int -> signed:bool -> [< sint ] t -> [< sint ] t -> [> sint ] t

val bit_or :
  size:int -> signed:bool -> [< sint ] t -> [< sint ] t -> [> sint ] t

val bit_xor :
  size:int -> signed:bool -> [< sint ] t -> [< sint ] t -> [> sint ] t

val bit_shl :
  size:int -> signed:bool -> [< sint ] t -> [< sint ] t -> [> sint ] t

val bit_shr :
  size:int -> signed:bool -> [< sint ] t -> [< sint ] t -> [> sint ] t

(** Floating point operations *)

val eq_f : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
val geq_f : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
val gt_f : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
val leq_f : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
val lt_f : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
val plus_f : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
val minus_f : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
val times_f : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
val div_f : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
val rem_f : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
val abs_f : [< sfloat ] t -> [> sfloat ] t
val is_normal : [< sfloat ] t -> [> sbool ] t
val is_subnormal : [< sfloat ] t -> [> sbool ] t
val is_zero : [< sfloat ] t -> [> sbool ] t
val is_infinite : [< sfloat ] t -> [> sbool ] t
val is_nan : [< sfloat ] t -> [> sbool ] t
val float_round : Svalue.FloatRoundingMode.t -> [< sfloat ] t -> [> sfloat ] t

module Ptr : sig
  val mk : [< sloc ] t -> [< sint ] t -> [> sptr ] t
  val loc : [< sptr ] t -> [> sloc ] t
  val ofs : [< sptr ] t -> [> sint ] t
  val decompose : [< sptr ] t -> [> sloc ] t * [> sint ] t
  val add_ofs : [< sptr ] t -> [< sint ] t -> [> sptr ] t
  val loc_of_int : int -> [> sloc ] t
  val null : [> sptr ] t
  val null_loc : [> sloc ] t
  val is_null_loc : [< sloc ] t -> [> sbool ] t
  val is_null : [< sptr ] t -> [> sbool ] t
  val is_at_null_loc : [< sptr ] t -> [> sbool ] t
end

module SSeq : sig
  val mk : seq_ty:'a sseq ty -> 'a t list -> [> 'a sseq ] t
end

module Infix : sig
  val ( ==@ ) : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
  val ( ==?@ ) : 'a t -> 'a t -> [> sbool ] t
  val ( >@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( &&@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
  val ( ||@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
  val ( +@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( -@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ~- ) : [< sint ] t -> [> sint ] t
  val ( *@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( /@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( %@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( ==.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( >.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( >=.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( <.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( <=.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( +.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val ( -.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val ( *.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val ( /.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
end

module Syntax : sig
  module Sym_int_syntax : sig
    val mk_int : int -> [> sint ] t
    val zero : [> sint ] t
    val one : [> sint ] t
  end
end
