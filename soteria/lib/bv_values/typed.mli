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
val t_int : int -> [> sint ] ty
val t_ptr : int -> [> sptr ] ty
val t_loc : int -> [> sloc ] ty
val t_seq : ([< any ] as 'a) ty -> [> 'a sseq ] ty
val t_f16 : [> sfloat ] ty
val t_f32 : [> sfloat ] ty
val t_f64 : [> sfloat ] ty
val t_f128 : [> sfloat ] ty
val t_f : Svalue.FloatPrecision.t -> [> sfloat ] ty

(** {2 Typed svalues} *)

type +'a t
type sbool = T.sbool

(** Basic value operations *)

val get_ty : 'a t -> Svalue.ty
val type_type : Svalue.ty -> 'a ty
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
val cast_int : 'a t -> [> sint ] t option
val is_float : 'a ty -> bool
val size_of_int : [< sint ] t -> int
val untyped : 'a t -> Svalue.t
val untyped_list : 'a t list -> Svalue.t list
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val ppa : Format.formatter -> 'a t -> unit
val equal : ([< any ] as 'a) t -> 'a t -> bool
val compare : ([< any ] as 'a) t -> 'a t -> int
val hash : [< any ] t -> int

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
val distinct : 'a t list -> [> sbool ] t
val ite : [< sbool ] t -> 'a t -> 'a t -> 'a t

(** Bit vector operations *)

module BitVec : sig
  (* constructor *)
  val mk : int -> Z.t -> [> sint ] t
  val mk_masked : int -> Z.t -> [> sint ] t
  val mki : int -> int -> [> sint ] t
  val mki_masked : int -> int -> [> sint ] t
  val mk_nz : int -> Z.t -> [> nonzero ] t
  val mki_nz : int -> int -> [> nonzero ] t
  val zero : int -> [> sint ] t
  val one : int -> [> nonzero ] t
  val bv_to_z : bool -> int -> Z.t -> Z.t

  (* arithmetic *)
  val add : [< sint ] t -> [< sint ] t -> [> sint ] t
  val sub : [< sint ] t -> [< sint ] t -> [> sint ] t
  val mul : [< sint ] t -> [< sint ] t -> [> sint ] t
  val div : signed:bool -> [< sint ] t -> [< sint ] t -> [> sint ] t
  val rem : signed:bool -> [< sint ] t -> [< sint ] t -> [> sint ] t
  val mod_ : [< sint ] t -> [< sint ] t -> [> sint ] t
  val neg : [< sint ] t -> [> sint ] t

  (* overflow checks *)
  val add_overflows : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val sub_overflows : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val mul_overflows : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val neg_overflows : [< sint ] t -> [> sbool ] t

  (* inequalities *)
  val lt : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val leq : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val gt : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val geq : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t

  (* bitvec manipulation *)
  val concat : [< sint ] t -> [< sint ] t -> [> sint ] t
  val extend : signed:bool -> int -> [< sint ] t -> [> sint ] t
  val extract : int -> int -> [< sint ] t -> [> sint ] t

  (* bitwise operations *)
  val and_ : [< sint ] t -> [< sint ] t -> [> sint ] t
  val or_ : [< sint ] t -> [< sint ] t -> [> sint ] t
  val xor : [< sint ] t -> [< sint ] t -> [> sint ] t
  val shl : [< sint ] t -> [< sint ] t -> [> sint ] t
  val lshr : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ashr : [< sint ] t -> [< sint ] t -> [> sint ] t
  val not : [< sint ] t -> [> sint ] t

  (* bool-bv conversions *)
  val of_bool : int -> [< sbool ] t -> [> sint ] t
  val to_bool : [< sint ] t -> [> sbool ] t
  val not_bool : [< sint ] t -> [> sint ] t

  (* float-bv conversions *)
  val of_float : signed:bool -> [< sfloat ] t -> [> sint ] t
  val to_float : signed:bool -> [< sint ] t -> [> sfloat ] t
end

(** Floating point operations *)

module Float : sig
  val mk : Svalue.FloatPrecision.t -> string -> [> sfloat ] t
  val f16 : float -> [> sfloat ] t
  val f32 : float -> [> sfloat ] t
  val f64 : float -> [> sfloat ] t
  val f128 : float -> [> sfloat ] t
  val like : [> sfloat ] t -> float -> [> sfloat ] t
  val fp_of : [< sfloat ] t -> Svalue.FloatPrecision.t
  val eq : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val geq : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val gt : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val leq : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val lt : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val add : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val sub : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val mul : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val div : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val rem : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val abs : [< sfloat ] t -> [> sfloat ] t
  val neg : [< sfloat ] t -> [> sfloat ] t
  val is_normal : [< sfloat ] t -> [> sbool ] t
  val is_subnormal : [< sfloat ] t -> [> sbool ] t
  val is_zero : [< sfloat ] t -> [> sbool ] t
  val is_infinite : [< sfloat ] t -> [> sbool ] t
  val is_nan : [< sfloat ] t -> [> sbool ] t
  val round : Svalue.FloatRoundingMode.t -> [< sfloat ] t -> [> sfloat ] t
end

module Ptr : sig
  val mk : [< sloc ] t -> [< sint ] t -> [> sptr ] t
  val loc : [< sptr ] t -> [> sloc ] t
  val ofs : [< sptr ] t -> [> sint ] t
  val decompose : [< sptr ] t -> [> sloc ] t * [> sint ] t
  val add_ofs : [< sptr ] t -> [< sint ] t -> [> sptr ] t
  val loc_of_int : int -> int -> [> sloc ] t
  val null : int -> [> sptr ] t
  val null_loc : int -> [> sloc ] t
  val is_null_loc : [< sloc ] t -> [> sbool ] t
  val is_null : [< sptr ] t -> [> sbool ] t
  val is_at_null_loc : [< sptr ] t -> [> sbool ] t
end

module SSeq : sig
  val mk : seq_ty:'a sseq ty -> 'a t list -> [> 'a sseq ] t
end

module Infix : sig
  val ( ==@ ) : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
  val ( ==?@ ) : 'a t -> 'b t -> [> sbool ] t
  val ( >@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >=$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <=$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( &&@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
  val ( ||@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
  val ( +@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( -@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ~- ) : [< sint ] t -> [> sint ] t
  val ( *@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( /@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( /$@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( %@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( %$@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( <<@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( >>@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( >>>@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ^@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( &@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( |@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
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
