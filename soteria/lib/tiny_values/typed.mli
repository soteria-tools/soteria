(** {2 Phantom types} *)

module T : sig
  type sint = [ `NonZero | `Zero ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type any = [ `Bool | `NonZero | `Zero ]

  val pp_sint : Format.formatter -> sint -> unit
  val pp_nonzero : Format.formatter -> nonzero -> unit
  val pp_sbool : Format.formatter -> sbool -> unit
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
val untyped : 'a t -> Svalue.t
val untyped_list : 'a t list -> Svalue.t list
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val ppa : Format.formatter -> 'a t -> unit
val equal : ([< any ] as 'a) t -> 'a t -> bool
val compare : ([< any ] as 'a) t -> 'a t -> int
val hash : [< any ] t -> int

(** Typed constructors *)

val sem_eq : 'a t -> 'a t -> sbool t
val sem_eq_untyped : 'a t -> 'b t -> [> sbool ] t
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
val int_z : Z.t -> [> sint ] t
val int : int -> [> sint ] t
val nonzero_z : Z.t -> [> nonzero ] t
val nonzero : int -> [> nonzero ] t
val zero : [> sint ] t
val one : [> nonzero ] t

(** Integer operations *)

val geq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val gt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val leq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val lt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val add : [< sint ] t -> [< sint ] t -> [> sint ] t
val sub : [< sint ] t -> [< sint ] t -> [> sint ] t
val mul : [< sint ] t -> [< sint ] t -> [> sint ] t
val div : [< sint ] t -> [< nonzero ] t -> [> sint ] t
val rem : [< sint ] t -> [< nonzero ] t -> [> sint ] t
val mod_ : [< sint ] t -> nonzero t -> [> sint ] t
val neg : [< sint ] t -> [> sint ] t

module Infix : sig
  val ( ==@ ) : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
  val ( ==?@ ) : 'a t -> 'b t -> [> sbool ] t
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
end

module Syntax : sig
  module Sym_int_syntax : sig
    val mk_nonzero : int -> [> nonzero ] t
    val zero : unit -> [> sint ] t
    val one : unit -> [> sint ] t
  end
end

module Expr : Symex.Value.Expr with type 'a v := 'a t and type 'a ty := 'a ty
