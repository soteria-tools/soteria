(** {2 Phantom types} *)

module T : sig
  type sint = [ `NonZero | `MaybeZero ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type sptr = [ `Ptr ]
  type sloc = [ `Loc ]
  type 'a sseq = [ `List of 'a ]
  type 'a sopt = [ `Opt of 'a ]
  type cval = [ sint | sptr ]

  type any =
    [ `Bool | `Ptr | `Loc | `List of any | `Opt of any | `NonZero | `MaybeZero ]

  val pp_sint : sint Fmt.t
  val pp_nonzero : nonzero Fmt.t
  val pp_sbool : sbool Fmt.t
  val pp_sptr : sptr Fmt.t
  val pp_sloc : sloc Fmt.t
  val pp_cval : cval Fmt.t
  val pp_sseq : 'a Fmt.t -> 'a sseq Fmt.t
  val pp_sopt : 'a Fmt.t -> 'a sopt Fmt.t
  val pp_any : any Fmt.t
end

open T

(** {2 Types} *)
type +'a ty

val pp_ty :
  (Format.formatter -> 'a ty -> unit) -> Format.formatter -> 'a ty -> unit

val ppa_ty : Format.formatter -> 'a ty -> unit
val t_bool : [> sbool ] ty
val t_int : [> sint ] ty
val t_ptr : [> sptr ] ty
val t_loc : [> sloc ] ty
val t_seq : ([< any ] as 'a) ty -> [> 'a sseq ] ty
val t_opt : ([< any ] as 'a) ty -> [> 'a sopt ] ty

(** {2 Typed svalues} *)

type +'a t

val nondet :
  ?constrs:(([< any ] as 'a) t -> [> sbool ] t list) -> 'a ty -> 'a t Csymex.t

val get_ty : 'a t -> Svalue.ty
val type_ : Svalue.t -> 'a t
val type_checked : Svalue.t -> 'a ty -> 'a t option
val cast : 'a t -> 'b t
val cast_checked : 'a t -> 'b ty -> 'b t option
val untyped : 'a t -> Svalue.t
val untyped_list : 'a t list -> Svalue.t list
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val ppa : Format.formatter -> 'a t -> unit
val equal : ([< any ] as 'a) t -> 'a t -> bool
val compare : ([< any ] as 'a) t -> 'a t -> int
val sem_eq : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
val v_true : [> sbool ] t
val v_false : [> sbool ] t
val bool : bool -> [> sbool ] t
val and_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
val or_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
val not : [< sbool ] t -> [> sbool ] t
val not_int_bool : [< sint ] t -> [> sint ] t
val distinct : 'a t list -> [> sbool ] t list
val int_z : Z.t -> [> sint ] t
val int : int -> [> sint ] t
val nonzero_z : Z.t -> [> nonzero ] t
val nonzero : int -> [> nonzero ] t
val int_of_bool : [< sbool ] t -> [> sint ] t
val bool_of_int : [< sint ] t -> [> sbool ] t
val zero : [> sint ] t
val one : [> nonzero ] t
val geq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val gt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val leq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val lt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val plus : [< sint ] t -> [< sint ] t -> [> sint ] t
val minus : [< sint ] t -> [< sint ] t -> [> sint ] t
val times : [< sint ] t -> [< sint ] t -> [> sint ] t
val div : [< sint ] t -> nonzero t -> [> sint ] t

val check_nonzero :
  sint t -> ([> nonzero ] t, [> `NonZeroIsZero ]) Csymex.Result.t

module Ptr : sig
  val mk : [< sloc ] t -> [< sint ] t -> [> sptr ] t
  val loc : [< sptr ] t -> [> sloc ] t
  val ofs : [< sptr ] t -> [> sint ] t
  val loc_of_int : int -> [> sloc ] t
  val null : [> sptr ] t
  val null_loc : [> sloc ] t
  val is_null : [< sptr ] t -> [> sbool ] t
  val is_at_null_loc : [< sptr ] t -> [> sbool ] t
end

module SOption : sig
  val is_some : [< 'a sopt ] t -> [> sbool ] t
  val is_none : [< 'a sopt ] t -> [> sbool ] t
  val unwrap : [< 'a sopt ] t -> 'a t
  val none : inner_ty:([< any ] as 'a) ty -> 'a sopt t
  val some : 'a t -> 'a sopt t
end

module SSeq : sig
  val mk : inner_ty:([< any ] as 'a) ty -> 'a t list -> [> 'a sseq ] t
end

module Infix : sig
  val ( #== ) : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
  val ( #> ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( #>= ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( #< ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( #<= ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( #&& ) : [< sbool ] t -> [< sbool ] t -> [< sbool ] t
  val ( #|| ) : [< sbool ] t -> [< sbool ] t -> [< sbool ] t
  val ( #+ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( #- ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ~- ) : [< sint ] t -> [> sint ] t
  val ( #* ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( #/ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
end

module Result : sig
  type pc = sbool t
  type nonrec ('a, 'b) t = ('a, 'b) Result.t Csymex.t

  val ok : ?learned:pc list -> 'a -> ('a, 'b) t
  val error : ?learned:pc list -> 'b -> ('a, 'b) t
  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  val fold_left :
    'a list -> init:'acc -> f:('acc -> 'a -> ('acc, 'b) t) -> ('acc, 'b) t
end

module Syntax : sig
  module Symex_syntax : sig
    val branch_on :
      sbool t ->
      then_:(unit -> 'a Csymex.t) ->
      else_:(unit -> 'a Csymex.t) ->
      'a Csymex.t
  end

  module Sym_int_syntax : sig
    val mk_int : int -> [> sint ] t
    val zero : [> sint ] t
    val one : [> sint ] t
  end
end
