(** {2 Phantom types} *)

type sint = [ `NonZero | `MaybeZero ]
type nonzero = [ `NonZero ]
type sbool = [ `Bool ]
type sptr = [ `Ptr ]
type sloc = [ `Loc ]
type svoid = [ `Void ]
type 'a sseq = [ `List of 'a ]
type 'a sopt = [ `Opt of 'a ]

type any =
  [ `Bool
  | `Ptr
  | `Loc
  | `Void
  | `List of any
  | `Opt of any
  | `NonZero
  | `MaybeZero ]

(** {2 Types} *)
type +'a ty

val t_bool : [> sbool ] ty
val t_int : [> sint ] ty
val t_ptr : [> sptr ] ty
val t_void : [> svoid ] ty
val t_seq : ([< any ] as 'a) ty -> [> 'a sseq ] ty
val t_opt : ([< any ] as 'a) ty -> [> 'a sopt ] ty

(** {2 Typed svalues} *)

type +'a t

val fresh : ([< any ] as 'a) ty -> 'a t
val type_ : Svalue.t -> 'a t
val cast : 'a t -> 'b t
val untyped : 'a t -> Svalue.t
val pp : Format.formatter -> 'a t -> unit
val equal : ([< any ] as 'a) t -> 'a t -> bool
val sem_eq : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
val v_true : [> sbool ] t
val v_false : [> sbool ] t
val bool : bool -> [> sbool ] t
val and_ : sbool t -> sbool t -> [> sbool ] t
val not : sbool t -> [> sbool ] t
val distinct : 'a t list -> [> sbool ] t
val int_z : Z.t -> [> sint ] t
val int : int -> [> sint ] t
val nonzero_z : Z.t -> [> nonzero ] t
val nonzero : int -> [> nonzero ] t
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
  val mk : [< sint ] t -> [< sint ] t -> [> sptr ] t
  val loc : [< sptr ] t -> [> sloc ] t
  val ofs : [< sptr ] t -> [> sint ] t
  val null : [> sptr ] t
  val is_null : [< sptr ] t -> [> sbool ] t
end

module SOption : sig
  val is_some : [< 'a sopt ] t -> [> sbool ] t
  val is_none : [< 'a sopt ] t -> [> sbool ] t
  val unwrap : [< 'a sopt ] t -> 'a t
  val none : 'a sopt t
  val some : 'a t -> 'a sopt t
end

module SSeq : sig
  val mk : ([< any ] as 'a) t list -> [> 'a sseq ] t
end

val void : unit t

module Infix : sig
  val ( #== ) : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
  val ( #> ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( #>= ) : [< sint ] t -> [< sint ] t -> sbool t
  val ( #< ) : [< sint ] t -> [< sint ] t -> sbool t
  val ( #<= ) : [< sint ] t -> [< sint ] t -> sbool t
  val ( #&& ) : [< sbool ] t -> [< sbool ] t -> [< sbool ] t
  val ( #+ ) : [< sint ] t -> [< sint ] t -> sint t
  val ( #- ) : [< sint ] t -> [< sint ] t -> sint t
  val ( #* ) : [< sint ] t -> [< sint ] t -> sint t
  val ( #/ ) : [< sint ] t -> nonzero t -> sint t
end

module Syntax : sig
  module Symex_syntax : sig
    val branch_on :
      sbool t ->
      then_:(unit -> 'a Csymex.t) ->
      else_:(unit -> 'a Csymex.t) ->
      'a Csymex.t
  end
end
