open Hashcons

module Var : sig
  type t

  val to_string : t -> string
  val pp : t Fmt.t
  val eq : t -> t -> bool
  val fresh : unit -> t
end

type ty =
  | Bool
  | Int
  | Pointer
  (* | BitVec of int *)
  | Seq of ty
[@@deriving eq]

type t_node =
  | Var of (Var.t * ty)
  | Bool of bool
  | Int of Z.t
  | Ptr of (t * t)
  | Seq of t list
[@@deriving show]

and t = t_node hash_consed

val bool : bool -> t
val int_z : Z.t -> t
val int : int -> t
val ptr : t -> t -> t
val seq : t list -> t
val zero : t
val one : t
val pp : t Fmt.t
val fresh : ty -> t
