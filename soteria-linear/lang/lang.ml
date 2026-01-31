open Soteria.Soteria_std
module String_map = Map.MakePp (Soteria.Soteria_std.String)

module BinOp = struct
  type t = Add | Sub | Mul | Div | And | Or | Eq | Lt
  [@@deriving show { with_path = false }]
end

module Pure_expr = struct
  type t =
    | Int of int  (** Integer literal *)
    | Bool of bool  (** Boolean literal *)
    | Var of string  (** Variable reference *)
    | BinOp of t * BinOp.t * t  (** Binary operation *)
    | NondetInt  (** Nondeterministic integer *)
  [@@deriving show { with_path = false }]
end

(* module Asrt = struct
 *   type atom =
 *     | TakePointsTo of string * Pure_expr.t  (** Take points-to assertion *)
 *     | TakePointsToFreed of Pure_expr.t  (** Take points-to-freed assertion *)
 *     | Pure of Pure_expr.t  (** Pure assertion *)
 *   [@@deriving show { with_path = false }]
 *
 *   (** Separating conjunction of atomic assertions *)
 *   type t = atom list [@@deriving show { with_path = false }]
 * end *)

module Expr = struct
  type t =
    | Pure_expr of Pure_expr.t  (** Pure expression *)
    | Let of string option * t * t  (** Let binding *)
    | If of t * t * t  (** If expression *)
    | Load of Pure_expr.t  (** Load from memory address *)
    | Store of Pure_expr.t * Pure_expr.t  (** Store to memory address *)
    | Alloc
    | Free of Pure_expr.t
    | Call of string * Pure_expr.t list  (** Function call *)
  [@@deriving show { with_path = false }]
end

(* module Spec = struct
 *   type t = { pre : Asrt.t; post : Asrt.t }
 *   [@@deriving show { with_path = false }]
 * end *)

module Fun_def = struct
  type t = {
    name : string;
    args : string list;
    body : Expr.t; (* spec : Spec.t option; *)
  }
  [@@deriving show { with_path = false }]
end

module Program = struct
  type t = Fun_def.t String_map.t [@@deriving show { with_path = false }]
end
