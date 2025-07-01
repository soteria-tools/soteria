open Hc
module T = Typed.T

type t_kind =
  | Core of Svalue.t
  | Complex of Svalue.t * Svalue.t
  | None
  | NotImplemented
  | Ellipsis

and t_node = { kind : t_kind; ty : Classes.t }
and t = t_node hash_consed

let equal_t_kind a b =
  match (a, b) with
  | Core a, Core b -> Svalue.equal a b
  | Complex (a1, a2), Complex (b1, b2) ->
      Svalue.equal a1 b1 && Svalue.equal a2 b2
  | _ -> false

let hash_t_kind kind =
  match kind with
  | Core v -> Svalue.hash v
  | Complex (a, b) -> Hashtbl.hash (Svalue.hash a, Svalue.hash b)
  | None | NotImplemented | Ellipsis -> Hashtbl.hash kind

let equal_t_node a b = Classes.equal a.ty b.ty && equal_t_kind a.kind b.kind

let hash_t_node { kind; ty } =
  let hty = Classes.hash ty in
  Hashtbl.hash (hash_t_kind kind, hty)

module Hcons = Hc.Make (struct
  type t = t_node

  let equal = equal_t_node
  let hash = hash_t_node
end)

let ( <| ) kind ty =
  let node = { kind; ty } in
  Hcons.hashcons node

let v_true = Core Svalue.v_true <| Classes.Builtin Classes.Bool
let v_false = Core Svalue.v_false <| Classes.Builtin Classes.Bool
let bool b = if b then v_true else v_false
let int i = Core (Svalue.int i) <| Classes.Builtin Classes.Int
let int_z z = Core (Svalue.int_z z) <| Classes.Builtin Classes.Int
let none = None <| Classes.Builtin Classes.NoneType
let float f = Core (Svalue.float_f F64 f) <| Classes.Builtin Classes.Float

let complex (re : float) (im : float) =
  Complex (Svalue.float_f F64 re, Svalue.float_f F64 im)
  <| Classes.Builtin Classes.Complex

let not_implemented =
  NotImplemented <| Classes.Builtin Classes.NotImplementedType

let ellipsis = Ellipsis <| Classes.Builtin Classes.EllipsisType
