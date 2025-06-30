open Hc
module Var = Soteria_symex.Var

type ty = TBool [@@deriving eq]

let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag

type t_kind = Var of Var.t | Bool of bool [@@deriving eq]
and t_node = { kind : t_kind; ty : ty } [@@deriving eq]
and t = t_node hash_consed

let kind t = t.node.kind

module Hcons = Hc.Make (struct
  type t = t_node

  let equal = equal_t_node

  let hash { kind; ty } =
    let hty = Hashtbl.hash ty in
    match kind with Var _ | Bool _ -> Hashtbl.hash (kind, hty)
end)

let ( <| ) kind ty =
  let node = { kind; ty } in
  Hcons.hashcons node

let iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  match kind sv with Var v -> f (v, sv.node.ty) | Bool _ -> ()

let[@inline] equal a b = Int.equal a.tag b.tag
let v_true = Bool true <| TBool
let v_false = Bool false <| TBool

let sure_neq a b =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with Bool a, Bool b -> a <> b | _ -> false

(* let v_true =  *)
