open Soteria_std
open Simple_smt

type t = Svalue.t
type ty = Svalue.ty

let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let init_commands = []
let sort_of_ty : ty -> sexp = function TBool -> t_bool | TInt -> t_int
let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023
let smt_of_unop : Svalue.Unop.t -> sexp -> sexp = function Not -> bool_not

let smt_of_binop : Svalue.Binop.t -> sexp -> sexp -> sexp = function
  | Eq -> eq
  | Leq -> num_leq
  | Lt -> num_lt
  | And -> bool_and
  | Or -> bool_or
  | Plus -> num_add
  | Minus -> num_sub
  | Times -> num_mul
  | Div -> num_div
  | Rem -> num_rem
  | Mod -> num_mod

let rec encode_value (v : Svalue.t) =
  match v.node.kind with
  | Var v -> atom (Svalue.Var.to_string v)
  | Int z -> int_zk z
  | Bool b -> bool_k b
  | Ite (c, t, e) ->
      ite (encode_value_memo c) (encode_value_memo t) (encode_value_memo e)
  | Unop (unop, v1) ->
      let v1 = encode_value_memo v1 in
      smt_of_unop unop v1
  | Binop (binop, v1, v2) ->
      let v1 = encode_value_memo v1 in
      let v2 = encode_value_memo v2 in
      smt_of_binop binop v1 v2
  | Nop (Distinct, vs) ->
      let vs = List.map encode_value_memo vs in
      distinct vs

and encode_value_memo v =
  match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Hc.tag with
  | Some k -> k
  | None ->
      let k = encode_value v in
      Hashtbl.Hint.add memo_encode_value_tbl v.Hc.tag k;
      k

let encode_value (v : Svalue.t) =
  Svalue.split_ands v |> Iter.map encode_value_memo |> Iter.to_list |> bool_ands
