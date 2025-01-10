open Cerb_frontend
open AilSyntax
open Bfa_std

module Sym_hashset = Hashset.Make (struct
  type t = Symbol.sym

  let pp = Fmt_ail.pp_sym
  let hash = Hashtbl.hash
  let equal = Symbol.equal_sym
end)

(** A callgraph is just a hashtbl mapping caller to the list of their callees *)
type t = (Symbol.sym, Sym_hashset.t) Hashtbl.t

let pp =
  let pp_caller ft (caller, callees) =
    Fmt.pf ft "@[<h>%a ->@ %a@]" Fmt_ail.pp_sym caller Sym_hashset.pp callees
  in
  Fmt.vbox (Fmt.iter_bindings ~sep:Fmt.sp Hashtbl.iter pp_caller)

let resolve_static_function fexpr =
  match fexpr with
  | AnnotatedExpression
      ( _,
        _,
        _,
        AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname)) ) ->
      Some fname
  | _ -> None

let rec expr_callees d (e : Ail_tys.expr) =
  let (AnnotatedExpression (_, _, _, e)) = e in
  let expr_callees = expr_callees d in
  match e with
  | AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname)) ->
      Sym_hashset.add d fname
  | AilEfunction_decay _ -> ()
  | AilEcall (func_expr, arg_exprs) ->
      expr_callees func_expr;
      List.iter expr_callees arg_exprs
  | AilEunary (_, expr)
  | AilEcast (_, _, expr)
  | AilEassert expr
  | AilEsizeof_expr expr
  | AilEannot (_, expr)
  | AilEva_start (expr, _)
  | AilEva_arg (expr, _)
  | AilEva_end expr
  | AilEprint_type expr
  | AilEbmc_assume expr
  | AilErvalue expr
  | AilEarray_decay expr
  | AilEmemberof (expr, _)
  | AilEmemberofptr (expr, _)
  | AilEatomic expr
  | AilEgeneric (expr, _) ->
      expr_callees expr
  | AilEva_copy (expr1, expr2)
  | AilEbinary (expr1, _, expr2)
  | AilEassign (expr1, expr2)
  | AilEcompoundAssign (expr1, _, expr2)
  | AilEcond (expr1, None, expr2) ->
      expr_callees expr1;
      expr_callees expr2
  | AilEcond (expr1, Some expr2, expr3) ->
      expr_callees expr1;
      expr_callees expr2;
      expr_callees expr3
  | AilEarray (_, _, exprs) -> List.iter (Option.iter expr_callees) exprs
  | AilEstruct (_, fields) ->
      List.iter (fun (_, expr_opt) -> Option.iter expr_callees expr_opt) fields
  | AilEunion (_, _, expr_opt) -> Option.iter expr_callees expr_opt
  | AilEcompound (_, _, expr) -> expr_callees expr
  | AilEgcc_statement (_, stmt) -> List.iter (stmt_callees d) stmt
  | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEident _ | AilEoffsetof _
  | AilEsizeof _ | AilEalignof _ | AilEreg_load _ ->
      ()

and stmt_callees d stmt =
  let expr_callees = expr_callees d in
  let stmt_callees = stmt_callees d in
  let (AnnotatedStatement (_, _, stmt)) = stmt in
  match stmt with
  | AilSskip | AilSbreak | AilScontinue | AilSreturnVoid | AilSgoto _ -> ()
  | AilSreturn e | AilSexpr e | AilSreg_store (_, e) -> expr_callees e
  | AilSblock (_, stmts) | AilSpar stmts -> List.iter stmt_callees stmts
  | AilSif (e, s1, s2) ->
      expr_callees e;
      stmt_callees s1;
      stmt_callees s2
  | AilSwhile (e, s, _) | AilSdo (s, e, _) | AilSswitch (e, s) ->
      stmt_callees s;
      expr_callees e
  | AilScase (_, s)
  | AilScase_rangeGNU (_, _, s)
  | AilSdefault s
  | AilSlabel (_, s, _)
  | AilSmarker (_, s) ->
      stmt_callees s
  | AilSdeclaration l ->
      List.iter (fun (_, e_opt) -> Option.iter expr_callees e_opt) l

let of_prog (prog : Ail_tys.sigma) : t =
  let tbl = Hashtbl.create 253 in
  let add_fonction_callees (f : Ail_tys.fundef) : unit =
    let id, (_, _, _, _, stmts) = f in
    if not (Hashtbl.mem tbl id) then (
      let callees = Sym_hashset.create 0 in
      stmt_callees callees stmts;
      Hashtbl.replace tbl id callees)
  in
  List.iter add_fonction_callees prog.function_definitions;
  tbl
