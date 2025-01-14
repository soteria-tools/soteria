open Cerb_frontend
open AilSyntax
open Bfa_std

include Graph.Make_in_place (struct
  type t = Symbol.sym

  let pp = Fmt_ail.pp_sym
  let hash = Hashtbl.hash
  let equal = Symbol.equal_sym
end)

(** A callgraph is just a hashtbl mapping caller to the list of their callees *)

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
      Node_set.add d fname
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
  let graph = with_node_capacity 253 in
  let add_fonction_callees (f : Ail_tys.fundef) : unit =
    let id, (_, _, _, _, stmts) = f in
    if not (Hashtbl.mem graph id) then (
      let callees = Node_set.with_capacity 0 in
      stmt_callees callees stmts;
      set_edges_from graph id callees)
  in
  List.iter add_fonction_callees prog.function_definitions;
  graph

(** A topological order where SCCs are not necessarily well-ordered *)
let weak_topological_order (cg : t) : Symbol.sym list =
  let cg_list =
    Hashtbl.to_seq cg
    |> Seq.map (fun (caller, callees) ->
           (caller, Node_set.to_seq callees |> List.of_seq))
    |> List.of_seq
  in
  let sorted_components = Tsort.sort_strongly_connected_components cg_list in
  (* We could order the components themselves a bit better, but let's ignore it for now. *)
  List.concat sorted_components
