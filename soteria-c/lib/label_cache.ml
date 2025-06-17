open Cerb_frontend
open Cerb_frontend.AilSyntax

type stmt = GenTypes.genTypeCategory statement
type label_map = (Symbol.sym * stmt) Dynarray.t
type t = (Symbol.sym, label_map) Hashtbl.t

module Sym_set = Symbol_std.Set

let rec label_map_expr lm expr : unit =
  let f = label_map_expr lm in
  let fs = label_map_stmt lm in
  let (AnnotatedExpression (_, _, _, expr)) = expr in
  match expr with
  | AilEva_start (e, _)
  | AilEva_arg (e, _)
  | AilEva_end e
  | AilEunary (_, e)
  | AilEcast (_, _, e)
  | AilEassert e
  | AilEcompound (_, _, e)
  | AilEannot (_, e)
  | AilEsizeof_expr e
  | AilEprint_type e
  | AilErvalue e
  | AilEarray_decay e
  | AilEfunction_decay e
  | AilEatomic e
  | AilEmemberof (e, _)
  | AilEmemberofptr (e, _)
  | AilEbmc_assume e ->
      f e
  | AilEbinary (e1, _, e2)
  | AilEassign (e1, e2)
  | AilEcompoundAssign (e1, _, e2)
  | AilEva_copy (e1, e2) ->
      f e1;
      f e2
  | AilEcond (e1, e2_opt, e3) ->
      f e1;
      Option.iter f e2_opt;
      f e3
  | AilEcall (e1, es) -> List.iter f (e1 :: es)
  | AilEstruct (_ty_sym, members) ->
      List.iter (fun (_, e_opt) -> Option.iter f e_opt) members
  | AilEarray (_, _, e_opts) -> List.iter (Option.iter f) e_opts
  | AilEunion (_ty_sym, _field, e_opt) -> Option.iter f e_opt
  | AilEgeneric (e, assocs) ->
      f e;
      List.iter (function AilGAtype (_, e) | AilGAdefault e -> f e) assocs
  | AilEgcc_statement (_, stmts) -> List.iter fs stmts
  | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEsizeof _ | AilEalignof _
  | AilEreg_load _ | AilEoffsetof _ | AilEinvalid _ | AilEident _ ->
      ()

and label_map_stmt lm stmt : unit =
  let f = label_map_stmt lm in
  let fe = label_map_expr lm in
  match stmt.node with
  | AilSlabel (lab, stmt, _) ->
      Dynarray.add_last lm (lab, stmt);
      label_map_stmt lm stmt
  | AilSexpr e | AilSreturn e | AilSreg_store (_, e) -> label_map_expr lm e
  | AilSwhile (e, stmt, _) | AilSdo (stmt, e, _) | AilSswitch (e, stmt) ->
      label_map_expr lm e;
      label_map_stmt lm stmt
  | AilScase (_, stmt)
  | AilScase_rangeGNU (_, _, stmt)
  | AilSdefault stmt
  | AilSmarker (_, stmt) (* CN stuff *) ->
      f stmt
  | AilSif (e, s1, s2) ->
      fe e;
      f s1;
      f s2
  | AilSdeclaration l -> List.iter (fun (_, e_opt) -> Option.iter fe e_opt) l
  | AilSblock (_, stmtl) | AilSpar stmtl -> List.iter f stmtl
  | AilSskip | AilSbreak | AilScontinue | AilSreturnVoid | AilSgoto _ -> ()

let label_map stmt : label_map =
  let tbl = Dynarray.create () in
  label_map_stmt tbl stmt;
  tbl

let find_target (tbl : t) fundef lab =
  let name, (_, _, _, _, stmt) = fundef in
  let lm =
    match Hashtbl.find_opt tbl name with
    | Some lm -> lm
    | None ->
        let lm = label_map stmt in
        Hashtbl.replace tbl name lm;
        lm
  in
  Dynarray.find_map
    (fun (l, v) -> if Symbol_std.equal l lab then Some v else None)
    lm
