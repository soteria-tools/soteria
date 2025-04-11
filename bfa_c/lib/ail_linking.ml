open Cerb_frontend
open AilSyntax
module Sym_set = Set.Make (Ail_helpers.Symbol_std)

type extern_idmap = (Symbol.identifier, sigma_extern_id) Pmap.map
type redundant_globs = (Symbol.sym * Symbol.sym) list

(* Ext2 should be the accumulator! *)
let link_extern (ext_cur : extern_idmap) (ext_oth : extern_idmap)
    (symmap : Ail_tys.extern_symmap) :
    (extern_idmap * redundant_globs * Ail_tys.extern_symmap, string) result =
  Pmap.fold
    (fun k (in_d, in_lk) acc ->
      let open Syntaxes.Result in
      let* acc_ext, acc_tent, symmap = acc in
      let symmap =
        match in_lk with
        | IK_declaration -> symmap
        | IK_definition | IK_tentative -> Pmap.add in_d in_d symmap
      in
      match Pmap.lookup k ext_cur with
      | None -> Ok (Pmap.add k (in_d, in_lk) acc_ext, acc_tent, symmap)
      | Some (cur_def, IK_declaration) ->
          Ok
            ( Pmap.add k (in_d, in_lk) acc_ext,
              acc_tent,
              Pmap.add cur_def in_d symmap )
      | Some (cur_tent, IK_tentative) -> (
          match in_lk with
          | IK_definition | IK_tentative ->
              Ok
                ( Pmap.add k (in_d, in_lk) acc_ext,
                  (cur_tent, in_d) :: acc_tent,
                  Pmap.add cur_tent in_d symmap )
          | IK_declaration ->
              Ok
                ( Pmap.add k (cur_tent, IK_tentative) acc_ext,
                  acc_tent,
                  Pmap.add in_d cur_tent symmap ))
      | Some (cur_def, IK_definition) -> (
          match in_lk with
          | IK_definition ->
              Error (Fmt.str "Duplicate external name %a" Fmt_ail.pp_id k)
          | IK_tentative ->
              Ok
                ( Pmap.add k (cur_def, IK_definition) acc_ext,
                  (in_d, cur_def) :: acc_tent,
                  Pmap.add in_d cur_def symmap )
          | IK_declaration ->
              Ok
                ( Pmap.add k (cur_def, IK_definition) acc_ext,
                  acc_tent,
                  Pmap.add in_d cur_def symmap )))
    ext_oth
    (Ok (ext_cur, [], symmap))

let set_of_bindings (bindings : bindings) =
  List.fold_left
    (fun acc (sym, _) -> Sym_set.add sym acc)
    Sym_set.empty bindings

let rec free_syms_expr acc expr =
  let (AnnotatedExpression (_, _, _, expr)) = expr in
  match expr with
  | AilEident sym -> Sym_set.add sym acc
  | AilEva_start (e, id) -> free_syms_expr (Sym_set.add id acc) e
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
      free_syms_expr acc e
  | AilEbinary (e1, _, e2)
  | AilEassign (e1, e2)
  | AilEcompoundAssign (e1, _, e2)
  | AilEva_copy (e1, e2) ->
      free_syms_expr (free_syms_expr acc e1) e2
  | AilEcond (e1, e2_opt, e3) ->
      let acc = free_syms_expr (free_syms_expr acc e1) e3 in
      Option.fold ~none:acc ~some:(free_syms_expr acc) e2_opt
  | AilEcall (e1, es) -> List.fold_left free_syms_expr acc (e1 :: es)
  | AilEstruct (_ty_sym, members) ->
      List.fold_left
        (fun acc (_, e) -> Option.fold ~none:acc ~some:(free_syms_expr acc) e)
        acc members
  | AilEarray (_, _, eopts) ->
      List.fold_left
        (fun acc e_opt ->
          Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt)
        acc eopts
  | AilEunion (_ty_sym, _field, e_opt) ->
      Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt
  | AilEgeneric (e, assocs) ->
      let acc = free_syms_expr acc e in
      List.fold_left
        (fun acc assoc ->
          match assoc with
          | AilGAtype (_, e) | AilGAdefault e -> free_syms_expr acc e)
        acc assocs
  | AilEgcc_statement (bindings, stmts) ->
      let exclude = set_of_bindings bindings in
      let res = List.fold_left free_syms_stmt acc stmts in
      Sym_set.diff res exclude
  | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEsizeof _ | AilEalignof _
  | AilEreg_load _ | AilEoffsetof _ ->
      acc

and free_syms_stmt acc stmt =
  let (AnnotatedStatement (_, _, stmt)) = stmt in
  match stmt with
  | AilSexpr e | AilSreturn e | AilSreg_store (_, e) -> free_syms_expr acc e
  | AilSwhile (e, stmt, _) | AilSdo (stmt, e, _) | AilSswitch (e, stmt) ->
      free_syms_stmt (free_syms_expr acc e) stmt
  | AilScase (_, stmt)
  | AilScase_rangeGNU (_, _, stmt)
  | AilSdefault stmt
  | AilSmarker (_, stmt) (* CN stuff *)
  | AilSlabel (_, stmt, _) ->
      free_syms_stmt acc stmt
  | AilSif (e, s1, s2) ->
      let acc = free_syms_expr acc e in
      let acc = free_syms_stmt acc s1 in
      free_syms_stmt acc s2
  | AilSskip | AilSbreak | AilScontinue | AilSreturnVoid | AilSgoto _ -> acc
  | AilSdeclaration l ->
      List.fold_left
        (fun acc (_, e_opt) ->
          Option.fold ~none:acc ~some:(free_syms_expr acc) e_opt)
        acc l
  | AilSblock (bindings, stmtl) ->
      let exclude = set_of_bindings bindings in
      let res = List.fold_left free_syms_stmt acc stmtl in
      Sym_set.diff res exclude
  | AilSpar stmtl -> List.fold_left free_syms_stmt acc stmtl

let free_syms_object_definition acc (_, expr) = free_syms_expr acc expr

let link_main opt_m1 opt_m2 =
  match (opt_m1, opt_m2) with
  | Some _, Some _ -> Error "linking: multiple main functions"
  | Some m, None | None, Some m -> Ok (Some m)
  | None, None -> Ok None

let has_cn_stuff (sigma : 'a sigma) =
  match sigma with
  | {
   cn_functions = [];
   cn_lemmata = [];
   cn_predicates = [];
   cn_decl_specs = [];
   cn_idents;
   _;
  }
    when Pmap.is_empty cn_idents ->
      false
  | _ -> true

let link_aux ((m1, f1) : 'a ail_program) ((m2, f2) : 'a ail_program)
    (symmap : Ail_tys.extern_symmap) : (Ail_tys.linked_program, string) result =
  let open Syntaxes.Result in
  let* () =
    if has_cn_stuff f1 || has_cn_stuff f2 then
      Result.error "linking: CN stuff not supported"
    else Ok ()
  in
  let* m = link_main m1 m2 in
  (* TODO: figure out what to do with redundant_globs *)
  (* TODO: it should be some kind of merge_globs where we iterate through all
           decl_obj declarations and then remove redundancies from both declarations
           and object_definitions *)
  let+ extern_idmap, _redundant_globs, symmap =
    link_extern f1.extern_idmap f2.extern_idmap symmap
  in
  Fmt.pr "Redudant globs: @[<v 2>%a@]@\n"
    (Fmt.list ~sep:Fmt.cut
       (Fmt.hbox (Fmt.pair ~sep:(Fmt.any " -> ") Fmt_ail.pp_sym Fmt_ail.pp_sym)))
    _redundant_globs;
  Ail_tys.
    {
      sigma =
        {
          object_definitions = f1.object_definitions @ f2.object_definitions;
          function_definitions =
            f1.function_definitions @ f2.function_definitions;
          declarations = f1.declarations @ f2.declarations;
          tag_definitions = f1.tag_definitions @ f2.tag_definitions;
          extern_idmap;
          typedef_attributes =
            Pmap.union f1.typedef_attributes f2.typedef_attributes;
          loop_attributes = Pmap.union f1.loop_attributes f2.loop_attributes;
          static_assertions = f1.static_assertions @ f2.static_assertions;
          cn_idents = f1.cn_idents;
          cn_functions = [];
          cn_predicates = [];
          cn_lemmata = [];
          cn_datatypes = [];
          cn_decl_specs = [];
        };
      entry_point = m;
      symmap;
    }

let link : Ail_tys.program list -> (Ail_tys.linked_program, string) result =
  let open Ail_tys in
  function
  | [] -> Result.error "linking: no ail files"
  | (entry_point, sigma) :: fs ->
      let symmap =
        Pmap.fold
          (fun _ (d, lk) acc ->
            match lk with
            | IK_definition | IK_tentative -> Pmap.add d d acc
            | IK_declaration -> acc)
          sigma.extern_idmap
          (Pmap.empty Symbol.compare_sym)
      in
      let init_linked = { entry_point; sigma; symmap } in
      List.fold_left
        (fun acc f' ->
          Result.bind acc
          @@
          fun {
                entry_point = entry_point_acc;
                sigma = sigma_acc;
                symmap = symmap_acc;
              }
          -> link_aux (entry_point_acc, sigma_acc) f' symmap_acc)
        (Ok init_linked) fs

let link progs =
  Result.map_error (fun s -> (`LinkError s, Call_trace.empty)) (link progs)
