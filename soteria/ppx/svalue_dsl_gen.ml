(** Code generation for the [%%svalue] DSL: turns a {!Svalue_dsl_ast.program}
    into the hash-consed value-language structure items. *)

open Ppxlib
open Svalue_dsl_ast
module B = Ast_builder.Default

(* The whole program, stashed so deep helpers can recover [literal] decls
   (printers, payload types) without threading them everywhere. *)
let ctx_program_ref : program ref = ref []

let efunction ~loc cases = B.pexp_function_cases ~loc cases

(* {2 Small AST helpers} *)

let lid ~loc s : Longident.t Loc.t =
  let parts = String.split_on_char '.' s in
  let l =
    match parts with
    | [] -> assert false
    | x :: xs -> List.fold_left (fun acc p -> Longident.Ldot (acc, p)) (Lident x) xs
  in
  { txt = l; loc }

let evar ~loc s = B.pexp_ident ~loc (lid ~loc s)
let pvar ~loc s = B.ppat_var ~loc { txt = s; loc }

let econstr ~loc name args =
  let arg =
    match args with
    | [] -> None
    | [ a ] -> Some a
    | l -> Some (B.pexp_tuple ~loc l)
  in
  B.pexp_construct ~loc (lid ~loc name) arg

let pconstr ~loc name pats =
  let arg =
    match pats with
    | [] -> None
    | [ p ] -> Some p
    | l -> Some (B.ppat_tuple ~loc l)
  in
  B.ppat_construct ~loc (lid ~loc name) arg

let eapply ~loc f args = B.eapply ~loc f args

(* pattern [{ node = { kind = <p>; _ }; _ }] *)
let pat_kind ~loc p =
  B.ppat_record ~loc
    [
      ( lid ~loc "node",
        B.ppat_record ~loc [ (lid ~loc "kind", p) ] Open );
    ]
    Open

(* expression [e.tag] *)
let etag ~loc e = B.pexp_field ~loc e (lid ~loc "tag")

(* {2 Context built from the program} *)

type lit_info = {
  li_kind : string;  (** kind constructor, e.g. [Int] *)
  li_ty : string;  (** ty name, e.g. [Int] *)
  li_ctor : string;  (** term constructor, e.g. [int_z] *)
  li_pay : string;  (** printed payload type, e.g. ["Z.t"] *)
}

type ctx = {
  tys : string list;
  ty_args : (string * Ppxlib.core_type list) list;
  leaves : leaf_decl list;
  lits : lit_info list;
  ops : op_decl list;
  nops : nop_decl list;
  by_name : (string, op_decl) Hashtbl.t;
  by_sym : (string, op_decl) Hashtbl.t;
  by_ctor : (string, op_decl) Hashtbl.t;  (** raw-construction RHS *)
  leaf_names : string list;
  aux : Ppxlib.structure list;
  lit_by_ty : (string, lit_info) Hashtbl.t;
  sorts : sort_decl list;
  ty_sort : (string, string) Hashtbl.t;  (** ty -> default sort *)
  sort_base : (string, string) Hashtbl.t;  (** sort -> runtime ty *)
  op_sorts : (string, string list * string) Hashtbl.t;  (** name -> sig sorts *)
}

(* The runtime [ty] behind a signature token, which may be a [ty] name or a
   [sort] refining one (e.g. [nonzero] -> [Int]). *)
let runtime_ty ctx s =
  match Hashtbl.find_opt ctx.sort_base s with Some t -> t | None -> s

let payload_string ct =
  Format.asprintf "%a" Pprintast.core_type ct |> String.trim

let build_ctx (p : program) : ctx =
  let tys = ref [] and leaves = ref [] and lits = ref [] in
  let ops = ref [] and nops = ref [] and aux = ref [] in
  let sorts = ref [] and ty_sort_l = ref [] and ty_args = ref [] in
  List.iter
    (function
      | DAux s -> aux := !aux @ [ s ]
      | DSort s -> sorts := !sorts @ [ s ]
      | DTy t ->
          tys := t.ty_variants;
          ty_sort_l := t.ty_sorts;
          ty_args := t.ty_args
      | DLeaf l -> leaves := !leaves @ [ l ]
      | DLiteral l ->
          let li_ctor =
            match l.lit_ctor with
            | Some c -> c
            | None -> "mk_" ^ String.lowercase_ascii l.lit_name
          in
          lits :=
            !lits
            @ [
                {
                  li_kind = l.lit_name;
                  li_ty = l.lit_ty;
                  li_ctor;
                  li_pay = payload_string l.lit_payload;
                };
              ]
      | DOp o -> ops := !ops @ [ o ]
      | DNop n -> nops := !nops @ [ n ])
    p;
  (* Typed signatures keep the original (sort) tokens; everything else uses
     the runtime [ty] behind them. *)
  let op_sorts = Hashtbl.create 16 in
  let sort_base0 = Hashtbl.create 8 in
  List.iter
    (fun (s : sort_decl) ->
      Option.iter
        (fun b -> Hashtbl.replace sort_base0 s.sort_name b)
        s.sort_base)
    !sorts;
  let rt s =
    match Hashtbl.find_opt sort_base0 s with Some t -> t | None -> s
  in
  ops :=
    List.map
      (fun o ->
        Hashtbl.replace op_sorts o.op_name (o.op_args, o.op_ret);
        {
          o with
          op_args = List.map rt o.op_args;
          op_ret = rt o.op_ret;
        })
      !ops;
  let by_name = Hashtbl.create 16
  and by_sym = Hashtbl.create 16
  and by_ctor = Hashtbl.create 16 in
  List.iter
    (fun o ->
      Hashtbl.replace by_name o.op_name o;
      Hashtbl.replace by_ctor o.op_ctor o;
      Option.iter (fun s -> Hashtbl.replace by_sym s o) o.op_symbol)
    !ops;
  let lit_by_ty = Hashtbl.create 16 in
  List.iter (fun l -> Hashtbl.replace lit_by_ty l.li_ty l) !lits;
  let ty_sort = Hashtbl.create 8 and sort_base = Hashtbl.create 8 in
  List.iter (fun (v, s) -> Hashtbl.replace ty_sort v s) !ty_sort_l;
  List.iter
    (fun (s : sort_decl) ->
      Option.iter (fun b -> Hashtbl.replace sort_base s.sort_name b) s.sort_base)
    !sorts;
  {
    tys = !tys;
    ty_args = !ty_args;
    leaves = !leaves;
    lits = !lits;
    ops = !ops;
    nops = !nops;
    by_name;
    by_sym;
    by_ctor;
    leaf_names = List.map (fun (l : leaf_decl) -> l.leaf_name) !leaves;
    aux = !aux;
    lit_by_ty;
    sorts = !sorts;
    ty_sort;
    sort_base;
    op_sorts;
  }

let ty_ctor name = "T" ^ name
let arity o = List.length o.op_args
let is_ite o = o.op_ctor = "Ite"

let resolve_op ctx name =
  match Hashtbl.find_opt ctx.by_name name with
  | Some o -> Some o
  | None -> Hashtbl.find_opt ctx.by_sym name

let conv_int ~loc pay n =
  match pay with
  | "Z.t" -> eapply ~loc (evar ~loc "Z.of_int") [ B.eint ~loc n ]
  | "int" -> B.eint ~loc n
  | _ -> B.eint ~loc n

(* {2 Rule compilation} *)

(* Whether [name]/[#] expr denotes a literal-payload-level value. *)
let rec lit_level ctx env = function
  | ELit _ -> true
  | EInt _ -> true
  | EBool _ -> true
  | EVar (x, _) -> ( match List.assoc_opt x env with Some `Lit -> true | _ -> false)
  | EApp (op, _params, args, _) -> (
      match resolve_op ctx op with
      | Some o ->
          Option.is_some o.op_fold && List.for_all (lit_level ctx env) args
      | None -> false)
  | EWild _ | EOcaml _ -> false

(* Collect the binding kind (term / literal / ctor-param) of LHS variables. *)
let rec collect_env acc = function
  | EVar (x, _) -> if List.mem_assoc x acc then acc else (x, `Term) :: acc
  | ELit (x, _, _) -> if List.mem_assoc x acc then acc else (x, `Lit) :: acc
  | EApp (_, params, args, _) ->
      let acc =
        List.fold_left
          (fun acc -> function
            | EVar (x, _) when not (List.mem_assoc x acc) -> (x, `Param) :: acc
            | _ -> acc)
          acc params
      in
      List.fold_left collect_env acc args
  | EWild _ | EBool _ | EInt _ | EOcaml _ -> acc

(* Compile a LHS sub-expression matched against a whole [t] value.
   Returns (pattern, guards) and mutates [seen] for repeat-variable guards. *)
let rec compile_pat ctx ~loc ~expect_ty ~seen e :
    pattern * expression list =
  match e with
  | EWild _ -> (B.ppat_any ~loc, [])
  | EVar (x, _) ->
      if List.mem x !seen then (
        let fresh = x ^ "__r" ^ string_of_int (List.length !seen) in
        seen := fresh :: !seen;
        (pvar ~loc fresh, [ eapply ~loc (evar ~loc "equal") [ evar ~loc x; evar ~loc fresh ] ]))
      else (
        seen := x :: !seen;
        (pvar ~loc x, []))
  | ELit (x, kindopt, _) ->
      let li =
        match kindopt with
        | Some k -> (
            match List.find_opt (fun l -> l.li_kind = k || l.li_ty = k) ctx.lits with
            | Some l -> l
            | None -> Location.raise_errorf ~loc "unknown literal kind %s" k)
        | None -> (
            match expect_ty with
            | Some ty -> (
                match Hashtbl.find_opt ctx.lit_by_ty ty with
                | Some l -> l
                | None ->
                    Location.raise_errorf ~loc
                      "no literal kind for ty %s (annotate #%s:Kind)" ty x)
            | None ->
                Location.raise_errorf ~loc
                  "cannot infer literal kind for #%s (annotate #%s:Kind)" x x)
      in
      (pat_kind ~loc (pconstr ~loc li.li_kind [ pvar ~loc x ]), [])
  | EBool (b, _) ->
      let bk =
        match List.find_opt (fun l -> l.li_pay = "bool") ctx.lits with
        | Some l -> l.li_kind
        | None -> "Bool"
      in
      ( pat_kind ~loc
          (pconstr ~loc bk
             [ B.ppat_construct ~loc (lid ~loc (string_of_bool b)) None ]),
        [] )
  | EInt (n, _) ->
      let li =
        match expect_ty with
        | Some ty -> Hashtbl.find_opt ctx.lit_by_ty ty
        | None -> None
      in
      let li =
        match li with
        | Some l -> l
        | None -> (
            match ctx.lits with
            | l :: _ -> l
            | [] -> Location.raise_errorf ~loc "no literal kind declared")
      in
      let fresh = "c__" ^ string_of_int (List.length !seen) in
      seen := fresh :: !seen;
      ( pvar ~loc fresh,
        [
          eapply ~loc (evar ~loc "equal")
            [ evar ~loc fresh; eapply ~loc (evar ~loc li.li_ctor) [ conv_int ~loc li.li_pay n ] ];
        ] )
  | EApp (lname, [], [ arg ], _) when List.mem lname ctx.leaf_names ->
      (* leaf-constructor pattern, e.g. [Var a] binds the payload *)
      let p, g = compile_pat ctx ~loc ~expect_ty:None ~seen arg in
      let p = match p with { ppat_desc = Ppat_record _; _ } -> B.ppat_any ~loc | _ -> p in
      (pat_kind ~loc (pconstr ~loc lname [ p ]), g)
  | EApp (opname, params, args, _) -> (
      match resolve_op ctx opname with
      | None -> Location.raise_errorf ~loc "unknown operator %s in pattern" opname
      | Some o ->
          let ppats =
            List.map
              (function
                | EWild _ -> B.ppat_any ~loc
                | EVar (x, _) -> pvar ~loc x
                | _ ->
                    Location.raise_errorf ~loc
                      "constructor params must be variables or '_' in patterns")
              params
          in
          let cpat = pconstr ~loc o.op_ctor ppats in
          let subpats, subguards =
            List.split
              (List.map2
                 (fun a ty ->
                   compile_pat ctx ~loc ~expect_ty:(Some ty) ~seen a)
                 args o.op_args)
          in
          let guards = List.concat subguards in
          let kpat =
            if is_ite o then pconstr ~loc "Ite" subpats
            else if arity o = 1 then
              pconstr ~loc "Unop" [ cpat; List.hd subpats ]
            else pconstr ~loc "Binop" (cpat :: subpats)
          in
          (pat_kind ~loc kpat, guards))
  | EOcaml _ -> Location.raise_errorf ~loc "{{ ... }} not allowed in a rule LHS"

(* Term constructor that wraps a literal payload for ty [ty]. *)
let lit_ctor_for ctx ty =
  match Hashtbl.find_opt ctx.lit_by_ty ty with Some l -> Some l.li_ctor | None -> None

let rec emit_term ctx ~loc ~env ~expect_ty e : expression =
  match e with
  | EWild _ -> Location.raise_errorf ~loc "'_' not allowed in a rule RHS"
  | EOcaml ex -> ex
  | EVar (x, _) -> (
      match List.assoc_opt x env with
      | Some `Lit -> (
          match Option.bind expect_ty (lit_ctor_for ctx) with
          | Some c -> eapply ~loc (evar ~loc c) [ evar ~loc x ]
          | None -> evar ~loc x)
      | _ -> evar ~loc x)
  | ELit (x, _, _) -> (
      match Option.bind expect_ty (lit_ctor_for ctx) with
      | Some c -> eapply ~loc (evar ~loc c) [ evar ~loc x ]
      | None -> evar ~loc x)
  | EBool (b, _) -> eapply ~loc (evar ~loc "of_bool") [ B.ebool ~loc b ]
  | EInt (n, _) -> (
      match Option.bind expect_ty (Hashtbl.find_opt ctx.lit_by_ty) with
      | Some li -> eapply ~loc (evar ~loc li.li_ctor) [ conv_int ~loc li.li_pay n ]
      | None -> (
          match ctx.lits with
          | li :: _ -> eapply ~loc (evar ~loc li.li_ctor) [ conv_int ~loc li.li_pay n ]
          | [] -> B.eint ~loc n))
  | EApp (opname, params, args, _) when Hashtbl.mem ctx.by_ctor opname ->
      (* Raw construction: build the operator node directly (no
         re-simplification), exactly like [Binop (Leq, b, a) <| TBool]. *)
      let o = Hashtbl.find ctx.by_ctor opname in
      let pes = List.map (emit_param ctx ~loc ~env) params in
      let aes =
        List.map2
          (fun a ty -> emit_term ctx ~loc ~env ~expect_ty:(Some ty) a)
          args o.op_args
      in
      let ty_e =
        if o.op_ret = "any" then
          let idx =
            let rec f i = function
              | "any" :: _ -> i
              | _ :: tl -> f (i + 1) tl
              | [] -> 0
            in
            f 0 o.op_args
          in
          B.pexp_field ~loc
            (B.pexp_field ~loc (List.nth aes idx) (lid ~loc "node"))
            (lid ~loc "ty")
        else econstr ~loc (ty_ctor o.op_ret) []
      in
      let cexp = econstr ~loc o.op_ctor pes in
      let kind =
        if is_ite o then econstr ~loc "Ite" aes
        else if arity o = 1 then econstr ~loc "Unop" [ cexp; List.hd aes ]
        else if o.op_commutative then
          eapply ~loc (evar ~loc "mk_commut_binop") (cexp :: aes)
        else econstr ~loc "Binop" (cexp :: aes)
      in
      eapply ~loc (evar ~loc "<|") [ kind; ty_e ]
  | EApp (opname, params, args, _) -> (
      match resolve_op ctx opname with
      | None -> Location.raise_errorf ~loc "unknown operator %s in RHS" opname
      | Some o ->
          if
            params = []
            && Option.is_some o.op_fold
            && List.for_all (lit_level ctx env) args
          then
            let fold = Option.get o.op_fold in
            let folded =
              eapply ~loc fold (List.map (emit_lit ctx ~loc ~env) args)
            in
            match lit_ctor_for ctx o.op_ret with
            | Some c -> eapply ~loc (evar ~loc c) [ folded ]
            | None -> folded
          else
            eapply ~loc (evar ~loc o.op_name)
              (List.map (emit_param ctx ~loc ~env) params
              @ List.map2
                  (fun a ty -> emit_term ctx ~loc ~env ~expect_ty:(Some ty) a)
                  args o.op_args))

and emit_param _ctx ~loc ~env:_ e : expression =
  match e with
  | EVar (x, _) -> evar ~loc x
  | EInt (n, _) -> B.eint ~loc n
  | EBool (b, _) -> B.ebool ~loc b
  | EOcaml ex -> ex
  | _ -> Location.raise_errorf ~loc "invalid constructor parameter"

and emit_lit ctx ~loc ~env e : expression =
  match e with
  | ELit (x, _, _) | EVar (x, _) -> evar ~loc x
  | EInt (n, _) ->
      let pay =
        match ctx.lits with l :: _ -> l.li_pay | [] -> "int"
      in
      conv_int ~loc pay n
  | EBool (b, _) -> B.ebool ~loc b
  | EOcaml ex -> ex
  | EApp (opname, _, args, _) -> (
      match resolve_op ctx opname with
      | Some { op_fold = Some fold; _ } ->
          eapply ~loc fold (List.map (emit_lit ctx ~loc ~env) args)
      | _ -> Location.raise_errorf ~loc "non-foldable operator %s at literal level" opname)
  | EWild _ -> Location.raise_errorf ~loc "'_' not allowed at literal level"

let rec compile_guard ctx ~loc ~env g : expression =
  match g with
  | GOcaml e -> e
  | GAnd (a, b) ->
      eapply ~loc (evar ~loc "&&")
        [ compile_guard ctx ~loc ~env a; compile_guard ctx ~loc ~env b ]
  | GCmp (cmp, a, b) ->
      let ea = emit_term ctx ~loc ~env ~expect_ty:None a in
      let eb = emit_term ctx ~loc ~env ~expect_ty:None b in
      let eq = eapply ~loc (evar ~loc "equal") [ ea; eb ] in
      if cmp = `Eq then eq else eapply ~loc (evar ~loc "Stdlib.not") [ eq ]

let and_guards ~loc gs =
  match gs with
  | [] -> None
  | g :: tl ->
      Some
        (List.fold_left
           (fun acc g -> eapply ~loc (evar ~loc "&&") [ acc; g ])
           g tl)

(* One match arm from a user rule. [swap] mirrors a binary commutative op. *)
let rule_arm ctx ~loc o ~swap (r : rule) : case option =
  let args =
    match r.r_lhs with
    (* top-level ctor params are the smart constructor's own named arguments
       (bound by their declared names), so they are ignored here *)
    | EApp (opn, _params, args, _) when resolve_op ctx opn = Some o -> args
    | _ ->
        Location.raise_errorf ~loc
          "rule LHS must be an application of operator '%s'" o.op_name
  in
  let args = if swap then List.rev args else args in
  let tys = if swap then List.rev o.op_args else o.op_args in
  let seen = ref [] in
  let pats, guards =
    List.split
      (List.map2
         (fun a ty -> compile_pat ctx ~loc ~expect_ty:(Some ty) ~seen a)
         args tys)
  in
  let env =
    List.map (fun (n, _) -> (n, `Param)) o.op_params @ collect_env [] r.r_lhs
  in
  let guards = List.concat guards in
  let guards =
    match r.r_guard with
    | None -> guards
    | Some g -> guards @ [ compile_guard ctx ~loc ~env g ]
  in
  let lhs_pat =
    match pats with [ p ] -> p | l -> B.ppat_tuple ~loc l
  in
  let rhs = emit_term ctx ~loc ~env ~expect_ty:(Some o.op_ret) r.r_rhs in
  Some (B.case ~lhs:lhs_pat ~guard:(and_guards ~loc guards) ~rhs)

(* {2 Derived structure items} *)

let ty_decl ~loc ctx =
  let cds =
    List.map
      (fun v ->
        let ctys =
          match List.assoc_opt v ctx.ty_args with Some l -> l | None -> []
        in
        B.constructor_declaration ~loc ~name:{ txt = ty_ctor v; loc }
          ~args:(Pcstr_tuple ctys) ~res:None)
      ctx.tys
  in
  let td =
    {
      (B.type_declaration ~loc ~name:{ txt = "ty"; loc } ~params:[] ~cstrs:[]
         ~kind:(Ptype_variant cds) ~private_:Public ~manifest:None)
      with
      ptype_attributes =
        [
          B.attribute ~loc ~name:{ txt = "deriving"; loc }
            ~payload:
              (PStr
                 [
                   B.pstr_eval ~loc
                     [%expr eq, show { with_path = false }, ord]
                     [];
                 ]);
        ];
    }
  in
  B.pstr_type ~loc Recursive [ td ]

let ty_helpers ~loc ctx =
  let aliases =
    List.filter_map
      (fun v ->
        if List.mem_assoc v ctx.ty_args then None (* parameterized: no alias *)
        else
          Some
            [%stri
              let [%p pvar ~loc ("t_" ^ String.lowercase_ascii v)] =
                [%e econstr ~loc (ty_ctor v) []]])
      ctx.tys
  in
  let is_bool =
    if List.mem "Bool" ctx.tys then
      [
        [%stri
          let is_bool_ty = function
            | [%p pconstr ~loc (ty_ctor "Bool") []] -> true
            | _ -> false];
      ]
    else []
  in
  aliases @ is_bool

(* operator variant module ([Unop] / [Binop] / [Nop]) *)
(* [defs]: (ctor, payload core types, pp string) *)
let op_module ~loc ~name defs =
  if defs = [] then []
  else
    let cds =
      List.map
        (fun (c, ctys, _) ->
          B.constructor_declaration ~loc ~name:{ txt = c; loc }
            ~args:(Pcstr_tuple ctys) ~res:None)
        defs
    in
    let td =
      {
        (B.type_declaration ~loc ~name:{ txt = "t"; loc } ~params:[] ~cstrs:[]
           ~kind:(Ptype_variant cds) ~private_:Public ~manifest:None)
        with
        ptype_attributes =
          [
            B.attribute ~loc ~name:{ txt = "deriving"; loc }
              ~payload:
                (PStr [ B.pstr_eval ~loc [%expr eq, ord] [] ]);
          ];
      }
    in
    let pp_cases =
      List.map
        (fun (c, ctys, s) ->
          B.case
            ~lhs:
              (pconstr ~loc c
                 (if ctys = [] then [] else [ B.ppat_any ~loc ]))
            ~guard:None
            ~rhs:(eapply ~loc (evar ~loc "Fmt.string") [ evar ~loc "ft"; B.estring ~loc s ]))
        defs
    in
    let pp = [%stri let pp ft = [%e efunction ~loc pp_cases]] in
    [
      B.pstr_module ~loc
        (B.module_binding ~loc
           ~name:{ txt = Some name; loc }
           ~expr:
             (B.pmod_structure ~loc [ B.pstr_type ~loc Recursive [ td ]; pp ]));
    ]

(* the recursive hash-consed kind type *)
let kind_type ~loc ctx =
  let leaf_cd l =
    B.constructor_declaration ~loc ~name:{ txt = l.leaf_name; loc }
      ~args:(Pcstr_tuple [ l.leaf_payload ]) ~res:None
  in
  let lit_cd (l : lit_info) (decl : literal_decl) =
    let cd =
      B.constructor_declaration ~loc ~name:{ txt = l.li_kind; loc }
        ~args:(Pcstr_tuple [ decl.lit_payload ]) ~res:None
    in
    match decl.lit_print with
    | None -> cd
    | Some pr ->
        {
          cd with
          pcd_attributes =
            [
              B.attribute ~loc ~name:{ txt = "printer"; loc }
                ~payload:(PStr [ B.pstr_eval ~loc pr [] ]);
            ];
        }
  in
  let lit_decls =
    List.filter_map
      (function DLiteral d -> Some d | _ -> None)
      !ctx_program_ref
  in
  let find_decl name = List.find (fun d -> d.lit_name = name) lit_decls in
  let t = [%type: t] in
  let tlist = [%type: t list] in
  let leaf_cds = List.map leaf_cd ctx.leaves in
  let lit_cds = List.map (fun li -> lit_cd li (find_decl li.li_kind)) ctx.lits in
  let unop_cd =
    B.constructor_declaration ~loc ~name:{ txt = "Unop"; loc }
      ~args:(Pcstr_tuple [ [%type: Unop.t]; t ]) ~res:None
  in
  let binop_cd =
    B.constructor_declaration ~loc ~name:{ txt = "Binop"; loc }
      ~args:(Pcstr_tuple [ [%type: Binop.t]; t; t ]) ~res:None
  in
  let nop_cd =
    B.constructor_declaration ~loc ~name:{ txt = "Nop"; loc }
      ~args:(Pcstr_tuple [ [%type: Nop.t]; tlist ]) ~res:None
  in
  let ite_cd =
    B.constructor_declaration ~loc ~name:{ txt = "Ite"; loc }
      ~args:(Pcstr_tuple [ t; t; t ]) ~res:None
  in
  let has_un = List.exists (fun o -> arity o = 1) ctx.ops in
  let has_bin = List.exists (fun o -> arity o = 2 && not (is_ite o)) ctx.ops in
  let has_nop = ctx.nops <> [] in
  let has_ite = List.exists is_ite ctx.ops in
  let kind_cds =
    leaf_cds @ lit_cds
    @ (if has_un then [ unop_cd ] else [])
    @ (if has_bin then [ binop_cd ] else [])
    @ (if has_nop then [ nop_cd ] else [])
    @ if has_ite then [ ite_cd ] else []
  in
  let kind_td =
    B.type_declaration ~loc ~name:{ txt = "t_kind"; loc } ~params:[] ~cstrs:[]
      ~kind:(Ptype_variant kind_cds) ~private_:Public ~manifest:None
  in
  let node_td =
    B.type_declaration ~loc ~name:{ txt = "t_node"; loc } ~params:[] ~cstrs:[]
      ~kind:
        (Ptype_record
           [
             B.label_declaration ~loc ~name:{ txt = "kind"; loc } ~mutable_:Immutable
               ~type_:[%type: t_kind];
             B.label_declaration ~loc ~name:{ txt = "ty"; loc } ~mutable_:Immutable
               ~type_:[%type: ty];
           ])
      ~private_:Public ~manifest:None
  in
  let t_td =
    {
      (B.type_declaration ~loc ~name:{ txt = "t"; loc } ~params:[] ~cstrs:[]
         ~kind:Ptype_abstract ~private_:Public
         ~manifest:(Some [%type: t_node hash_consed]))
      with
      ptype_attributes =
        [
          B.attribute ~loc ~name:{ txt = "deriving"; loc }
            ~payload:
              (PStr
                 [
                   B.pstr_eval ~loc [%expr show { with_path = false }, eq, ord] [];
                 ]);
        ];
    }
  in
  B.pstr_type ~loc Recursive [ kind_td; node_td; t_td ]

(* {2 Smart constructors} *)

let result_ty_expr ~loc _ctx o =
  if o.op_ret = "any" then
    (* take the ty of the first [any] argument (e.g. the [if_] of an ite) *)
    let idx =
      let rec find i = function
        | "any" :: _ -> i
        | _ :: tl -> find (i + 1) tl
        | [] -> 1
      in
      find 0 o.op_args
    in
    B.pexp_field ~loc
      (B.pexp_field ~loc (evar ~loc (Printf.sprintf "__a%d" idx)) (lid ~loc "node"))
      (lid ~loc "ty")
  else econstr ~loc (ty_ctor o.op_ret) []

let ctor_params_e ~loc o =
  List.map (fun (nm, _) -> evar ~loc nm) o.op_params

let default_expr ~loc ctx o =
  let n = arity o in
  let cexp = econstr ~loc o.op_ctor (ctor_params_e ~loc o) in
  let kind =
    if is_ite o then
      econstr ~loc "Ite"
        (List.init n (fun i -> evar ~loc (Printf.sprintf "__a%d" i)))
    else if n = 1 then econstr ~loc "Unop" [ cexp; evar ~loc "__a0" ]
    else if o.op_commutative then
      eapply ~loc (evar ~loc "mk_commut_binop")
        [ cexp; evar ~loc "__a0"; evar ~loc "__a1" ]
    else econstr ~loc "Binop" [ cexp; evar ~loc "__a0"; evar ~loc "__a1" ]
  in
  eapply ~loc (evar ~loc "<|") [ kind; result_ty_expr ~loc ctx o ]

(* synthetic arms from declared algebraic properties *)
let derived_arms ~loc ctx o : case list =
  let n = arity o in
  let arms = ref [] in
  let add c = arms := c :: !arms in
  let lit_eq operand z =
    match Hashtbl.find_opt ctx.lit_by_ty (List.nth o.op_args 0) with
    | Some li ->
        eapply ~loc (evar ~loc "equal")
          [ operand; eapply ~loc (evar ~loc li.li_ctor) [ conv_int ~loc li.li_pay z ] ]
    | None -> [%expr false]
  in
  if o.op_involutive && n = 1 then
    add
      (B.case
         ~lhs:
           (pat_kind ~loc
              (pconstr ~loc "Unop"
                 [
                   pconstr ~loc o.op_ctor
                     (List.map (fun _ -> B.ppat_any ~loc) o.op_params);
                   pvar ~loc "__x";
                 ]))
         ~guard:None ~rhs:(evar ~loc "__x"));
  if o.op_idempotent && n = 2 then
    add
      (B.case
         ~lhs:(B.ppat_tuple ~loc [ pvar ~loc "__a0"; pvar ~loc "__b" ])
         ~guard:(Some (eapply ~loc (evar ~loc "equal") [ evar ~loc "__a0"; evar ~loc "__b" ]))
         ~rhs:(evar ~loc "__a0"));
  (match o.op_identity with
  | Some z when n = 2 ->
      add
        (B.case
           ~lhs:(B.ppat_tuple ~loc [ pvar ~loc "__a0"; pvar ~loc "__b" ])
           ~guard:(Some (lit_eq (evar ~loc "__b") z))
           ~rhs:(evar ~loc "__a0"));
      add
        (B.case
           ~lhs:(B.ppat_tuple ~loc [ pvar ~loc "__a0"; pvar ~loc "__b" ])
           ~guard:(Some (lit_eq (evar ~loc "__a0") z))
           ~rhs:(evar ~loc "__b"))
  | _ -> ());
  (match o.op_absorbing with
  | Some z when n = 2 ->
      let zexpr =
        match Hashtbl.find_opt ctx.lit_by_ty o.op_ret with
        | Some li -> eapply ~loc (evar ~loc li.li_ctor) [ conv_int ~loc li.li_pay z ]
        | None -> [%expr assert false]
      in
      add
        (B.case
           ~lhs:(B.ppat_tuple ~loc [ pvar ~loc "__a0"; pvar ~loc "__b" ])
           ~guard:
             (Some
                (eapply ~loc (evar ~loc "||")
                   [ lit_eq (evar ~loc "__a0") z; lit_eq (evar ~loc "__b") z ]))
           ~rhs:zexpr)
  | _ -> ());
  List.rev !arms

(* the constant-folding arm derived from [fold] *)
let fold_arm ~loc ctx o : case option =
  match o.op_fold with
  | None -> None
  | Some fold ->
      let n = arity o in
      let names = List.init n (fun i -> Printf.sprintf "__f%d" i) in
      let pats =
        List.map2
          (fun nm ty ->
            match Hashtbl.find_opt ctx.lit_by_ty ty with
            | Some li -> pat_kind ~loc (pconstr ~loc li.li_kind [ pvar ~loc nm ])
            | None -> B.ppat_any ~loc)
          names o.op_args
      in
      let lhs = match pats with [ p ] -> p | l -> B.ppat_tuple ~loc l in
      let folded = eapply ~loc fold (List.map (evar ~loc) names) in
      let rhs =
        match lit_ctor_for ctx o.op_ret with
        | Some c -> eapply ~loc (evar ~loc c) [ folded ]
        | None -> folded
      in
      Some (B.case ~lhs ~guard:None ~rhs)

let smart_ctor ~loc ctx o : value_binding =
  let n = arity o in
  let cparams = List.map (fun (nm, _) -> pvar ~loc nm) o.op_params in
  let params =
    cparams @ List.init n (fun i -> pvar ~loc (Printf.sprintf "__a%d" i))
  in
  let scrut =
    match n with
    | 1 -> evar ~loc "__a0"
    | _ -> B.pexp_tuple ~loc (List.init n (fun i -> evar ~loc (Printf.sprintf "__a%d" i)))
  in
  let user_arms =
    List.concat_map
      (fun r ->
        let a = Option.to_list (rule_arm ctx ~loc o ~swap:false r) in
        if o.op_commutative && n = 2 then
          a @ Option.to_list (rule_arm ctx ~loc o ~swap:true r)
        else a)
      o.op_rules
  in
  let arms =
    derived_arms ~loc ctx o
    @ Option.to_list (fold_arm ~loc ctx o)
    @ user_arms
    @ [ B.case ~lhs:(B.ppat_any ~loc) ~guard:None ~rhs:(default_expr ~loc ctx o) ]
  in
  let body =
    List.fold_right
      (fun p acc -> B.pexp_fun ~loc Nolabel None p acc)
      params
      (B.pexp_match ~loc scrut arms)
  in
  B.value_binding ~loc ~pat:(pvar ~loc o.op_name) ~expr:body

let smart_ctors ~loc ctx =
  match ctx.ops with
  | [] -> []
  | _ ->
      [ B.pstr_value ~loc Recursive (List.map (smart_ctor ~loc ctx) ctx.ops) ]

(* {2 Literal & leaf constructors} *)

let let_fun ~loc name params body =
  let e =
    List.fold_right
      (fun p acc -> B.pexp_fun ~loc Nolabel None (pvar ~loc p) acc)
      params body
  in
  B.pstr_value ~loc Nonrecursive
    [ B.value_binding ~loc ~pat:(pvar ~loc name) ~expr:e ]

let leaf_lit_ctors ~loc ctx =
  let var_ctor =
    List.filter_map
      (fun (l : leaf_decl) ->
        if l.leaf_ty = None then
          Some
            (let_fun ~loc
               ("mk_" ^ String.lowercase_ascii l.leaf_name)
               [ "v"; "ty" ]
               (eapply ~loc (evar ~loc "<|")
                  [ econstr ~loc l.leaf_name [ evar ~loc "v" ]; evar ~loc "ty" ]))
        else None)
      ctx.leaves
  in
  let lit_ctors =
    List.map
      (fun (li : lit_info) ->
        let_fun ~loc li.li_ctor [ "x" ]
          (eapply ~loc (evar ~loc "<|")
             [
               econstr ~loc li.li_kind [ evar ~loc "x" ];
               econstr ~loc (ty_ctor li.li_ty) [];
             ]))
      ctx.lits
  in
  (* bool conveniences *)
  let bool_extra =
    match List.find_opt (fun l -> l.li_pay = "bool") ctx.lits with
    | Some li ->
        [
          [%stri let v_true = [%e eapply ~loc (evar ~loc li.li_ctor) [ [%expr true] ]]];
          [%stri let v_false = [%e eapply ~loc (evar ~loc li.li_ctor) [ [%expr false] ]]];
          [%stri let of_bool b = if b then v_true else v_false];
          [%stri
            let to_bool t =
              if equal t v_true then Some true
              else if equal t v_false then Some false
              else None];
        ]
    | None -> []
  in
  var_ctor @ lit_ctors @ bool_extra

(* {2 Hash-consing, equality, iteration} *)

let pp_fun ~loc ctx =
  let cases = ref [] in
  let add c = cases := c :: !cases in
  List.iter
    (fun (l : leaf_decl) ->
      if l.leaf_ty = None then
        add
          (B.case
             ~lhs:(pconstr ~loc l.leaf_name [ pvar ~loc "v" ])
             ~guard:None
             ~rhs:[%expr Fmt.pf ft "V%a" Var.pp v]))
    ctx.leaves;
  List.iter
    (fun (li : lit_info) ->
      let printer =
        if li.li_pay = "bool" then [%expr Fmt.pf ft "%b" x]
        else
          let decl =
            List.find_opt
              (function DLiteral d -> d.lit_name = li.li_kind | _ -> false)
              !ctx_program_ref
          in
          match decl with
          | Some (DLiteral { lit_print = Some pr; _ }) ->
              [%expr Fmt.pf ft "%a" [%e pr] x]
          | _ -> [%expr Fmt.pf ft "%s" (Stdlib.string_of_int (Hashtbl.hash x))]
      in
      add (B.case ~lhs:(pconstr ~loc li.li_kind [ pvar ~loc "x" ]) ~guard:None ~rhs:printer))
    ctx.lits;
  if List.exists (fun o -> arity o = 1) ctx.ops then
    add
      (B.case ~lhs:[%pat? Unop (op, v)] ~guard:None
         ~rhs:[%expr Fmt.pf ft "%a(%a)" Unop.pp op pp v]);
  if List.exists (fun o -> arity o = 2 && not (is_ite o)) ctx.ops then
    add
      (B.case ~lhs:[%pat? Binop (op, a, b)] ~guard:None
         ~rhs:[%expr Fmt.pf ft "(%a %a %a)" pp a Binop.pp op pp b]);
  if ctx.nops <> [] then
    add
      (B.case ~lhs:[%pat? Nop (op, l)] ~guard:None
         ~rhs:[%expr Fmt.pf ft "%a(%a)" Nop.pp op Fmt.(list ~sep:comma pp) l]);
  if List.exists is_ite ctx.ops then
    add
      (B.case ~lhs:[%pat? Ite (c, t, e)] ~guard:None
         ~rhs:[%expr Fmt.pf ft "(%a ? %a : %a)" pp c pp t pp e]);
  let m = B.pexp_match ~loc [%expr t.node.kind] (List.rev !cases) in
  [ [%stri let rec pp ft t = [%e m]] ]

let core_items ~loc ctx =
  let leaf_lit_pats =
    List.map (fun (l : leaf_decl) -> pconstr ~loc l.leaf_name [ B.ppat_any ~loc ]) ctx.leaves
    @ List.map (fun li -> pconstr ~loc li.li_kind [ B.ppat_any ~loc ]) ctx.lits
  in
  let leaf_lit_or =
    match leaf_lit_pats with
    | [] -> B.ppat_any ~loc
    | p :: tl -> List.fold_left (fun acc x -> B.ppat_or ~loc acc x) p tl
  in
  let prelude =
    [
      [%stri let pp_hash_consed pp_node ft t = pp_node ft t.node];
      [%stri let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag];
      [%stri let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag];
    ]
  in
  let has_un = List.exists (fun o -> arity o = 1) ctx.ops in
  let has_bin = List.exists (fun o -> arity o = 2 && not (is_ite o)) ctx.ops in
  let has_nop = ctx.nops <> [] in
  let has_ite = List.exists is_ite ctx.ops in
  let var_pat =
    match List.find_opt (fun l -> l.leaf_ty = None) ctx.leaves with
    | Some l -> pconstr ~loc l.leaf_name [ pvar ~loc "v" ]
    | None -> [%pat? Var v]
  in
  let iter_cases =
    [
      B.case ~lhs:var_pat ~guard:None ~rhs:[%expr f (v, sv.node.ty)];
      B.case ~lhs:leaf_lit_or ~guard:None ~rhs:[%expr ()];
    ]
    @ (if has_un then
         [ B.case ~lhs:[%pat? Unop (_, s)] ~guard:None ~rhs:[%expr recurse s] ]
       else [])
    @ (if has_bin then
         [
           B.case ~lhs:[%pat? Binop (_, l, r)] ~guard:None
             ~rhs:[%expr recurse l; recurse r];
         ]
       else [])
    @ (if has_nop then
         [
           B.case ~lhs:[%pat? Nop (_, l)] ~guard:None
             ~rhs:[%expr List.iter recurse l];
         ]
       else [])
    @
    if has_ite then
      [
        B.case ~lhs:[%pat? Ite (c, t, e)] ~guard:None
          ~rhs:[%expr recurse c; recurse t; recurse e];
      ]
    else []
  in
  let iter_match = B.pexp_match ~loc [%expr sv.node.kind] iter_cases in
  let hash_cases =
    [ B.case ~lhs:leaf_lit_or ~guard:None ~rhs:[%expr Hashtbl.hash (kind, hty)] ]
    @ (if has_un then
         [
           B.case ~lhs:[%pat? Unop (op, v)] ~guard:None
             ~rhs:[%expr Hashtbl.hash (op, v.tag, hty)];
         ]
       else [])
    @ (if has_bin then
         [
           B.case ~lhs:[%pat? Binop (op, l, r)] ~guard:None
             ~rhs:[%expr Hashtbl.hash (op, l.tag, r.tag, hty)];
         ]
       else [])
    @ (if has_nop then
         [
           B.case ~lhs:[%pat? Nop (op, l)] ~guard:None
             ~rhs:[%expr Hashtbl.hash (op, List.map (fun s -> s.tag) l, hty)];
         ]
       else [])
    @
    if has_ite then
      [
        B.case ~lhs:[%pat? Ite (c, t, e)] ~guard:None
          ~rhs:[%expr Hashtbl.hash (c.tag, t.tag, e.tag, hty)];
      ]
    else []
  in
  let hash_match = B.pexp_match ~loc [%expr kind] hash_cases in
  let commut =
    (if has_bin then
       [
         [%stri
           let mk_commut_binop op l r =
             if l.tag <= r.tag then Binop (op, l, r) else Binop (op, r, l)];
       ]
     else [])
    @
    if has_nop then
      [
        [%stri
          let mk_commut_nop op vs =
            Nop (op, List.sort_uniq (fun l r -> Int.compare l.tag r.tag) vs)];
      ]
    else []
  in
  prelude
  @ [ kind_type ~loc ctx ]
  @ [
      [%stri let unique_tag t = t.tag];
      [%stri let hash t = t.tag];
      [%stri let kind t = t.node.kind];
      [%stri let[@inline] equal a b = Int.equal a.tag b.tag];
      [%stri let[@inline] compare a b = Int.compare a.tag b.tag];
      [%stri
        let rec iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
          let recurse s = iter_vars s f in
          [%e iter_match]];
      [%stri let pp_full ft t = pp_t_node ft t.node];
    ]
  @ pp_fun ~loc ctx
  @ [
      [%stri
        module Hcons = Hc.Make (struct
          type nonrec t = t_node

          let equal = equal_t_node

          let hash { kind; ty } =
            let hty = Hashtbl.hash ty in
            [%e hash_match]
        end)];
      [%stri let ( <| ) kind ty : t = Hcons.hashcons { kind; ty }];
    ]
  @ commut

(* hand-written n-ary smart constructors ([{{ ... }}] body on a [nop]) *)
let nop_bodies ~loc ctx =
  List.filter_map
    (fun (nd : nop_decl) ->
      match nd.nop_body with
      | Some body ->
          Some
            (B.pstr_value ~loc Nonrecursive
               [ B.value_binding ~loc ~pat:(pvar ~loc nd.nop_name) ~expr:body ])
      | None -> None)
    ctx.nops

(* {2 Generic dispatchers + eval} *)

let dispatchers ~loc ctx =
  let unops = List.filter (fun o -> arity o = 1) ctx.ops in
  let binops = List.filter (fun o -> arity o = 2 && not (is_ite o)) ctx.ops in
  let mk name modname ops =
    if ops = [] then []
    else
      let cases =
        List.map
          (fun o ->
            (* [function Lt s -> lt s | Add c -> add c | Not -> not | ...] *)
            let pnames =
              List.mapi (fun i _ -> Printf.sprintf "__p%d" i) o.op_params
            in
            B.case
              ~lhs:
                (pconstr ~loc
                   (modname ^ "." ^ o.op_ctor)
                   (List.map (pvar ~loc) pnames))
              ~guard:None
              ~rhs:
                (let f = evar ~loc o.op_name in
                 match pnames with
                 | [] -> f
                 | _ -> eapply ~loc f (List.map (evar ~loc) pnames)))
          ops
      in
      [ B.pstr_value ~loc Nonrecursive
          [ B.value_binding ~loc ~pat:(pvar ~loc name)
              ~expr:(efunction ~loc cases) ] ]
  in
  let mk_nop =
    if ctx.nops = [] then []
    else
      let cases =
        List.map
          (fun (nd : nop_decl) ->
            B.case
              ~lhs:(pconstr ~loc ("Nop." ^ nd.nop_ctor) [])
              ~guard:None ~rhs:(evar ~loc nd.nop_name))
          ctx.nops
      in
      [ B.pstr_value ~loc Nonrecursive
          [ B.value_binding ~loc ~pat:(pvar ~loc "mk_nop")
              ~expr:(efunction ~loc cases) ] ]
  in
  mk "mk_unop" "Unop" unops @ mk "mk_binop" "Binop" binops @ mk_nop

let eval_items ~loc ctx =
  let has_un = List.exists (fun o -> arity o = 1) ctx.ops in
  let has_bin = List.exists (fun o -> arity o = 2 && not (is_ite o)) ctx.ops in
  let has_nop = ctx.nops <> [] in
  let has_ite = List.exists is_ite ctx.ops in
  let leaf_var =
    match List.find_opt (fun l -> l.leaf_ty = None) ctx.leaves with
    | Some l -> l.leaf_name
    | None -> "Var"
  in
  let lit_pats =
    List.map (fun li -> pconstr ~loc li.li_kind [ B.ppat_any ~loc ]) ctx.lits
  in
  let lit_or =
    match lit_pats with
    | [] -> B.ppat_any ~loc
    | p :: tl -> List.fold_left (fun acc x -> B.ppat_or ~loc acc x) p tl
  in
  let arms = ref [] in
  let add c = arms := c :: !arms in
  add
    (B.case
       ~lhs:(pat_kind ~loc (pconstr ~loc leaf_var [ pvar ~loc "v" ]))
       ~guard:None
       ~rhs:[%expr eval_var v x.node.ty]);
  add (B.case ~lhs:(pat_kind ~loc lit_or) ~guard:None ~rhs:[%expr x]);
  if has_un then
    add
      (B.case
         ~lhs:(pat_kind ~loc [%pat? Unop (op, v)])
         ~guard:None
         ~rhs:[%expr let nv = eval v in if v == nv then x else mk_unop op nv]);
  if has_bin then
    add
      (B.case
         ~lhs:(pat_kind ~loc [%pat? Binop (op, v1, v2)])
         ~guard:None
         ~rhs:
           [%expr
             let nv1 = eval v1 in
             let nv2 = eval v2 in
             if v1 == nv1 && v2 == nv2 then x else mk_binop op nv1 nv2]);
  if has_nop then
    add
      (B.case
         ~lhs:(pat_kind ~loc [%pat? Nop (op, l)])
         ~guard:None
         ~rhs:
           [%expr
             let l, changed = Soteria_std.List.map_changed eval l in
             if Stdlib.not changed then x else mk_nop op l]);
  if has_ite then
    add
      (B.case
         ~lhs:(pat_kind ~loc [%pat? Ite (g, t, e)])
         ~guard:None
         ~rhs:
           [%expr
             let g = eval g in
             if equal g v_true then eval t
             else if equal g v_false then eval e
             else ite g (eval t) (eval e)]);
  let m = B.pexp_match ~loc [%expr x] (List.rev !arms) in
  [
    [%stri
      type _ Effect.t += Eval_var : Var.t * ty -> t Effect.t];
    [%stri let eval_var (v : Var.t) (ty : ty) : t = Effect.perform (Eval_var (v, ty))];
    [%stri let rec eval (x : t) : t = [%e m]];
    [%stri
      let eval ~(eval_var : Var.t -> ty -> t option) (x : t) : t option =
        try Some (eval x) with
        | Division_by_zero -> None
        | effect Eval_var (v, ty), k -> (
            match eval_var v ty with
            | Some v -> Effect.Deep.continue k v
            | None -> None)];
  ]

(* {2 Phantom-typed wrapper module} *)

(* The sort used to type a signature token (a [sort] name keeps itself, a
   [ty] name maps to its declared default sort). *)
let sort_of ctx name =
  if List.exists (fun (s : sort_decl) -> s.sort_name = name) ctx.sorts then name
  else match Hashtbl.find_opt ctx.ty_sort name with Some s -> s | None -> name

let typed_module ~loc ctx =
  if ctx.sorts = [] then []
  else begin
    let buf = Buffer.create 1024 in
    let p fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
    let row s = Format.asprintf "%a" Pprintast.core_type s.sort_row in
    let arg_ty tok =
      if tok = "any" then "'a t"
      else Printf.sprintf "[< %s ] t" (sort_of ctx tok)
    in
    let ret_ty tok =
      if tok = "any" then "'a t"
      else Printf.sprintf "[> %s ] t" (sort_of ctx tok)
    in
    (* {b The phantom seal}: an abstract [+'a t]/[+'a ty] in the signature is
       what actually enforces the sorts (exactly as the hand-written
       [typed.mli] does — [type +'a t = t] alone would be vacuous). *)
    p "  module T : sig\n";
    List.iter
      (fun (s : sort_decl) -> p "    type %s = %s\n" s.sort_name (row s))
      ctx.sorts;
    List.iter
      (fun (s : sort_decl) ->
        p "    val pp_%s : Format.formatter -> %s -> unit\n" s.sort_name
          s.sort_name)
      ctx.sorts;
    p "  end\n";
    p "  open T\n";
    p "  type raw_t = t\n";
    p "  type raw_ty = ty\n";
    p "  type +'a ty\n";
    p "  type +'a t\n";
    p "  val get_ty : 'a t -> raw_ty\n";
    p "  val untype_type : 'a ty -> raw_ty\n";
    p "  val ppa : Format.formatter -> 'a t -> unit\n";
    p "  val pp : (Format.formatter -> 'b -> unit) -> Format.formatter -> 'a t -> unit\n";
    p "  val ppa_ty : Format.formatter -> 'a ty -> unit\n";
    p "  val pp_ty : (Format.formatter -> 'b -> unit) -> Format.formatter -> 'a ty -> unit\n";
    p "  val equal_ty : 'a ty -> 'b ty -> bool\n";
    p "  val cast : 'a t -> 'b t\n";
    p "  val untyped : 'a t -> raw_t\n";
    p "  val untyped_list : 'a t list -> raw_t list\n";
    p "  val type_ : raw_t -> 'a t\n";
    p "  val type_checked : raw_t -> 'a ty -> 'a t option\n";
    p "  val cast_checked : 'a t -> 'b ty -> 'b t option\n";
    p "  val cast_checked2 : 'a t -> 'b t -> ('c t * 'c t * 'c ty) option\n";
    List.iter
      (fun o ->
        let args, ret = Hashtbl.find ctx.op_sorts o.op_name in
        let pty =
          List.map
            (fun (_, ct) ->
              "(" ^ Format.asprintf "%a" Pprintast.core_type ct ^ ")")
            o.op_params
        in
        p "  val %s : %s\n" o.op_name
          (String.concat " -> "
             (pty @ List.map arg_ty args @ [ ret_ty ret ])))
      ctx.ops;
    List.iter
      (fun (li : lit_info) ->
        let s = sort_of ctx li.li_ty in
        p "  val %s : %s -> [> %s ] t\n" li.li_ctor li.li_pay s;
        if li.li_pay = "bool" then (
          p "  val v_true : [> %s ] t\n" s;
          p "  val v_false : [> %s ] t\n" s;
          p "  val to_bool : 'a t -> bool option\n"))
      ctx.lits;
    List.iter
      (fun (l : leaf_decl) ->
        if l.leaf_ty = None then
          p "  val mk_%s : %s -> 'a ty -> 'a t\n"
            (String.lowercase_ascii l.leaf_name)
            (Format.asprintf "%a" Pprintast.core_type l.leaf_payload))
      ctx.leaves;
    let sg =
      try Ppxlib.Parse.interface (Lexing.from_string (Buffer.contents buf))
      with e ->
        Location.raise_errorf ~loc "internal: Typed sig parse (%s):\n%s"
          (Printexc.to_string e) (Buffer.contents buf)
    in
    let sort_tds =
      List.map
        (fun (s : sort_decl) ->
          B.pstr_type ~loc Recursive
            [
              B.type_declaration ~loc ~name:{ txt = s.sort_name; loc }
                ~params:[] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public
                ~manifest:(Some s.sort_row);
            ])
        ctx.sorts
    in
    let sort_pps =
      List.map
        (fun (s : sort_decl) ->
          B.pstr_value ~loc Nonrecursive
            [
              B.value_binding ~loc
                ~pat:(pvar ~loc ("pp_" ^ s.sort_name))
                ~expr:
                  (B.pexp_fun ~loc Nolabel None (B.ppat_any ~loc)
                     (B.pexp_fun ~loc Nolabel None (B.ppat_any ~loc)
                        [%expr ()]));
            ])
        ctx.sorts
    in
    let t_mod =
      B.pstr_module ~loc
        (B.module_binding ~loc ~name:{ txt = Some "T"; loc }
           ~expr:(B.pmod_structure ~loc (sort_tds @ sort_pps)))
    in
    let reexport name = [%stri let [%p pvar ~loc name] = [%e evar ~loc name]] in
    let op_binds = List.map (fun o -> reexport o.op_name) ctx.ops in
    let lit_binds =
      List.concat_map
        (fun (li : lit_info) ->
          if li.li_pay = "bool" then
            [
              reexport li.li_ctor;
              reexport "v_true";
              reexport "v_false";
              reexport "to_bool";
            ]
          else [ reexport li.li_ctor ])
        ctx.lits
    in
    let var_binds =
      List.filter_map
        (fun (l : leaf_decl) ->
          if l.leaf_ty = None then
            Some (reexport ("mk_" ^ String.lowercase_ascii l.leaf_name))
          else None)
        ctx.leaves
    in
    let glue =
      [
        [%stri type raw_t = t];
        [%stri type raw_ty = ty];
        [%stri type nonrec +'a ty = ty];
        [%stri type nonrec +'a t = t];
        [%stri let get_ty x = x.node.ty];
        [%stri let untype_type x = x];
        [%stri let ppa = pp];
        [%stri let pp _ = pp];
        [%stri let ppa_ty = pp_ty];
        [%stri let pp_ty _ = pp_ty];
        [%stri let equal_ty = equal_ty];
        [%stri let cast x = x];
        [%stri let untyped x = x];
        [%stri let untyped_list l = l];
        [%stri let type_ x = x];
        [%stri
          let type_checked x ty =
            if equal_ty x.node.ty ty then Some x else None];
        [%stri let cast_checked = type_checked];
        [%stri
          let cast_checked2 x y =
            if equal_ty x.node.ty y.node.ty then Some (x, y, x.node.ty)
            else None];
      ]
    in
    let body =
      (t_mod :: [%stri open T] :: glue) @ op_binds @ lit_binds @ var_binds
    in
    [
      B.pstr_module ~loc
        (B.module_binding ~loc ~name:{ txt = Some "Typed"; loc }
           ~expr:
             (B.pmod_constraint ~loc
                (B.pmod_structure ~loc body)
                (B.pmty_signature ~loc sg)));
    ]
  end

(* {2 Top level} *)

let generate ~loc (p : program) : structure =
  ctx_program_ref := p;
  let ctx = build_ctx p in
  let unops = List.filter_map (fun o -> if arity o = 1 then Some o else None) ctx.ops in
  let binops =
    List.filter_map
      (fun o -> if arity o = 2 && not (is_ite o) then Some o else None)
      ctx.ops
  in
  let pp_of o = match o.op_symbol with Some s -> s | None -> o.op_name in
  let op_def o = (o.op_ctor, List.map snd o.op_params, pp_of o) in
  let items =
    [ ty_decl ~loc ctx ]
    @ ty_helpers ~loc ctx
    @ op_module ~loc ~name:"Nop"
        (List.map
           (fun n ->
             (n.nop_ctor, [], Option.value ~default:n.nop_ctor n.nop_symbol))
           ctx.nops)
    @ op_module ~loc ~name:"Unop" (List.map op_def unops)
    @ op_module ~loc ~name:"Binop" (List.map op_def binops)
    @ core_items ~loc ctx
    @ leaf_lit_ctors ~loc ctx
    @ List.concat ctx.aux
    @ smart_ctors ~loc ctx
    @ nop_bodies ~loc ctx
    @ dispatchers ~loc ctx
    @ eval_items ~loc ctx
    @ typed_module ~loc ctx
  in
  [
    B.pstr_include ~loc
      (B.include_infos ~loc
         (B.pmod_structure ~loc
            (B.pstr_attribute ~loc
               (B.attribute ~loc ~name:{ txt = "ocaml.warning"; loc }
                  ~payload:(PStr [ B.pstr_eval ~loc (B.estring ~loc "-a") [] ]))
            :: items)));
  ]
