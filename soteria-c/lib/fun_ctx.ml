module Bidirectional_map = struct
  include Bimap.Make (Symbol_std) (Z)

  let has_sym = mem_l
  let get_loc_id = find_l
  let get_sym = find_r
end

(* FIXME: This is slightly off because we could have a location that is already
   assigned. This will cause an unsoundness when testing pointer equality with
   other objects. But, if you do this, you already have a bug! We will merely
   signal the wrong bug. In any case, we will fix this when we have native
   support for disjoint addresses. *)
type t = { counter : Z.t; bmap : Bidirectional_map.t }

let empty = { counter = Z.one; bmap = Bidirectional_map.empty }

let declare_fn sym t =
  let counter = t.counter in
  let bmap = Bidirectional_map.add sym counter t.bmap in
  { counter = Z.succ counter; bmap }

let of_linked_program (prog : Ail_tys.linked_program) =
  (* We add all function definitions *)
  let first_pass =
    ListLabels.fold_left prog.sigma.function_definitions ~init:empty
      ~f:(fun ctx (sym, _) -> declare_fn sym ctx)
  in
  (* We also add all {declarations} of a builtin function for which there is no
     {definition}. *)
  ListLabels.fold_left prog.sigma.declarations ~init:first_pass
    ~f:(fun ctx (sym, (_, _, decl)) ->
      match decl with
      | Cerb_frontend.AilSyntax.Decl_object _ -> ctx
      | Decl_function _ ->
          let sym = Ail_helpers.resolve_sym ~prog sym in
          let sym_name =
            match sym with
            | Cerb_frontend.Symbol.Symbol (_, _, SD_Id name) -> name
            | _ -> failwith "Expected a function symbol"
          in
          let is_a_builtin = List.mem sym_name Stubs.builtin_functions in
          let is_already_declared = Bidirectional_map.has_sym sym ctx.bmap in
          if (not is_a_builtin) || is_already_declared then ctx
          else declare_fn sym ctx)

let decay_fn_sym sym t =
  Bidirectional_map.get_loc_id sym t.bmap
  |> Option.map Typed.Ptr.loc_of_z
  |> Csymex.of_opt_not_impl
       ~msg:(Fmt.str "Function has not been declared! %a" Fmt_ail.pp_sym sym)

let get_sym sv t =
  let res =
    match Typed.kind sv with
    | BitVec z -> Bidirectional_map.get_sym z t.bmap
    | _ -> None
  in
  Csymex.of_opt_not_impl ~msg:"Could not resolve function" @@ res
