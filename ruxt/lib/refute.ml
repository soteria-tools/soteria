module Rustsymex = Soteria_rust_lib.Rustsymex
module Rust_val = Soteria_rust_lib.Rust_val
module Wpst_interp = Soteria_rust_lib.Interp.Make (Heap)
module Compo_res = Soteria.Symex.Compo_res
open Syntaxes.FunctionWrap
open Rustsymex.Syntax
open Charon.Types

exception ExecutionError of string

let execution_err msg = raise (ExecutionError msg)

module Symok = struct
  let unwrap res =
    Rustsymex.map res (function
      | Compo_res.Ok v -> v
      | _ -> execution_err "Expected Ok")

  let load ptr ty state = Heap.load ptr ty state |> unwrap

  let store ptr ty rv state =
    let+ (), state = Heap.store ptr ty rv state |> unwrap in
    state

  let alloc_ty ty state = Heap.alloc_ty ty state |> unwrap

  let alloc ty rv state =
    let* ptr, state = alloc_ty ty state in
    let+ state = store ptr ty rv state in
    (ptr, state)

  let free ptr state =
    let+ (), state = Heap.free ptr state |> unwrap in
    state

  let exec_fun ~args ~state fundef =
    Wpst_interp.exec_fun ~args ~state fundef |> unwrap
end

module Drop = struct
  type t = Charon.UllbcAst.fun_decl TypeDeclId.Map.t

  let is_impl (fundef : Charon.UllbcAst.fun_decl) =
    match fundef.src with TraitImplItem (_, _, "drop", _) -> true | _ -> false

  let create_ctx () : t =
    let crate = Soteria_rust_lib.Crate.get_crate () in
    let add_drop _ fundef ctx =
      if is_impl fundef then
        match List.hd fundef.signature.inputs with
        | TRef (_, TAdt { id = TAdtId id; _ }, _) ->
            TypeDeclId.Map.add id fundef ctx
        | _ -> ctx
      else ctx
    in
    FunDeclId.Map.fold add_drop crate.fun_decls TypeDeclId.Map.empty

  let find_and_exec ty (ctx : t) ~none ~some =
    match ty with
    | TAdt { id = TAdtId id; _ } -> (
        match TypeDeclId.Map.find_opt id ctx with
        | Some fundef ->
            let* ptr, state = some in
            let* _, state = Symok.exec_fun fundef ~args:[ Ptr ptr ] ~state in
            Symok.free ptr state
        | None -> none)
    | _ -> none
end

let wrap_call (fundef : Charon.UllbcAst.fun_decl) summs =
  (* Check reference arguments and allocate values on heap *)
  let* args, state, arg_ptrs =
    ListLabels.fold_left2 summs fundef.signature.inputs
      ~init:(Rustsymex.return ([], Heap.empty, []))
      ~f:(fun acc summ ty ->
        let* args, state, arg_ptrs = acc in
        let* arg, state = Summary.produce summ state in
        match ty with
        | TRef (_, ty, _) ->
            let+ ptr, state = Symok.alloc ty arg state in
            (Rust_val.Ptr ptr :: args, state, (ty, ptr) :: arg_ptrs)
        | _ -> Rustsymex.return (arg :: args, state, arg_ptrs))
  in
  let args = List.rev args in
  (* Symbolically execute the function call *)
  let++ ret, state = Wpst_interp.exec_fun fundef ~args ~state in
  (fundef.signature.output, ret, state, arg_ptrs)

let wrap_return summ_ctx process =
  (* Obtain the result from the executing the function call *)
  let** ty, ret, state, arg_ptrs = process in
  let* state =
    match ty with
    | TRef (_, ty, kind) -> (
        (* The return value must be a pointer *)
        let ptr = Rust_val.as_ptr ret in
        match kind with
        | RShared ->
            (* For shared references, we simply read the return pointer *)
            let+ _, state = Symok.load ptr ty state in
            state
        | RMut ->
            (* For mutable references, we write to the pointer with safe values*)
            let summs = Summary.Context.get ty summ_ctx in
            ListLabels.map summs ~f:(fun summ () ->
                let* ret, state = Summary.produce summ state in
                Symok.store ptr ty ret state)
            |> Rustsymex.branches)
    | _ -> Rustsymex.return state
  in
  (* Return the same result with the updated state *)
  Rustsymex.Result.ok (ty, ret, state, arg_ptrs)

let drop_and_return drop_ctx process =
  (* Obtain the result from the executing the function call *)
  let** ty, ret, state, arg_ptrs = process in
  (* Drop the return value *)
  let drop_ret state =
    Drop.find_and_exec ty drop_ctx ~none:(Rustsymex.return state)
      ~some:(Symok.alloc ty ret state)
  in
  (* Drop a reference argument *)
  let drop_ptr ty ptr state =
    Drop.find_and_exec ty drop_ctx ~none:(Symok.free ptr state)
      ~some:(Rustsymex.return (ptr, state))
  in
  (* For each reference, we create an execution branch that returns the
     stored value and drops everything else, including the return value *)
  let rec get_branches ?(acc = []) ?(st = Rustsymex.return state) = function
    | [] ->
        (* Case 0: we learn from the return value, the rest has been dropped *)
        let branch () =
          let* state = st in
          Rustsymex.Result.ok (ty, ret, state)
        in
        branch :: acc
    | (ty, ptr) :: arg_ptrs ->
        (* Case 1: we learn from this reference and drop the rest *)
        let branch () =
          let* state = st in
          let* ret, state = Symok.load ptr ty state in
          let* state = Symok.free ptr state in
          let* state =
            ListLabels.fold_left arg_ptrs ~init:(drop_ret state)
              ~f:(fun st (ty, ptr) -> Rustsymex.bind st (drop_ptr ty ptr))
          in
          Rustsymex.Result.ok (ty, ret, state)
        in
        (* Case 2: we learn nothing from this reference, so we drop it *)
        let st =
          let* state = st in
          drop_ptr ty ptr state
        in
        (* We keep case 1 in the result and proceed with the state from case 2 *)
        get_branches arg_ptrs ~acc:(branch :: acc) ~st
  in
  get_branches arg_ptrs |> Rustsymex.branches

let leak_check = function
  | [] -> Result.ok ()
  | leaks ->
      Fmt.pr "Leaked:";
      List.iter (fun (loc, _) -> Fmt.pr " %a" Rustsymex.Value.ppa loc) leaks;
      Fmt.pr "\n";
      Result.error `MemoryLeak

let try_refute fuel drop_ctx fundef summs summ_ctx =
  (* Symbolically execute the function call *)
  let outcomes =
    wrap_call fundef summs
    |> wrap_return summ_ctx
    |> drop_and_return drop_ctx
    |> Rustsymex.run ~mode:UX ~fuel
  in
  (* For each successful outcome, the summary context will be updated. *)
  ListLabels.fold_left outcomes ~init:(Result.ok summ_ctx)
    ~f:(fun acc -> function
    (* Successful termination: update the summary context *)
    | Compo_res.Ok (ty, ret, state), pcs ->
        Result.bind acc @@ fun ctx ->
        let summ, leaks = Summary.make ret pcs state in
        let update_ctx () = Summary.Context.update ty summ ctx in
        leak_check leaks |> Result.map update_ctx
    (* Unsuccessful termination: found a type unsoundness *)
    | _ -> Result.error `TypeUnsound)

let get_funs () =
  let crate = Soteria_rust_lib.Crate.get_crate () in
  let can_infer _ (fundef : Charon.UllbcAst.fun_decl) =
    fundef.item_meta.is_local
    && (not fundef.signature.is_unsafe)
    && (not @@ Drop.is_impl fundef)
    && (fundef.item_meta.attr_info.public || not !Config.current.only_public)
  in
  FunDeclId.Map.filter can_infer crate.fun_decls

let exec_crate (crate : Charon.UllbcAst.crate) =
  let@ () = Soteria_rust_lib.Crate.with_crate crate in
  let fun_decls = get_funs () in
  (* Set context for calls to try_refute *)
  let try_refute =
    let drop_ctx = Drop.create_ctx () in
    let soteria_fuel =
      let open Soteria.Symex.Fuel_gauge in
      let fuel = function None -> Fuel_value.Infinite | Some i -> Finite i in
      let steps = fuel !Config.current.step_fuel in
      let branching = fuel !Config.current.branch_fuel in
      { steps; branching }
    in
    try_refute soteria_fuel drop_ctx
  in
  (* The try_refute procedure is called for each function *)
  let try_refute _ (fundef : Charon.UllbcAst.fun_decl) acc =
    Result.bind acc @@ fun summ_ctx ->
    let tys =
      List.map
        (function TRef (_, ty, _) -> ty | ty -> ty)
        fundef.signature.inputs
    in
    let try_refute ctx summs = Result.bind ctx @@ try_refute fundef summs in
    Iter.fold try_refute acc (Summary.Context.iter_summs tys summ_ctx)
  in
  (* The meta-loop iterates over all safe functions *)
  let rec meta_loop summ_ctx fuel =
    if fuel <= 0 then false (* Fuel exhausted *)
    else (* Call try_refute on all functions and accumulate the results *)
      match FunDeclId.Map.fold try_refute fun_decls (Result.ok summ_ctx) with
      | Ok summ_ctx -> meta_loop summ_ctx (fuel - 1)
      | Error _ -> true (* Type unsoundness found *)
  in
  (* Run the algorithm starting from an empty summary context *)
  meta_loop Summary.Context.empty !Config.current.pass_fuel

let exec_ruxt config file_name =
  Config.set config;
  let open Soteria_rust_lib in
  let compile () = fst @@ Frontend.parse_ullbc_of_file file_name in
  match Driver.wrap_step "Compiling" compile |> exec_crate with
  | found_unsoundness ->
      let ret, msg =
        if found_unsoundness then (1, "Found type unsoundness!\n")
        else (0, "No type unsoundness found!\n")
      in
      Fmt.pr "%s" msg;
      exit ret
  | exception ExecutionError e -> Driver.fatal e
  | exception Frontend.PluginError e -> Driver.fatal ~name:"Plugin" ~code:3 e
  | exception Frontend.FrontendError e -> Driver.fatal ~name:"Charon" ~code:4 e
  | exception Frontend.CompilationError e ->
      Soteria.Terminal.Diagnostic.print_diagnostic_simple ~severity:Error
        ("Compilation error:\n" ^ e);
      Driver.Outcome.exit Error
  | exception Driver.ExecutionError e -> Driver.fatal ~name:"Rusteria" ~code:3 e
