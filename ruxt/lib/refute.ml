module Config_ = Config
open Soteria_rust_lib
module Config = Config_
module Wpst_interp = Interp.Make (Heap)
module Compo_res = Soteria.Symex.Compo_res
open Syntaxes.FunctionWrap
open Rustsymex.Syntax
open Charon.Types

exception ExecutionError of string

let execution_err msg = raise (ExecutionError msg)

(* Symbolic execution of Ok-terminating computations *)
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

  let exec_fun ~args ~state fun_decl =
    Wpst_interp.exec_fun ~args ~state fun_decl |> unwrap
end

module Drop = struct
  type t = Frontend.fun_decl TypeDeclId.Map.t

  let is_impl (fun_decl : Frontend.fun_decl) =
    match fun_decl.src with
    | TraitImplItem (_, _, "drop", _) -> true
    | _ -> false

  let init_ctx () : t =
    let crate = Crate.get_crate () in
    let add_drop _ fun_decl ctx =
      if is_impl fun_decl then
        match List.hd fun_decl.signature.inputs with
        | TRef (_, TAdt { id = TAdtId id; _ }, _) ->
            TypeDeclId.Map.add id fun_decl ctx
        | _ -> ctx
      else ctx
    in
    FunDeclId.Map.fold add_drop crate.fun_decls TypeDeclId.Map.empty

  let find_and_exec ty (ctx : t) ~none ~some =
    match ty with
    | TAdt { id = TAdtId id; _ } -> (
        match TypeDeclId.Map.find_opt id ctx with
        | Some fun_decl ->
            let* ptr, state = some in
            let* _, state = Symok.exec_fun fun_decl ~args:[ Ptr ptr ] ~state in
            Symok.free ptr state
        | None -> none)
    | _ -> none
end

module Wrapper = struct
  type 'meta t =
    ( Heap.Sptr.t Rust_val.t * Heap.t * 'meta,
      Error.t Heap.err,
      Heap.serialized )
    Rustsymex.Result.t

  let call (fun_decl : Frontend.fun_decl) summs :
      (ty * Heap.Sptr.t Rust_val.full_ptr) list t =
    (* Check reference arguments and allocate values on heap *)
    let* args, state, arg_ptrs =
      ListLabels.fold_left2 summs fun_decl.signature.inputs
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
    let++ ret, state = Wpst_interp.exec_fun fun_decl ~args ~state in
    (ret, state, arg_ptrs)

  let return ty summ_ctx (wrapper : (ty * Heap.Sptr.t Rust_val.full_ptr) list t)
      : (ty * Heap.Sptr.t Rust_val.full_ptr) list t =
    (* Obtain the result from the executing the function call *)
    let** ret, state, arg_ptrs = wrapper in
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
    Rustsymex.Result.ok (ret, state, arg_ptrs)

  let branch ty drop_ctx (wrapper : (ty * Heap.Sptr.t Rust_val.full_ptr) list t)
      : ty t =
    (* Obtain the result from the executing the function call *)
    let** ret, state, arg_ptrs = wrapper in
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
            Rustsymex.Result.ok (ret, state, ty)
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
            Rustsymex.Result.ok (ret, state, ty)
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
end

let leak_check = function
  | [] -> Result.ok ()
  | leaks ->
      Fmt.pr "Leaked:";
      List.iter (fun (loc, _) -> Fmt.pr " %a" Rustsymex.Value.ppa loc) leaks;
      Fmt.pr "\n";
      Result.error `MemoryLeak

let try_refute fuel drop_ctx (fun_decl : Frontend.fun_decl) summs summ_ctx =
  let ty = fun_decl.signature.output in
  (* Mimic behaviour of reference-free wrappers on function call *)
  let process =
    Wrapper.call fun_decl summs
    |> Wrapper.return ty summ_ctx
    |> Wrapper.branch ty drop_ctx
  in
  (* Symbolically execute the function call *)
  let outcomes = Rustsymex.run_needs_stats ~mode:UX ~fuel process in
  (* For each successful outcome, the summary context will be updated. *)
  ListLabels.fold_left outcomes ~init:(Result.ok summ_ctx)
    ~f:(fun acc -> function
    (* Successful termination: update the summary context *)
    | Compo_res.Ok (ret, state, ty), pcs ->
        Result.bind acc @@ fun ctx ->
        let summ, leaks = Summary.make ret pcs state in
        let update_ctx () = Summary.Context.update ty summ ctx in
        leak_check leaks |> Result.map update_ctx
    (* Unsuccessful termination: found a type unsoundness *)
    | _ -> Result.error `TypeUnsound)

let get_funs () =
  let crate = Crate.get_crate () in
  let can_infer _ (fun_decl : Frontend.fun_decl) =
    fun_decl.item_meta.is_local
    && (not fun_decl.signature.is_unsafe)
    && (not @@ Drop.is_impl fun_decl)
    && (fun_decl.item_meta.attr_info.public || not !Config.current.only_public)
  in
  FunDeclId.Map.filter can_infer crate.fun_decls

let pass_funs f fun_decls summ_ctx =
  let f _ (fun_decl : Frontend.fun_decl) acc =
    let tys =
      List.map (function TRef (_, ty, _) | ty -> ty) fun_decl.signature.inputs
    in
    let f ctx summs = Result.bind ctx @@ f fun_decl summs in
    Iter.fold f acc (Summary.Context.iter_summs tys summ_ctx)
  in
  FunDeclId.Map.fold f fun_decls (Result.ok summ_ctx)

let exec_crate (crate : Crate.t) =
  let@ () = Crate.with_crate crate in
  let config = !Config.current in
  (* Set context for calls to try_refute *)
  let try_refute =
    let drop_ctx = Drop.init_ctx () in
    let soteria_fuel =
      let open Soteria.Symex.Fuel_gauge in
      let fuel = function None -> Fuel_value.Infinite | Some i -> Finite i in
      { steps = fuel config.step_fuel; branching = fuel config.branch_fuel }
    in
    try_refute soteria_fuel drop_ctx
  in
  (* The meta-loop iterates over all safe functions *)
  let fun_decls = get_funs () in
  let rec find_unsoundness summ_ctx fuel =
    if fuel <= 0 then false (* Fuel exhausted, no unsoundness found *)
    else (* Call try_refute on all functions and accumulate the results *)
      match pass_funs try_refute fun_decls summ_ctx with
      | Ok summ_ctx -> find_unsoundness summ_ctx (fuel - 1)
      | Error _ -> true (* Type unsoundness found *)
  in
  (* Collect statistics from all the runs *)
  let Soteria.Stats.{ res = found_unsoundness; stats } =
    Rustsymex.Stats.As_ctx.with_stats () (fun () ->
        (* Run the algorithm starting from an empty summary context *)
        find_unsoundness Summary.Context.empty config.pass_fuel)
  in
  if config.print_stats then Driver.print_stats stats;
  found_unsoundness

let exec_ruxt config file_name =
  Config.set config;
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
