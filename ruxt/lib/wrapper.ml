open Soteria_rust_lib
module State = State.Tree_state.Make (Tree_borrows.Concrete.Make)
open State.SM.Syntax
open Soteria.Symex
open Charon
module Wpst_interp = Interp.Make (State)

let exec_fun ~args fun_decl =
  let* state = State.SM.get_state () in
  let** ret, state =
    State.SM.lift @@ Wpst_interp.exec_fun ~args ~state fun_decl
  in
  let+ () = State.SM.set_state state in
  Compo_res.ok ret

module Symok = struct
  let unwrap res =
    State.SM.map res (function
      | Compo_res.Ok v -> v
      | _ -> failwith "Expected Ok in wrapper")

  let load ptr ty = State.load ptr ty |> unwrap
  let store ptr ty rv = State.store ptr ty rv |> unwrap
  let free ptr = State.free ptr |> unwrap

  let alloc ty rv =
    let* ptr = State.alloc_ty ty |> unwrap in
    let+ () = store ptr ty rv in
    ptr

  let exec_drop drops ty ~none ~some =
    match ty with
    | Types.TAdt { id = TAdtId id; _ } -> (
        match Types.TypeDeclId.Map.find_opt id drops with
        | Some fun_decl ->
            let* ptr = some in
            let* _ = exec_fun fun_decl ~args:[ Ptr ptr ] |> unwrap in
            free ptr
        | None -> none)
    | _ -> none
end

type t =
  Summary.t list ->
  ( Types.ty * Summary.Ret.t,
    Error.with_trace * Wpst_interp.StateM.st,
    State.syn list )
  State.SM.Result.t

let call (fun_decl : UllbcAst.fun_decl) summs =
  let ty = fun_decl.signature.output in
  let if_rmut ~then_ ~else_ =
    let rmut_ty = match ty with TRef (_, ty, RMut) -> Some ty | _ -> None in
    Option.fold rmut_ty ~none:else_ ~some:then_
  in
  let summ = if_rmut ~then_:(fun _ -> Some (List.hd summs)) ~else_:None in
  let summs = if_rmut ~then_:(fun _ -> List.tl summs) ~else_:summs in
  (* Check reference arguments and allocate values on heap *)
  let* args, arg_ptrs, subst =
    ListLabels.fold_left2 summs fun_decl.signature.inputs
      ~init:(State.SM.return ([], [], Typed.Expr.Subst.empty))
      ~f:(fun acc summ ty ->
        let* args, arg_ptrs, subst = acc in
        let* arg, subst = Summary.run_producer subst summ in
        match ty with
        | Types.TRef (_, ty, _) ->
            let+ ptr = Symok.alloc ty arg in
            (Rust_val.Ptr ptr :: args, (ty, ptr) :: arg_ptrs, subst)
        | _ -> State.SM.return (arg :: args, arg_ptrs, subst))
  in
  let args = List.rev args in
  (* Symbolically execute the function call *)
  let** ret = exec_fun fun_decl ~args in
  (* Handle the return value if it is a reference *)
  let+ () =
    match ty with
    | TRef (_, ty, kind) -> (
        (* The return value must be a pointer *)
        let ptr = Rust_val.as_ptr ret in
        match kind with
        | RShared ->
            (* For shared references, we simply read the return pointer *)
            let+ _ = Symok.load ptr ty in
            ()
        | RMut ->
            (* For mutable references, we write to the pointer with safe
               values*)
            let* ret, _ = Summary.run_producer subst (Option.get summ) in
            Symok.store ptr ty ret)
    | _ -> State.SM.return ()
  in
  Compo_res.Ok (ty, ret, arg_ptrs)

let branch drops wrapper =
  (* Obtain the result from the executing the function call *)
  let** ty, ret, arg_ptrs = wrapper in
  (* Drop the return value *)
  let drop_ret () =
    Symok.exec_drop drops ty ~none:(State.SM.return ())
      ~some:(Symok.alloc ty ret)
  in
  (* Drop a reference argument *)
  let drop_ptr ty ptr () =
    Symok.exec_drop drops ty ~none:(Symok.free ptr) ~some:(State.SM.return ptr)
  in
  (* For each reference, we create an execution branch that returns the stored
     value and drops everything else, including the return value *)
  let rec get_branches ?(acc = []) ?(drops = State.SM.return ()) = function
    | [] ->
        (* Case 0: we learn from the return value, the rest has been dropped *)
        let branch () =
          let* () = drops in
          State.SM.Result.ok (ty, ret)
        in
        branch :: acc
    | (ty, ptr) :: arg_ptrs ->
        (* Case 1: we learn from this reference and drop the rest *)
        let branch () =
          let* () = drops in
          let* ret = Symok.load ptr ty in
          let* () = Symok.free ptr in
          let* () =
            ListLabels.fold_left arg_ptrs ~init:(drop_ret ())
              ~f:(fun (st : unit State.SM.t) (ty, ptr) ->
                State.SM.bind st (drop_ptr ty ptr))
          in
          State.SM.Result.ok (ty, ret)
        in
        (* Case 2: we learn nothing from this reference, so we drop it *)
        let drops =
          let* () = drops in
          drop_ptr ty ptr ()
        in
        (* We keep case 1 in the result and proceed with the state from case
           2 *)
        get_branches arg_ptrs ~acc:(branch :: acc) ~drops
  in
  get_branches arg_ptrs |> State.SM.branches

let make drops (fun_decl : UllbcAst.fun_decl) : t * Types.ty list =
  let tys =
    let sign = fun_decl.signature in
    let tys =
      List.map (function Types.TRef (_, ty, _) | ty -> ty) sign.inputs
    in
    match sign.output with TRef (_, ty, RMut) -> ty :: tys | _ -> tys
  in
  let wrapper summs = call fun_decl summs |> branch drops in
  (wrapper, tys)

let exec ~fuel (wrapper : t) summs =
  (* Symbolically execute the wrapped function call *)
  State.SM.Result.run_with_state ~state:State.empty (wrapper summs)
  |> Rustsymex.run_needs_stats ~mode:UX ~fuel
  |> Result.fold_list ~init:[] ~f:(fun summs -> function
    (* Successful termination: a new summary can been inferred *)
    | Compo_res.Ok ((ty, ret), state), pcs ->
        let open Result.Syntax in
        let+ summ = Summary.make ret state pcs in
        (ty, summ) :: summs
    (* Unsuccessful termination: found a type unsoundness *)
    | _ -> Result.error `TypeUnsound)
