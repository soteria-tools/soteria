open Soteria_rust_lib
open Rustsymex.Syntax
open Soteria.Symex
open Charon.Types
module Wpst_interp = Interp.Make (Heap)

module Symok = struct
  let unwrap res =
    Rustsymex.map res (function
      | Compo_res.Ok v -> v
      | _ -> failwith "Expected Ok in wrapper")

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

  let exec_drop drops ty ~none ~some =
    match ty with
    | TAdt { id = TAdtId id; _ } -> (
        match TypeDeclId.Map.find_opt id drops with
        | Some fun_decl ->
            let* ptr, state = some in
            let* _, state = exec_fun fun_decl ~args:[ Ptr ptr ] ~state in
            free ptr state
        | None -> none)
    | _ -> none
end

type t = Summary.t list -> process

and process =
  ( Heap.Sptr.t Rust_val.t * Heap.t * ty,
    Error.t Heap.err,
    Heap.serialized )
  Rustsymex.Result.t

let call (fun_decl : Frontend.fun_decl) summs =
  let ty = fun_decl.signature.output in
  let if_rmut ~then_ ~else_ =
    let rmut_ty = match ty with TRef (_, ty, RMut) -> Some ty | _ -> None in
    Option.fold rmut_ty ~none:else_ ~some:then_
  in
  let summ = if_rmut ~then_:(fun _ -> Some (List.hd summs)) ~else_:None in
  let summs = if_rmut ~then_:(fun _ -> List.tl summs) ~else_:summs in
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
  let** ret, state = Wpst_interp.exec_fun fun_decl ~args ~state in
  (* Handle the return value if it is a reference *)
  let+ state =
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
            let* ret, state = Summary.produce (Option.get summ) state in
            Symok.store ptr ty ret state)
    | _ -> Rustsymex.return state
  in
  Compo_res.Ok (ret, state, ty, arg_ptrs)

let branch drops wrapper =
  (* Obtain the result from the executing the function call *)
  let** ret, state, ty, arg_ptrs = wrapper in
  (* Drop the return value *)
  let drop_ret state =
    Symok.exec_drop drops ty ~none:(Rustsymex.return state)
      ~some:(Symok.alloc ty ret state)
  in
  (* Drop a reference argument *)
  let drop_ptr ty ptr state =
    Symok.exec_drop drops ty ~none:(Symok.free ptr state)
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

let make drops (fun_decl : Frontend.fun_decl) : t * ty list =
  let tys =
    let sign = fun_decl.signature in
    let tys = List.map (function TRef (_, ty, _) | ty -> ty) sign.inputs in
    match sign.output with TRef (_, ty, RMut) -> ty :: tys | _ -> tys
  in
  let wrapper summs = call fun_decl summs |> branch drops in
  (wrapper, tys)

let process (wrapper : t) summs : process = wrapper summs
