open Soteria_rust_lib
open Heap.SM.Syntax

let from_pc (s : Subst.values) (pc : Typed.sbool Typed.t) =
  match Typed.kind pc with
  | Binop (Eq, l, r) -> (
      match (Subst.learn s l, Subst.learn s r) with
      | Known l, Known r ->
          let** () = Heap.SM.consume_pure (Subst.Value.sem_eq l r) in
          Heap.SM.Result.ok (s, [], true)
      | Known v, Outcome f_subst | Outcome f_subst, Known v ->
          let s, pcs = f_subst v in
          Heap.SM.Result.ok (s, pcs, true)
      | _ -> Heap.SM.Result.ok (s, [ pc ], false))
  | _ -> (
      match Subst.learn s Subst.Value.(Typed.kind pc <| Typed.get_ty pc) with
      | Known v ->
          let** () = Heap.SM.consume_pure (Typed.type_ v) in
          Heap.SM.Result.ok (s, [], true)
      | _ -> Heap.SM.Result.ok (s, [ pc ], false))

let from_pcs (s : Subst.values) (pcs : Typed.sbool Typed.t list) =
  Heap.SM.Result.fold_list pcs ~init:(s, [], false)
    ~f:(fun (s, pcs, consumed) pc ->
      let++ s, sub_pcs, consumed_pc = from_pc s pc in
      (s, sub_pcs @ pcs, consumed || consumed_pc))

let from_serialized (s : Subst.values) ((l, b) : Heap.serialized) =
  match Typed.kind l with
  | Var v -> (
      match Subst.find_opt v s with
      | Some v ->
          (* TODO: heap consumption should update s *)
          let** () = Heap.consume (Typed.type_ v, b) in
          Heap.SM.Result.ok (s, true)
      | None -> Heap.SM.Result.ok (s, false))
  | _ -> failwith "expected a variable"

let from_serialized_globals (s : Subst.values)
    ((heap, globals) : Heap.serialized_globals) =
  let++ s, heap, consumed =
    Heap.SM.Result.fold_list heap ~init:(s, [], false)
      ~f:(fun (s, ser, consumed) heap ->
        let** s, did_consume = from_serialized s heap in
        if did_consume then Heap.SM.Result.ok (s, ser, true)
        else Heap.SM.Result.ok (s, heap :: ser, consumed))
  in
  (s, (heap, globals), consumed)

let run (ser : Heap.serialized_globals) (pcs : Typed.sbool Typed.t list) :
    (unit, [> Rustsymex.lfail ], Heap.serialized) Heap.SM.Result.t =
  let rec iter s ser pcs =
    let** s, pcs, consumed_pc = from_pcs s pcs in
    let** s, ser, consumed_serialized = from_serialized_globals s ser in
    if consumed_pc || consumed_serialized then
      match (ser, pcs) with
      | ([], _), [] -> Heap.SM.Result.ok pcs
      | _ -> iter s ser pcs
    else Heap.SM.Result.ok pcs
  in
  let** pcs = iter Subst.empty ser pcs in
  if pcs = [] then Heap.SM.Result.ok ()
  (* TODO: Send pcs to the solver with existentials *)
    else Heap.SM.consume_false ()
