open Soteria_rust_lib
open Rustsymex.Syntax

let from_pc (s : Subst.values) (pc : Typed.sbool Typed.t) =
  match Typed.kind pc with
  | Binop (Eq, l, r) -> (
      match (Subst.learn s l, Subst.learn s r) with
      | Known l, Known r ->
          let** () = Rustsymex.consume_pure (Subst.Value.sem_eq l r) in
          Rustsymex.Result.ok (s, [], true)
      | Known v, Outcome f_subst | Outcome f_subst, Known v ->
          let s, pcs = f_subst v in
          Rustsymex.Result.ok (s, pcs, true)
      | _ -> Rustsymex.Result.ok (s, [ pc ], false))
  | _ -> (
      match Subst.learn s Subst.Value.(Typed.kind pc <| Typed.get_ty pc) with
      | Known v ->
          let** () = Rustsymex.consume_pure (Typed.type_ v) in
          Rustsymex.Result.ok (s, [], true)
      | _ -> Rustsymex.Result.ok (s, [ pc ], false))

let from_pcs (s : Subst.values) (pcs : Typed.sbool Typed.t list) =
  Rustsymex.Result.fold_list pcs ~init:(s, [], false)
    ~f:(fun (s, pcs, consumed) pc ->
      let++ s, sub_pcs, consumed_pc = from_pc s pc in
      (s, sub_pcs @ pcs, consumed || consumed_pc))

let from_serialized (s : Subst.values) ((heap, globals) : Heap.serialized) st =
  let++ s, heap, st, consumed =
    Rustsymex.Result.fold_list heap ~init:(s, [], st, false)
      ~f:(fun (s, ser, st, consumed) (l, b) ->
        match Typed.kind l with
        | Var v -> (
            match Subst.find_opt v s with
            | Some v ->
                (* TODO: heap consumption should update s *)
                let** st = Heap.consume (Typed.type_ v, b) st in
                Rustsymex.Result.ok (s, ser, st, true)
            | None -> Rustsymex.Result.ok (s, (l, b) :: ser, st, consumed))
        | _ -> failwith "expected a variable")
  in
  (s, (heap, globals), st, consumed)

let run (ser : Heap.serialized) (pcs : Typed.sbool Typed.t list) (st : Heap.t) :
    (Heap.t, [> Rustsymex.lfail ], Heap.serialized) Rustsymex.Result.t =
  let rec iter s ser pcs st =
    let** s, pcs, consumed_pc = from_pcs s pcs in
    let** s, ser, st, consumed_atom = from_serialized s ser st in
    if consumed_pc || consumed_atom then
      match (ser, pcs) with
      | ([], _), [] -> Rustsymex.Result.ok (st, pcs)
      | _ -> iter s ser pcs st
    else Rustsymex.Result.ok (st, pcs)
  in
  let** st, pcs = iter Subst.empty ser pcs st in
  if pcs = [] then Rustsymex.Result.ok st
  (* TODO: Send pcs to the solver with existentials *)
    else Rustsymex.consume_false ()
