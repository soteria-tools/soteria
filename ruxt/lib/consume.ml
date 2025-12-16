module Typed = Soteria_rust_lib.Typed
module Rustsymex = Soteria_rust_lib.Rustsymex
open Rustsymex.Syntax

let try_pc (s : Subst.values) (pc : Typed.sbool Typed.t) =
  match Typed.kind pc with
  | Binop (Eq, l, r) -> (
      match (Subst.learn s l, Subst.learn s r) with
      | Known l, Known r ->
          let** () = Rustsymex.consume_pure (Subst.Value.sem_eq l r) in
          Rustsymex.Result.ok (Some (s, []))
      | Known v, Outcome f_subst | Outcome f_subst, Known v ->
          Rustsymex.Result.ok (Some (f_subst v))
      | _ -> Rustsymex.Result.ok None)
  | _ -> (
      match Subst.learn s Subst.Value.(Typed.kind pc <| Typed.get_ty pc) with
      | Known v ->
          let** () = Rustsymex.consume_pure (Typed.type_ v) in
          Rustsymex.Result.ok (Some (s, []))
      | _ -> Rustsymex.Result.ok None)

let iter_pcs (s : Subst.values) (pcs : Typed.sbool Typed.t list) =
  ListLabels.fold_left pcs
    ~init:(Rustsymex.Result.ok ([], None))
    ~f:(fun acc pc ->
      let** pcs, o_subst = acc in
      let s = Option.value ~default:s o_subst in
      let++ o_res = try_pc s pc in
      match o_res with
      | None -> (pc :: pcs, o_subst)
      | Some (s, new_pcs) -> (new_pcs @ pcs, Some s))

let iter_serialized (s : Subst.values) ((heap, globals) : Heap.serialized)
    (st : Heap.t) :
    ( Heap.serialized * (Subst.values * Heap.t) option,
      [> Rustsymex.lfail ],
      Heap.serialized )
    Rustsymex.Result.t =
  let++ heap, o_st =
    ListLabels.fold_left heap
      ~init:(Rustsymex.Result.ok ([], None))
      ~f:(fun acc (l, b) ->
        let** ser, o_st = acc in
        match Typed.kind l with
        | Var v -> (
            match Subst.find_opt v s with
            | Some v ->
                let s, st = Option.value ~default:(s, st) o_st in
                (* FIXME: heap consumption should update s *)
                let** st = Heap.consume (Typed.type_ v, b) st in
                Rustsymex.Result.ok (ser, Some (s, st))
            | None -> Rustsymex.Result.ok ((l, b) :: ser, o_st))
        | _ -> failwith "expected a variable")
  in
  ((heap, globals), o_st)

let rec iter ?(s = Subst.empty) ser pcs st =
  let** pcs, o_pcs = iter_pcs s pcs in
  let s = Option.value ~default:s o_pcs in
  let** ser, o_ser = iter_serialized s ser st in
  match (o_ser, o_pcs) with
  | None, None -> Rustsymex.Result.ok (st, pcs)
  | _ -> (
      let s, st = Option.value ~default:(s, st) o_ser in
      match (ser, pcs) with
      | ([], _), [] -> Rustsymex.Result.ok (st, pcs)
      | _ -> iter ~s ser pcs st)
