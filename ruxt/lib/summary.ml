open Soteria_rust_lib
open Charon_util
open Charon.Generated_Types
open Rustsymex.Syntax

module Ret = struct
  type t = Heap.Sptr.t rust_val

  let rec iter_vars ret f =
    match ret with
    | Base v -> Typed.iter_vars v f
    | Union (_, v) -> iter_vars v f
    | Enum (_, vals) | Struct vals | Tuple vals | Array vals ->
        List.iter (fun v -> iter_vars v f) vals
    | Ptr _ | ConstFn _ -> ()

  let rec subst subst_var ret =
    let map_subst vals = List.map (subst subst_var) vals in
    match ret with
    | Base v -> Base (Typed.subst subst_var v)
    | Union (field_id, v) -> Union (field_id, subst subst_var v)
    | Enum (disc, vals) -> Enum (disc, map_subst vals)
    | Struct vals -> Struct (map_subst vals)
    | Tuple vals -> Tuple (map_subst vals)
    | Array vals -> Array (map_subst vals)
    | (Ptr _ | ConstFn _) as rv -> rv
end

type t = {
  ret : Ret.t;
  pcs : Rustsymex.Value.sbool Typed.t list;
  state : Heap.serialized;
}

let iter_vars summ f =
  Ret.iter_vars summ.ret f;
  List.iter (fun v -> Typed.iter_vars v f) summ.pcs;
  Heap.iter_vars_serialized summ.state f

let subst subst_var summ =
  let ret = Ret.subst subst_var summ.ret in
  let pcs = List.map (Typed.subst subst_var) summ.pcs in
  let state = Heap.subst_serialized subst_var summ.state in
  { ret; pcs; state }

let to_symex summ =
  let* subst_var = Subst.add_vars Subst.empty (iter_vars summ) in
  let { ret; pcs; state } = subst (Subst.to_fn subst_var) summ in
  let+ () = Rustsymex.assume pcs in
  (ret, state)

module Context = struct
  module M = Charon.Types.TypeDeclId.Map

  type nonrec t = t list M.t

  let (empty : t) = M.empty

  let update ty summ ctx =
    match ty with
    | TAdt { id = TAdtId id; _ } ->
        let opt_cons = function
          | None -> Some [ summ ]
          (* PEDRO: Check if summ is already in summs *)
          | Some summs -> Some (summ :: summs)
        in
        M.update id opt_cons ctx
    | _ -> ctx

  let get ty ctx =
    match ty with
    | TAdt { id = TAdtId id; _ } -> (
        match M.find_opt id ctx with
        | None -> []
        | Some summs -> List.map to_symex summs)
    | TLiteral lit ->
        [
          (let+ cval = Layout.nondet_literal_ty lit in
           (Base cval, Heap.serialize Heap.empty));
        ]
    | _ -> []

  let update_res f tys ctx =
    let rec aux summs ctx = function
      | [] -> Result.bind ctx (f @@ List.rev summs)
      | hd :: tl ->
          List.fold_left (fun acc summ -> aux (summ :: summs) acc tl) ctx hd
    in
    aux [] (Result.ok ctx) (List.map (fun ty -> get ty ctx) tys)
end
