open Charon
include Ext_base

type 'ghost ty = 'ghost ext_ty
type 'ghost t = 'ghost ext_t

let equal = equal_ext_t
let compare = compare_ext_t
let equal_ty = equal_ext_ty
let compare_ty = compare_ext_ty
let pp_ty ft (ty : 'ghost ty) = pp_ext_ty ft ty

let pp pp ft v =
  match v with
  | Ptr (ptr, meta) -> Fmt.pf ft "Ptr(%a, %a)" pp ptr pp meta
  | PtrMeta MetaUnit -> Fmt.pf ft "()"
  | PtrMeta (MetaLen len) -> Fmt.pf ft "len(%a)" pp len
  | PtrMeta (MetaVTable vtable) -> Fmt.pf ft "vtable(%a)" pp vtable
  | ThinPtr ptr ->
      Fmt.pf ft "%a[%a]" pp ptr.ptr
        Fmt.(option ~none:(any "*") Ptr_tag.pp)
        ptr.tag
  | Enum (v, vals) ->
      Fmt.pf ft "Enum(%a: %a)" Types.pp_variant_id v
        (Fmt.list ~sep:(Fmt.any ", ") pp)
        vals
  | Tuple vals -> Fmt.pf ft "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp) vals
  | Array vals -> Fmt.pf ft "[%a]" (Iarray.pp ~sep:(Fmt.any ", ") pp) vals
  | Union vs ->
      Fmt.pf ft "Union(%a)"
        (Fmt.list ~sep:(Fmt.any ", ") (pp_block pp pp pp))
        vs
  | PolyVal tid -> Fmt.pf ft "PolyVal(%a)" Charon.Types.pp_type_var_id tid

let iter_vars_ptr iter_vars { ptr; size; align; tag = _ } =
  iter_vars ptr;
  iter_vars size;
  iter_vars align

(* TODO: derivable *)
let iter_vars iter_vars = function
  | Ptr (ptr, meta) ->
      iter_vars ptr;
      iter_vars meta
  | PtrMeta MetaUnit -> ()
  | PtrMeta (MetaLen len | MetaVTable len) -> iter_vars len
  | ThinPtr ptr -> iter_vars_ptr iter_vars ptr
  | Enum (_, vals) | Tuple vals -> List.iter iter_vars vals
  | Array vals -> Iarray.iter iter_vars vals
  | Union vs ->
      List.iter
        (fun { value; ty = _; offset; size } ->
          iter_vars value;
          iter_vars offset;
          iter_vars size)
        vs
  | PolyVal _ -> ()

(* Allocation-free structural hash *)
let[@inline] combine h x = (h * 65599) + x

let rec hash_ty : 'ghost ty -> int = function
  | TEnum ty -> combine 0 (Hashtbl.hash ty)
  | TUnion ty -> combine 1 (Hashtbl.hash ty)
  | TTuple tys ->
      List.fold_left (fun acc ty -> combine acc (Sv.hash_ty hash_ty ty)) 2 tys
  | TThinPtr -> 3
  | TFullPtr -> 4
  | TPolyType -> 5
  | TArray (ty, n) -> combine (combine 6 (Sv.hash_ty hash_ty ty)) (Z.hash n)
  | TPtrMeta -> 7

(* TODO: so derivable *)
let hash = function
  | Ptr (ptr, meta) -> combine (combine ptr.tag 1) meta.tag
  | PtrMeta MetaUnit -> combine 2 0
  | PtrMeta (MetaLen v) -> combine 2 (combine v.tag 1)
  | PtrMeta (MetaVTable v) -> combine 2 (combine v.tag 2)
  | ThinPtr { ptr; tag; size; align } ->
      combine
        (combine (combine (combine ptr.tag 3) size.tag) align.tag)
        (Option.fold ~none:(-1) ~some:Ptr_tag.hash tag)
  | Enum (var, vals) ->
      List.fold_left
        (fun acc (v : _ sv) -> combine acc v.tag)
        (combine (Types.VariantId.to_int var) 4)
        vals
  | Tuple vals ->
      List.fold_left (fun acc (v : _ sv) -> combine acc v.tag) 5 vals
  | Array vals ->
      Iarray.fold_left (fun acc (v : _ sv) -> combine acc v.tag) 8 vals
  | Union vs ->
      List.fold_left
        (fun acc { value : _ sv; ty; offset : _ sv; size : _ sv } ->
          combine
            (combine
               (combine (combine acc value.tag) (Hashtbl.hash ty))
               offset.tag)
            size.tag)
        6 vs
  | PolyVal x -> combine 7 (Types.TypeVarId.to_int x)

(* TODO: re-apply the smart constructors here *)
let mk _ty v : _ Sv.t_kind = Extension v

(* TODO: re-apply the smart constructors here *)
let eval _eval x = x

let rec apply_list apply ~missing_var s vs =
  match vs with
  | [] -> ([], s)
  | v :: vs ->
      let v, s = apply ~missing_var s v in
      let vs, s = apply_list apply ~missing_var s vs in
      (v :: vs, s)

let apply_iarray apply ~missing_var s arr =
  let n = Iarray.length arr in
  if n = 0 then (arr, s)
  else begin
    let s = ref s in
    let out =
      Iarray.map
        (fun prev ->
          let v, s' = apply ~missing_var !s prev in
          s := s';
          v)
        arr
    in
    (out, !s)
  end

let apply_subst_ptr apply ~missing_var s { ptr; size; align; tag } =
  let ptr, s = apply ~missing_var s ptr in
  let size, s = apply ~missing_var s size in
  let align, s = apply ~missing_var s align in
  ({ ptr; size; align; tag }, s)

(* TODO: derivable *)
let apply_subst apply ~missing_var s = function
  | Ptr (v, m) ->
      let v, s = apply ~missing_var s v in
      let m, s = apply ~missing_var s m in
      (Ptr (v, m), s)
  | PtrMeta MetaUnit -> (PtrMeta MetaUnit, s)
  | PtrMeta (MetaLen v) ->
      let v, s = apply ~missing_var s v in
      (PtrMeta (MetaLen v), s)
  | PtrMeta (MetaVTable v) ->
      let v, s = apply ~missing_var s v in
      (PtrMeta (MetaVTable v), s)
  | ThinPtr ptr ->
      let ptr, s = apply_subst_ptr apply ~missing_var s ptr in
      (ThinPtr ptr, s)
  | Enum (var, vs) ->
      let vs, s = apply_list apply ~missing_var s vs in
      (Enum (var, vs), s)
  | Tuple vs ->
      let vs, s = apply_list apply ~missing_var s vs in
      (Tuple vs, s)
  | Array vs ->
      let vs, s = apply_iarray apply ~missing_var s vs in
      (Array vs, s)
  | Union vs ->
      let apply ~missing_var s { value; ty; offset; size } =
        let value, s = apply ~missing_var s value in
        let offset, s = apply ~missing_var s offset in
        let size, s = apply ~missing_var s size in
        ({ value; ty; offset; size }, s)
      in
      let vs, s = apply_list apply ~missing_var s vs in
      (Union vs, s)
  | PolyVal _ as v -> (v, s)

let encode_ty _ = failwith "TODO: encode Rust ext_ty"
let encode_value _ = failwith "TODO: encode Rust ext_t"
