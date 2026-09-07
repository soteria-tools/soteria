open Charon
include Ext_base

type 'ghost ty = 'ghost ext_ty
type 'ghost t = 'ghost ext_t

let equal = equal_ext_t
let compare = compare_ext_t
let equal_ty = equal_ext_ty
let compare_ty = compare_ext_ty
let pp_ty ft (ty : 'ghost ty) = pp_ext_ty ft ty

let pp_variant_name ft (var, ty) =
  let vars = Crate.as_enum (t_as_enum ty) in
  Fmt.string ft (Types.VariantId.nth vars var).variant_name

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
        (Fmt.list ~sep:(Fmt.any ", ") (pp_block pp pp pp pp))
        vs
  | PolyVal tid -> Fmt.pf ft "PolyVal(%a)" Charon.Types.pp_type_var_id tid
  | Unop (ThinPtrPart part, v) -> Fmt.pf ft "%a.%a" pp v Unop.pp_ptr_part part
  | Unop (FullPtrInner, v) -> Fmt.pf ft "thin(%a)" pp v
  | Unop (FullPtrMeta, v) -> Fmt.pf ft "meta(%a)" pp v
  | Unop (PtrMetaAs part, v) -> Fmt.pf ft "%a.as<%a>" pp v Unop.pp_ptr_meta part
  | Unop (Field i, v) -> Fmt.pf ft "%a.%d" pp v i
  | Unop (VariantField (var, i), v) ->
      Fmt.pf ft "%a.as<%a>.%d" pp v pp_variant_name (var, v.node.ty) i
  | Unop (IsVariant var, v) ->
      Fmt.pf ft "%a.is<%a>" pp v pp_variant_name (var, v.node.ty)
  | Unop (ArrayField i, v) -> Fmt.pf ft "%a[%d]" pp v i

let iter_vars_ptr iter_vars { ptr; size; align; tag = _ } =
  iter_vars ptr;
  iter_vars size;
  iter_vars align

let iter_vars_block_value iter_vars = function
  | Scalar v -> iter_vars v
  | Aggregate (ag, _ty) -> iter_vars ag

let iter_vars_block iter_vars { value; offset; size } =
  iter_vars_block_value iter_vars value;
  iter_vars offset;
  iter_vars size

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
  | Union vs -> List.iter (iter_vars_block iter_vars) vs
  | PolyVal _ -> ()
  | Unop (_, v) -> iter_vars v

let hash_ghost _ = 0
let hash v = hash_ext_t hash_ghost v
let hash_ty t = hash_ext_ty hash_ghost t

let mk build ty v =
  match v with
  | ThinPtr ptr -> mk_thin_ptr ~build ptr
  | Ptr (ptr, meta) -> mk_full_ptr ~build ptr meta
  | PtrMeta MetaUnit -> mk_unit_meta ~build ()
  | PtrMeta (MetaLen v) -> mk_len_meta ~build v
  | PtrMeta (MetaVTable v) -> mk_vtable_meta ~build v
  | Tuple vs -> mk_tuple ~build vs
  | Array vs -> mk_array_of_svty ~build (array_elem_ty ty) vs
  | Enum (v_id, vs) -> mk_enum ~build (t_as_enum ty) v_id vs
  | Union blocks -> mk_union ~build (t_as_union ty) blocks
  | PolyVal ty_id -> mk_poly ~build ty_id
  | Unop (op, v) -> apply_unop ~build op v

let eval eval v =
  match v with
  | Ptr (p, m) ->
      let p' = eval p in
      let m' = eval m in
      if p == p' && m == m' then v else Ptr (p', m')
  | PtrMeta MetaUnit -> v
  | PtrMeta (MetaLen l) ->
      let l' = eval l in
      if l == l' then v else PtrMeta (MetaLen l')
  | PtrMeta (MetaVTable vt) ->
      let vt' = eval vt in
      if vt == vt' then v else PtrMeta (MetaVTable vt')
  | ThinPtr { ptr; size; align; tag } ->
      let ptr' = eval ptr in
      let size' = eval size in
      let align' = eval align in
      if ptr == ptr' && size == size' && align == align' then v
      else ThinPtr { ptr = ptr'; size = size'; align = align'; tag }
  | Enum (var_id, vs) ->
      let vs', changed = List.map_changed eval vs in
      if not changed then v else Enum (var_id, vs')
  | Tuple vs ->
      let vs', changed = List.map_changed eval vs in
      if changed then Tuple vs' else v
  | Array vs ->
      let vs', changed = Iarray.map_changed eval vs in
      if changed then Array vs' else v
  | Union vs ->
      let vs', changed =
        List.map_changed
          (fun ({ value; offset; size } as blk) ->
            let value' =
              match value with
              | Scalar v ->
                  let v' = eval v in
                  if v == v' then value else Scalar v'
              | Aggregate (ag, ty) ->
                  let ag' = eval ag in
                  if ag == ag' then value else Aggregate (ag', ty)
            in
            let offset' = eval offset in
            let size' = eval size in
            if value == value' && offset == offset' && size == size' then blk
            else { value = value'; offset = offset'; size = size' })
          vs
      in
      if changed then Union vs' else v
  | PolyVal _ -> v
  | Unop (op, x) ->
      let x' = eval x in
      if x == x' then v else Unop (op, x')

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

let apply_block_value apply ~missing_var s = function
  | Scalar v ->
      let v, s = apply ~missing_var s v in
      (Scalar v, s)
  | Aggregate (ag, ty) ->
      let ag, s = apply ~missing_var s ag in
      (Aggregate (ag, ty), s)

let apply_block apply ~missing_var s { value; offset; size } =
  let value, s = apply_block_value apply ~missing_var s value in
  let offset, s = apply ~missing_var s offset in
  let size, s = apply ~missing_var s size in
  ({ value; offset; size }, s)

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
      let vs, s = apply_list (apply_block apply) ~missing_var s vs in
      (Union vs, s)
  | PolyVal _ as v -> (v, s)
  | Unop (op, v) ->
      let v, s = apply ~missing_var s v in
      (Unop (op, v), s)

let encode_ty = Encoding.encode_ty
let encode_value = Encoding.encode_value
