open Charon
open Common.Charon_util
open Soteria.Smt
open Ext_base

(** Small helper to lazily declare a sort. *)
let declare_sort ?(type_params = []) name f =
  Soteria.Solvers.Decls.declare ~key:name (fun k ->
      k (declare_datatype name type_params (f ())));
  Atom name

module Tuple_sort = struct
  let pp_args ft sorts = Fmt.(list ~sep:(any ", ") pp_sexp) ft sorts
  let name sorts = quote (Fmt.str "@tuple<%a>" pp_args sorts)
  let con sorts = quote (Fmt.str "@mk-tuple<%a>" pp_args sorts)
  let field_sel sorts i = quote (Fmt.str "@tuple<%a>.%d" pp_args sorts i)
  let mk sorts fields = con sorts $$. fields
  let get_field sorts i v = field_sel sorts i $. v

  let sort sorts =
    declare_sort (name sorts) (fun () ->
        [ (con sorts, List.mapi (fun i s -> (field_sel sorts i, s)) sorts) ])
end

module Enum_sort = struct
  let pp_base ft adt = Fmt.pf ft "@enum<%a>" Crate.pp_type_decl_ref adt
  let name adt = quote (Fmt.to_to_string pp_base adt)

  let variant_con adt (variant : Types.variant) =
    quote (Fmt.str "%a::%s" pp_base adt variant.variant_name)

  let field_sel adt (variant : Types.variant) i =
    quote (Fmt.str "%a::%s.%d" pp_base adt variant.variant_name i)

  let mk adt variant fields = variant_con adt variant $$. fields
  let get_field adt variant i v = field_sel adt variant i $. v
  let is_variant adt variant v = tester (variant_con adt variant) v

  let sort sort_of_ty (adt : Types.type_decl_ref) =
    let variants = Crate.as_enum adt in
    if List.is_empty variants then
      L.failwith "Cannot encode the empty enum %a to SMT-LIB"
        Crate.pp_type_decl_ref adt;
    declare_sort (name adt) (fun () ->
        variants
        |> List.map @@ fun (variant : Types.variant) ->
           let fields =
             variant.fields
             |> List.mapi @@ fun i (f : Types.field) ->
                (field_sel adt variant i, sort_of_ty (ty_of_rust f.field_ty))
           in
           (variant_con adt variant, fields))
end

module Thin_ptr_sort = struct
  module Ptr_sort = Soteria.Bv_values.Encoding.Ptr_sort

  let pp_base ft () = Fmt.pf ft "@thin-ptr<%d>" (usize_bits ())
  let name () = quote (Fmt.to_to_string pp_base ())
  let con () = quote (Fmt.str "@mk-thin-ptr<%d>" (usize_bits ()))
  let part_sel part = quote (Fmt.str "%a.%a" pp_base () Unop.pp_ptr_part part)
  let mk ptr size align = con () $$. [ ptr; size; align ]
  let get_part part v = part_sel part $. v

  let sort () =
    declare_sort (name ()) (fun () ->
        let bits = usize_bits () in
        [
          ( con (),
            [
              (part_sel PtrInner, Ptr_sort.sort bits);
              (part_sel PtrSize, t_bits bits);
              (part_sel PtrAlign, t_bits bits);
            ] );
        ])
end

module Ptr_meta_sort = struct
  let base () = Fmt.str "@ptr-meta<%d>" (usize_bits ())
  let name () = quote (base ())
  let none_con () = quote (base () ^ "::none")
  let int_con () = quote (base () ^ "::int")
  let ptr_con () = quote (base () ^ "::ptr")
  let int_sel () = quote (base () ^ "::int.0")
  let ptr_sel () = quote (base () ^ "::ptr.0")
  let mk_none () = none_con () $$. []
  let mk_int v = int_con () $. v
  let mk_ptr v = ptr_con () $. v
  let get_int v = int_sel () $. v
  let get_ptr v = ptr_sel () $. v

  let sort () =
    declare_sort (name ()) (fun () ->
        let thin_sort = Thin_ptr_sort.sort () in
        [
          (none_con (), []);
          (int_con (), [ (int_sel (), t_bits (usize_bits ())) ]);
          (ptr_con (), [ (ptr_sel (), thin_sort) ]);
        ])
end

module Full_ptr_sort = struct
  let base () = Fmt.str "@full-ptr<%d>" (usize_bits ())
  let name () = quote (base ())
  let con () = quote (Fmt.str "@mk-full-ptr<%d>" (usize_bits ()))
  let ptr_sel () = quote (base () ^ ".ptr")
  let meta_sel () = quote (base () ^ ".meta")
  let mk ptr meta = con () $$. [ ptr; meta ]
  let get_ptr v = ptr_sel () $. v
  let get_meta v = meta_sel () $. v

  let sort () =
    declare_sort (name ()) (fun () ->
        let thin_sort = Thin_ptr_sort.sort () in
        let meta_sort = Ptr_meta_sort.sort () in
        [ (con (), [ (ptr_sel (), thin_sort); (meta_sel (), meta_sort) ]) ])
end

(* More explicit than an [ignore] *)
let gen_decl = ignore

let encode_ty sort_of_ty = function
  | TTuple tys -> Tuple_sort.sort (List.map sort_of_ty tys)
  | TEnum adt -> Enum_sort.sort sort_of_ty adt
  | TArray (ty, _) -> t_seq (sort_of_ty ty)
  | TThinPtr -> Thin_ptr_sort.sort ()
  | TFullPtr -> Full_ptr_sort.sort ()
  | TPtrMeta -> Ptr_meta_sort.sort ()
  | (TUnion _ | TPolyType) as ty ->
      L.failwith "Cannot encode type %a to SMT-LIB" pp_ext_ty ty

let encode_value sort_of_ty encode ~ty = function
  | Tuple vs ->
      let tys = t_as_tuple ty in
      let sorts = List.map sort_of_ty tys in
      gen_decl (Tuple_sort.sort sorts);
      Tuple_sort.mk sorts (List.map encode vs)
  | Enum (var_id, vs) ->
      let adt = t_as_enum ty in
      gen_decl (Enum_sort.sort sort_of_ty adt);
      let variant = Types.VariantId.nth (Crate.as_enum adt) var_id in
      Enum_sort.mk adt variant (List.map encode vs)
  | Array vs ->
      let elem_ty, len = t_as_array ty in
      if Z.equal Z.zero len then as_type seq_empty (t_seq (sort_of_ty elem_ty))
      else
        Iarray.to_list vs
        |> List.map (fun v -> seq_singl (encode v))
        |> seq_concat
  | ThinPtr { ptr; size; align; tag = _ignored } ->
      gen_decl (Thin_ptr_sort.sort ());
      Thin_ptr_sort.mk (encode ptr) (encode size) (encode align)
  | Ptr (ptr, meta) ->
      gen_decl (Full_ptr_sort.sort ());
      Full_ptr_sort.mk (encode ptr) (encode meta)
  | PtrMeta MetaUnit -> Ptr_meta_sort.mk_none ()
  | PtrMeta (MetaLen v) -> Ptr_meta_sort.mk_int (encode v)
  | PtrMeta (MetaVTable v) -> Ptr_meta_sort.mk_ptr (encode v)
  | Unop (op, v) -> encode_unop sort_of_ty ~ty:v.node.ty op (encode v)
  | Union _ -> L.failwith "Cannot encode union values to SMT-LIB"
  | PolyVal _ -> L.failwith "Cannot encode polymorphic values to SMT-LIB"
