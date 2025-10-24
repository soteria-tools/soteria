open Charon
open Typed.Infix
open Typed.Syntax
module BV = Typed.BitVec
open Rustsymex
open Rust_val
open Charon_util

exception CantComputeLayout of string * Types.ty
exception InvalidLayout of Types.ty

(** Layout of enum tags in memory. Note tags are distinct from discriminants: a
    discriminant is user specified and is what [Rvalue.Discriminant] returns,
    whereas a tag is specific to variant layouts, and may be of smaller size
    than the discriminant, or not be encoded at all if it is the untagged
    variant of a niche-optimised enum. *)
module Tag_layout = struct
  type encoding = Direct | Niche of Types.variant_id

  and t = {
    offset : int;
    ty : Types.literal_type; [@printer Charon_util.pp_literal_ty]
    encoding : encoding;
    tags : Z.t option Array.t;
        [@printer
          Fmt.(
            brackets @@ array ~sep:comma (option ~none:(any "none") Z.pp_print))]
        (** The tag associated to each variant, indexed by variant ID. If
            [None], the variant is either uninhabited or the untagged variant.
        *)
  }
  [@@deriving show { with_path = false }]
end

(** We use a custom type for the member offsets for layouts; this allows us to
    use a more efficient representation for arrays [T; N], that doesn't require
    N offsets. *)
module Fields_shape = struct
  type t =
    | Primitive  (** No fields present *)
    | Arbitrary of Types.variant_id * int Array.t
        (** Arbitrary field placement (structs, unions...), with the variant
            (e.g. enums with a single inhabited variant) *)
    | Enum of Tag_layout.t * t Array.t
        (** Enum fields: encodes a tag, and an array of field shapes for each
            variant (indexed by variant ID). Using [offset_of] on this isn't
            valid; one must first retrieve the fields shape of the corresponding
            variant. *)
    | Array of int  (** All fields are equally spaced (arrays, slices) *)

  let rec pp ft = function
    | Primitive -> Fmt.string ft "()"
    | Arbitrary (var, arr) ->
        Fmt.pf ft "{%a: %a}" Types.VariantId.pp_id var
          Fmt.(braces @@ array ~sep:comma int)
          arr
    | Enum (tag_layout, shapes) ->
        Fmt.pf ft "Enum (%a, %a)" Tag_layout.pp tag_layout
          Fmt.(brackets @@ array ~sep:comma pp)
          shapes
    | Array stride -> Fmt.pf ft "Array(%d)" stride

  let offset_of f = function
    | Primitive -> failwith "This layout has no fields"
    | Enum _ -> failwith "Can't get fields of enum; use `shape_for_variant`"
    | Arbitrary (_, arr) -> arr.(f)
    | Array stride -> f * stride

  let shape_for_variant variant = function
    | Enum (_, shapes) -> shapes.(Types.VariantId.to_int variant)
    | Arbitrary (v, _) as fs when Types.VariantId.equal_id v variant -> fs
    | s ->
        Fmt.failwith "Shape %a has no variant %a" pp s Types.VariantId.pp_id
          variant
end

type layout = { size : int; align : int; fields : Fields_shape.t }

let pp_layout fmt { size; align; fields } =
  Format.fprintf fmt "{ size = %d;@, align = %d;@, fields = @[%a@] }" size align
    Fields_shape.pp fields

module Session = struct
  type ty_key = Types.ty
  type variant_key = Types.type_decl_id * Types.variant

  (* TODO: allow different caches for different crates *)
  (* FIXME: inter-test mutability *)

  (** Cache of (type or variant) -> layout *)
  let layout_cache : (Types.ty, layout) Hashtbl.t = Hashtbl.create 128

  let get_or_compute_cached_layout ty f =
    match Hashtbl.find_opt layout_cache ty with
    | Some layout -> layout
    | None ->
        let layout = f () in
        Hashtbl.add layout_cache ty layout;
        layout
end

include Layout_common

let is_int : Types.ty -> bool = function
  | TLiteral (TInt _ | TUInt _) -> true
  | _ -> false

(* TODO: this is not really accurate, but good enough for now.
   See https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_literal_ty : Types.literal_type -> int = size_of_literal_ty
let empty_generics = TypesUtils.empty_generic_args

type meta_kind = LenKind | VTableKind | NoneKind

let rec dst_kind : Types.ty -> meta_kind = function
  | TAdt { id = TBuiltin TStr; _ } | TAdt { id = TBuiltin TSlice; _ } -> LenKind
  | TDynTrait _ -> VTableKind
  | TAdt { id = TAdtId id; _ } when Crate.is_struct id -> (
      match List.last_opt (Crate.as_struct id) with
      | None -> NoneKind
      | Some last -> dst_kind Types.(last.field_ty))
  | _ -> NoneKind

(** If this is a DST type with a slice tail, return the type of the slice's
    element. *)
let rec dst_slice_ty : Types.ty -> Types.ty option = function
  | TAdt { id = TBuiltin TStr; _ } -> Some (TLiteral (TUInt U8))
  | TAdt { id = TBuiltin TSlice; generics = { types = [ sub_ty ]; _ } } ->
      Some sub_ty
  | TAdt { id = TAdtId id; _ } when Crate.is_struct id -> (
      match List.last_opt (Crate.as_struct id) with
      | None -> None
      | Some last -> dst_slice_ty Types.(last.field_ty))
  | _ -> None

(** If this is a dynamically sized type (requiring a fat pointer) *)
let is_dst ty = dst_kind ty <> NoneKind

let size_to_fit ~size ~align =
  let ( % ) = Stdlib.( mod ) in
  if size % align = 0 then size else size + align - (size % align)

let max_array_len sub_size =
  (* We calculate the max array size for a 32bit architecture, like Miri does. *)
  let isize_bits = 32 - 1 in
  if sub_size = 0 then Z.of_int isize_bits
  else Z.((one lsl isize_bits) / of_int sub_size)

let rec layout_of (ty : Types.ty) : layout =
  Session.get_or_compute_cached_layout ty @@ fun () ->
  match ty with
  (* Literals *)
  | TLiteral ty ->
      let size = size_of_literal_ty ty in
      let align = align_of_literal_ty ty in
      { size; align; fields = Primitive }
  (* Fat pointers *)
  | TAdt { id = TBuiltin TBox; generics = { types = [ sub_ty ]; _ } }
  | TRef (_, sub_ty, _)
  | TRawPtr (sub_ty, _)
    when is_dst sub_ty ->
      let ptr_size = Crate.pointer_size () in
      { size = ptr_size * 2; align = ptr_size; fields = Primitive }
  (* Refs, pointers, boxes *)
  | TAdt { id = TBuiltin TBox; _ } | TRef (_, _, _) | TRawPtr (_, _) ->
      let ptr_size = Crate.pointer_size () in
      { size = ptr_size; align = ptr_size; fields = Primitive }
  (* Dynamically sized types -- we assume they have a size of 0. In truth, these types should
     simply never be allocated directly, and instead can only be obtained hidden behind
     references; however we must be able to compute their layout, to get e.g. the offset of
     the tail in a DST struct.
     FIXME: Maybe we should mark the layout as a DST, and ensure a DST layout's size is never
     used for an allocation. *)
  | TAdt { id = TBuiltin (TStr as ty); generics }
  | TAdt { id = TBuiltin (TSlice as ty); generics } ->
      let sub_ty =
        if ty = TSlice then List.hd generics.types else TLiteral (TUInt U8)
      in
      let sub_layout = layout_of sub_ty in
      { size = 0; align = sub_layout.align; fields = Array sub_layout.size }
  (* Same as above, but here we have even less information ! *)
  | TDynTrait _ -> { size = 0; align = 1; fields = Primitive }
  (* Tuples *)
  | TAdt { id = TTuple; generics = { types; _ } } -> layout_of_members types
  (* Custom ADTs (struct, enum, etc.) *)
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match adt.kind with
      | Struct fields -> layout_of_struct adt fields
      | Enum variants -> layout_of_enum adt variants
      | Union fs ->
          let layouts = List.map layout_of @@ Charon_util.field_tys fs in
          let hd = List.hd layouts in
          let tl = List.tl layouts in
          let size, align =
            List.fold_left
              (fun (size, align) l -> (max size l.size, max align l.align))
              (hd.size, hd.align) tl
          in
          let size = size_to_fit ~size ~align in
          (* All fields in the union start at 0 and overlap *)
          let fields = Array.make (List.length fs) 0 in
          { size; align; fields = Arbitrary (Types.VariantId.zero, fields) }
      | Opaque ->
          let msg = Fmt.str "opaque (%a)" Crate.pp_name adt.item_meta.name in
          raise (CantComputeLayout (msg, ty))
      | TDeclError _ -> raise (CantComputeLayout ("decl error", ty))
      | Alias _ -> raise (CantComputeLayout ("alias", ty)))
  (* Arrays *)
  | TAdt { id = TBuiltin TArray; generics } ->
      let size = List.hd generics.const_generics in
      let subty = List.hd generics.types in
      let len = Charon_util.z_of_const_generic size in
      let sub_layout = layout_of subty in
      if sub_layout.size <> 0 && Z.gt len (max_array_len sub_layout.size) then
        raise (InvalidLayout ty);
      let size = Z.(to_int (len * of_int sub_layout.size)) in
      { size; align = sub_layout.align; fields = Array sub_layout.size }
  (* Never -- zero sized type *)
  | TNever -> { size = 0; align = 1; fields = Primitive }
  (* Function definitions -- zero sized type *)
  | TFnDef _ -> { size = 0; align = 1; fields = Primitive }
  (* Function pointers (can point to a function or a state-less closure). *)
  | TFnPtr _ ->
      let ptr_size = Crate.pointer_size () in
      { size = ptr_size; align = ptr_size; fields = Primitive }
  (* Others (unhandled for now) *)
  | TPtrMetadata _ -> raise (CantComputeLayout ("pointer metadata", ty))
  | TVar _ -> raise (CantComputeLayout ("De Bruijn variable", ty))
  | TError _ -> raise (CantComputeLayout ("error type", ty))
  | TTraitType (tref, ty_name) -> layout_of @@ resolve_trait_ty tref ty_name

and layout_of_members ?(fst_size = 0) ?(fst_align = 1) members =
  let rec aux offsets curr_size curr_align = function
    | [] -> (List.rev offsets, curr_size, curr_align)
    | ty :: rest ->
        let { size; align; _ } = layout_of ty in
        let offset = size_to_fit ~size:curr_size ~align in
        let new_size = offset + size in
        let new_align = max align curr_align in
        aux (offset :: offsets) new_size new_align rest
  in
  let offsets, size, align = aux [] fst_size fst_align members in
  {
    size = size_to_fit ~size ~align;
    align;
    fields = Arbitrary (Types.VariantId.zero, Array.of_list offsets);
  }

and layout_of_struct (adt : Types.type_decl) (fields : Types.field list) =
  match adt.layout with
  | Some
      {
        variant_layouts = [ { field_offsets; _ } ];
        size = Some size;
        align = Some align;
        _;
      } ->
      {
        size;
        align;
        fields = Arbitrary (Types.VariantId.zero, Array.of_list field_offsets);
      }
  | Some { variant_layouts = [ { field_offsets; _ } ]; _ } ->
      (* we want to compute a size/align, but keep the field offsets
         this is needed for DSTs, where we're not provided a size but we definitely
         care about field positions (the size won't matter anyways since we use
         the pointer's metadata). *)
      let base = layout_of_members (field_tys fields) in
      {
        base with
        fields = Arbitrary (Types.VariantId.zero, Array.of_list field_offsets);
      }
  | _ -> layout_of_members (field_tys fields)

and layout_of_enum (adt : Types.type_decl) (variants : Types.variant list) =
  match adt.layout with
  | Some { discriminant_layout = None; variant_layouts; _ } -> (
      (* no discriminant: this means there is only one inhabited variant *)
      let inhabited =
        variant_layouts
        |> List.mapi (fun i v -> (i, v))
        |> List.filter (fun (_, (v : Types.variant_layout)) ->
               not v.uninhabited)
      in
      match inhabited with
      | [] ->
          (* no inhabited variant: uninhabited ZST *)
          let tag : Tag_layout.t =
            { offset = 0; ty = TInt I32; tags = [||]; encoding = Direct }
          in
          { size = 0; align = 1; fields = Enum (tag, [||]) }
      | [ (i, variant_layout) ] ->
          let vi = Types.VariantId.of_int i in
          let offsets = Array.of_list variant_layout.field_offsets in
          let variant = Types.VariantId.nth variants vi in
          let sub_layout = layout_of_members (field_tys variant.fields) in
          {
            size = sub_layout.size;
            align = sub_layout.align;
            fields = Arbitrary (vi, offsets);
          }
      | _ ->
          Fmt.failwith
            "More than one inhabited variant, but no discriminant layout? For \
             %a"
            Crate.pp_name adt.item_meta.name)
  | Some { discriminant_layout = Some _; _ } | None -> (
      let tags =
        match adt.layout with
        | Some { variant_layouts; _ } ->
            Monad.ListM.map variant_layouts (fun v ->
                Option.map z_of_scalar v.tag)
        | None ->
            Monad.ListM.map variants (fun v ->
                Some (z_of_literal v.discriminant))
      in
      let tag_layout : Tag_layout.t =
        match adt.layout with
        | Some { discriminant_layout = None; _ } -> failwith "Handled above"
        | Some { discriminant_layout = Some discr_layout; _ } ->
            (* there's a discriminant to handle *)
            let ty = lit_of_int_ty discr_layout.tag_ty in
            let offset = discr_layout.offset in
            let encoding : Tag_layout.encoding =
              match discr_layout.encoding with
              | Niche v -> Niche v
              | Direct -> Direct
            in
            { offset; ty; tags = Array.of_list tags; encoding }
        | None ->
            (* best effort: we assume direct encoding *)
            let ty : Types.literal_type =
              match variants with
              | [] -> TInt I32 (* Shouldn't matter *)
              | v :: _ -> lit_ty_of_lit v.discriminant
            in
            { offset = 0; ty; tags = Array.of_list tags; encoding = Direct }
      in
      let variant_layouts =
        match adt.layout with
        | Some { variant_layouts; size = Some size; align = Some align; _ } ->
            let variant_layouts =
              List.mapi
                (fun i v -> (Types.VariantId.of_int i, v))
                variant_layouts
            in
            Monad.ListM.map variant_layouts (fun (i, v) ->
                {
                  size;
                  align;
                  fields = Arbitrary (i, Array.of_list v.field_offsets);
                })
        | _ ->
            Monad.ListM.map variants (fun v ->
                layout_of_members (field_tys v.fields))
      in
      match variant_layouts with
      (* no variants: uninhabited ZST *)
      | [] -> { size = 0; align = 1; fields = Enum (tag_layout, [||]) }
      (* one ZST variant: inhabited ZST *)
      | [ { size = 0; align; fields } ] ->
          { size = 0; align; fields = Enum (tag_layout, [| fields |]) }
      (* N variants: realign variants with prepended tag (if not niche),
     use biggest and most aligned *)
      | _ ->
          let variant_layouts =
            match tag_layout.encoding with
            | Direct ->
                (* if we need to prepend the tag, we recompute the layout to consider its
               size and alignement (there probably is a smarter way to do this). *)
                let discr_layout = layout_of (TLiteral tag_layout.ty) in
                let layout_adjusted =
                  layout_of_members ~fst_size:discr_layout.size
                    ~fst_align:discr_layout.align
                in
                Monad.ListM.map variants (fun v ->
                    layout_adjusted (field_tys v.fields))
            | Niche _ -> variant_layouts
          in
          let size, align =
            List.fold_left
              (fun (size, align) l -> (max size l.size, max align l.align))
              (0, 1) variant_layouts
          in
          let fields = List.map (fun v -> v.fields) variant_layouts in
          { size; align; fields = Enum (tag_layout, Array.of_list fields) })

and resolve_trait_ty (tref : Types.trait_ref) ty_name =
  match tref.kind with
  | TraitImpl { id; _ } -> (
      let impl = Crate.get_trait_impl id in
      match List.find_opt (fun (n, _) -> ty_name = n) impl.types with
      | Some (_, ty) -> ty.binder_value.value
      | None ->
          let msg =
            Fmt.str "missing type '%s' in impl %a" ty_name Crate.pp_name
              impl.item_meta.name
          in
          raise (CantComputeLayout (msg, TTraitType (tref, ty_name))))
  | _ ->
      let msg = Fmt.str "trait type (%s)" ty_name in
      raise (CantComputeLayout (msg, TTraitType (tref, ty_name)))

let offset_in_array ty idx =
  let sub_layout = layout_of ty in
  idx * sub_layout.size

let layout_of_s ty =
  try return @@ layout_of ty with
  | CantComputeLayout (msg, ty') ->
      Fmt.kstr Rustsymex.not_impl
        "Cannot compute layout: %s@.%a@.Occurred when computing:@.%a" msg pp_ty
        ty' pp_ty ty
  | Crate.MissingDecl decl_ty ->
      Fmt.kstr Rustsymex.not_impl
        "Cannot compute layout: missing %s declaration@.Occured when computing \
         %a"
        decl_ty pp_ty ty

let size_of_s ty =
  let open Rustsymex.Syntax in
  let+ { size; _ } = layout_of_s ty in
  BV.usizei size

let align_of_s ty =
  let open Rustsymex.Syntax in
  let+ { align; _ } = layout_of_s ty in
  BV.usizeinz align

let min_value_z : Types.literal_type -> Z.t = function
  | TUInt _ -> Z.zero
  | TInt Isize -> Z.neg (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))
  | TInt I128 -> Z.neg (Z.shift_left Z.one 127)
  | TInt I64 -> Z.neg (Z.shift_left Z.one 63)
  | TInt I32 -> Z.neg (Z.shift_left Z.one 31)
  | TInt I16 -> Z.neg (Z.shift_left Z.one 15)
  | TInt I8 -> Z.neg (Z.shift_left Z.one 7)
  | _ -> failwith "Invalid integer type for min_value_z"

let max_value_z : Types.literal_type -> Z.t = function
  | TUInt U128 -> Z.pred (Z.shift_left Z.one 128)
  | TUInt U64 -> Z.pred (Z.shift_left Z.one 64)
  | TUInt U32 -> Z.pred (Z.shift_left Z.one 32)
  | TUInt U16 -> Z.pred (Z.shift_left Z.one 16)
  | TUInt U8 -> Z.pred (Z.shift_left Z.one 8)
  | TUInt Usize -> Z.pred (Z.shift_left Z.one (8 * Crate.pointer_size ()))
  | TInt I128 -> Z.pred (Z.shift_left Z.one 127)
  | TInt I64 -> Z.pred (Z.shift_left Z.one 63)
  | TInt I32 -> Z.pred (Z.shift_left Z.one 31)
  | TInt I16 -> Z.pred (Z.shift_left Z.one 15)
  | TInt I8 -> Z.pred (Z.shift_left Z.one 7)
  | TInt Isize -> Z.pred (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))
  | _ -> failwith "Invalid integer type for max_value_z"

let size_to_uint : int -> Types.ty = function
  | 1 -> TLiteral (TUInt U8)
  | 2 -> TLiteral (TUInt U16)
  | 4 -> TLiteral (TUInt U32)
  | 8 -> TLiteral (TUInt U64)
  | 16 -> TLiteral (TUInt U128)
  | _ -> failwith "Invalid integer size"

let lit_to_unsigned lit = size_to_uint @@ size_of_literal_ty lit

(* when using BitVector values, there are no constraints as they're encoded in the BV size
  let int_constraints ty =
  let min = min_value ty in
  let max = max_value ty in
  fun x -> [ min <=@ x; x <=@ max ] *)

let constraints :
    Types.literal_type -> [< T.cval ] Typed.t -> T.sbool Typed.t list = function
  | TInt _ | TUInt _ | TFloat (F16 | F32 | F64 | F128) -> fun _ -> []
  | TBool ->
      fun x ->
        (* Maybe worth checking which of these is better (if it matters at all)
           [ x ==@ 0s ||@ (x ==@ 1s) ]) *)
        let x = Typed.cast_lit TBool x in
        [ U8.(0s) <=@ x; (x <=@ U8.(1s)) ]
  | TChar ->
      (* A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than
       a surrogate code point. This has a fixed numerical definition: code points are in
       the range 0 to 0x10FFFF, inclusive. Surrogate code points, used by UTF-16, are in
       the range 0xD800 to 0xDFFF.
       https://doc.rust-lang.org/std/primitive.char.html *)
      let codepoint_min = U32.(0s) in
      let codepoint_max = U32.(0x10FFFFs) in
      let surrogate_min = U32.(0xD800s) in
      let surrogate_max = U32.(0xDFFFs) in
      fun x ->
        let x = Typed.cast_lit TChar x in
        [
          codepoint_min <=@ x;
          x <=@ codepoint_max;
          Typed.not (surrogate_min <=@ x &&@ (x <=@ surrogate_max));
        ]

let nondet_literal_ty (ty : Types.literal_type) : T.cval Typed.t Rustsymex.t =
  let open Rustsymex.Syntax in
  let rty =
    match ty with
    | TInt _ | TUInt _ | TBool | TChar -> Typed.t_int (size_of_literal_ty ty * 8)
    | TFloat F16 -> Typed.t_f16
    | TFloat F32 -> Typed.t_f32
    | TFloat F64 -> Typed.t_f64
    | TFloat F128 -> Typed.t_f128
  in
  let constrs = constraints ty in
  let* v = Rustsymex.nondet rty in
  let+ () = Rustsymex.assume (constrs v) in
  v

(** [nondet ~extern ~init ty] returns a nondeterministic value for [ty], along
    with some "state". It receives a function [extern] to get an optional
    external function that computes the arbitrary value; it tries using it, and
    otherwise guesses the valid values. [init] is the initial "state", that is
    modified and returned by [extern]. *)
let rec nondet ty : 'a rust_val Rustsymex.t =
  let open Rustsymex.Syntax in
  match ty with
  | Types.TLiteral lit ->
      let+ cval = nondet_literal_ty lit in
      Base cval
  | TAdt { id = TTuple; generics = { types; _ } } ->
      let+ fields = nondets types in
      Tuple fields
  | TAdt
      {
        id = TBuiltin TArray;
        generics = { const_generics = [ len ]; types = [ ty ]; _ };
      } ->
      let size = Charon_util.int_of_const_generic len in
      let+ fields = nondets @@ List.init size (fun _ -> ty) in
      Array fields
  | TAdt { id = TAdtId t_id; _ } -> (
      let type_decl = Crate.get_adt t_id in
      match type_decl.kind with
      | Enum variants -> (
          let layout = layout_of ty in
          let tag_layout =
            match layout.fields with
            | Fields_shape.Enum (tag_layout, _) -> tag_layout
            | _ -> failwith "Expected enum layout"
          in
          let* d = nondet_literal_ty tag_layout.ty in
          let* res =
            match_on variants ~constr:(fun v ->
                BV.of_literal v.discriminant ==@ d)
          in
          match (res, tag_layout.encoding) with
          | Some variant, _ ->
              let discr = BV.of_literal variant.discriminant in
              let+ fields = nondets @@ Charon_util.field_tys variant.fields in
              Enum (discr, fields)
          | None, Direct -> vanish ()
          | None, Niche untagged ->
              let variant = Types.VariantId.nth variants untagged in
              let discr = BV.of_literal variant.discriminant in
              let+ fields = nondets @@ Charon_util.field_tys variant.fields in
              Enum (discr, fields))
      | Struct fields ->
          let+ fields = nondets @@ Charon_util.field_tys fields in
          Struct fields
      | ty ->
          Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a"
            Types.pp_type_decl_kind ty)
  | ty -> Fmt.kstr Rustsymex.not_impl "nondet: unsupported type %a" pp_ty ty

and nondets tys =
  let open Rustsymex.Syntax in
  Rustsymex.fold_list tys ~init:[] ~f:(fun fields ty ->
      let+ f = nondet ty in
      f :: fields)

let zeroed_lit : Types.literal_type -> T.cval Typed.t = function
  | TFloat fty -> Typed.Float.mk fty "0.0"
  | (TInt _ | TUInt _ | TBool | TChar) as ty -> BV.mki_lit ty 0

let rec zeroed ~(null_ptr : 'a) : Types.ty -> 'a rust_val option =
  let zeroeds tys = Monad.OptionM.all (zeroed ~null_ptr) tys in
  function
  | TLiteral lit_ty -> Some (Base (zeroed_lit lit_ty))
  | TRawPtr _ -> Some (Ptr (null_ptr, Thin))
  | TFnPtr _ -> None
  | TRef _ -> None
  | TAdt { id = TTuple; generics = { types; _ } } ->
      zeroeds types |> Option.map (fun fields -> Tuple fields)
  | TAdt
      {
        id = TBuiltin TArray;
        generics = { types = [ ty ]; const_generics = [ len ]; _ };
      } ->
      let len = int_of_const_generic len in
      zeroed ~null_ptr ty
      |> Option.map (fun v -> Array (List.init len (fun _ -> v)))
  | TAdt { id = TAdtId t_id; _ } -> (
      let adt = Crate.get_adt t_id in
      match adt.kind with
      | Struct fields ->
          fields
          |> Charon_util.field_tys
          |> zeroeds
          |> Option.map (fun fields -> Struct fields)
      | Enum vars ->
          (vars
          |> List.find_opt (fun (v : Types.variant) ->
                 Z.equal Z.zero (z_of_literal v.discriminant))
          |> Option.bind)
          @@ fun (v : Types.variant) ->
          v.fields
          |> Charon_util.field_tys
          |> zeroeds
          |> Option.map (fun fs -> Enum (BV.of_literal v.discriminant, fs))
      | Union fs ->
          let layouts =
            List.mapi
              (fun i (f : Types.field) -> (f.field_ty, i, layout_of f.field_ty))
              fs
          in
          let ty, i, _ =
            List.fold_left
              (fun ((_, _, accl) as acc) ((_, _, l) as cur) ->
                if l.size > accl.size then cur else acc)
              (List.hd layouts) (List.tl layouts)
          in
          let field = Types.FieldId.of_int i in
          zeroed ~null_ptr ty |> Option.map (fun v -> Union (field, v))
      | k ->
          Fmt.failwith "Unhandled zeroed ADT kind: %a" Types.pp_type_decl_kind k
      )
  | ty -> Fmt.failwith "Unhandled zeroed type: %a" pp_ty ty

let rec is_inhabited : Types.ty -> bool = function
  | TNever -> false
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match adt.kind with
      | Struct fs -> List.for_all is_inhabited @@ Charon_util.field_tys fs
      | Union fs -> List.exists is_inhabited @@ Charon_util.field_tys fs
      | Enum [] -> false
      | Enum vars ->
          List.exists
            (fun (v : Types.variant) ->
              List.for_all is_inhabited @@ Charon_util.field_tys v.fields)
            vars
      | _ -> true)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      List.for_all is_inhabited types
  | _ -> true

(** Returns the given type as it's unique representant if it's a ZST; otherwise
    [None].

    FIXME: giltho: this plays awfully with polymorphism. *)
let rec as_zst : Types.ty -> 'a rust_val option =
  let as_zsts tys = Monad.OptionM.all as_zst tys in
  function
  | TNever -> Some (Tuple [])
  | TAdt { id = TBuiltin TArray; generics = { const_generics = [ len ]; _ } }
    when int_of_const_generic len = 0 ->
      Some (Array [])
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match adt.kind with
      | Struct fs ->
          as_zsts @@ Charon_util.field_tys fs
          |> Option.map (fun fs -> Struct fs)
      | Union _ -> None
      | Enum [] -> None (* an empty enum is uninhabited *)
      | Enum [ { fields; discriminant; _ } ] ->
          as_zsts @@ Charon_util.field_tys fields
          |> Option.map (fun fs -> Enum (BV.of_literal discriminant, fs))
      | Enum _ -> None
      | _ -> None)
  | TAdt { id = TTuple; generics = { types; _ } } ->
      as_zsts types |> Option.map (fun fs -> Tuple fs)
  | TFnDef binder -> Some (ConstFn binder.binder_value)
  | _ -> None

(** Apply the compiler-attribute to the given value *)
let apply_attribute v attr =
  let open Rustsymex.Syntax in
  match (v, attr) with
  | ( Base v,
      Meta.AttrUnknown
        { path = "rustc_layout_scalar_valid_range_start"; args = Some min } ) ->
      let min = Z.of_string min in
      let v, bits = Typed.cast_int v in
      if%sat v >=@ BV.mk bits min then Result.ok ()
      else Result.error (`StdErr "rustc_layout_scalar_valid_range_start")
  | ( Base v,
      AttrUnknown
        { path = "rustc_layout_scalar_valid_range_end"; args = Some max_s } ) ->
      let max = Z.of_string max_s in
      let v, bits = Typed.cast_int v in
      if%sat v <=@ BV.mk bits max then Result.ok ()
      else Result.error (`StdErr "rustc_layout_scalar_valid_range_end")
  | _ -> Result.ok ()

let apply_attributes v attributes =
  Result.fold_list attributes ~f:(fun () -> apply_attribute v) ~init:()

let rec is_unsafe_cell : Types.ty -> bool = function
  | TAdt { id = TAdtId adt_id; _ } -> (
      let adt = Crate.get_adt adt_id in
      if adt.item_meta.lang_item = Some "unsafe_cell" then true
      else
        match adt.kind with
        | Struct fs | Union fs -> List.exists is_unsafe_cell (field_tys fs)
        | Enum vs ->
            Iter.exists is_unsafe_cell
            @@ Iter.flat_map_l (fun (v : Types.variant) -> field_tys v.fields)
            @@ Iter.of_list vs
        | _ -> false)
  | TAdt { id = TBuiltin (TArray | TSlice); generics = { types = [ ty ]; _ } }
    ->
      is_unsafe_cell ty
  | TAdt { id = TTuple; generics = { types; _ } } ->
      List.exists is_unsafe_cell types
  | _ -> false

(** Traverses the given type and rust value, and returns all findable references
    with their type (ignores pointers, except if [include_ptrs] is true). This
    is needed e.g. when needing to get the pointers along with the size of their
    pointee, in particular in nested cases. *)
let rec ref_tys_in ?(include_ptrs = false) (v : 'a rust_val) (ty : Types.ty) :
    ('a full_ptr * Types.ty) list =
  let f = ref_tys_in ~include_ptrs in
  match (v, ty) with
  | Ptr ptr, (TAdt { id = TBuiltin TBox; _ } | TRef _) ->
      [ (ptr, get_pointee ty) ]
  | Ptr ptr, TRawPtr _ when include_ptrs -> [ (ptr, get_pointee ty) ]
  | Base _, _ -> []
  | Struct vs, TAdt { id = TAdtId adt_id; _ } ->
      let fields = Crate.as_struct adt_id in
      List.concat_map2 f vs (field_tys fields)
  | ( Array vs,
      TAdt { id = TBuiltin (TArray | TSlice); generics = { types = [ ty ]; _ } }
    ) ->
      List.concat_map (fun v -> f v ty) vs
  | Tuple vs, TAdt { id = TTuple; generics = { types; _ } } ->
      List.concat_map2 f vs types
  | Enum (d, vs), TAdt { id = TAdtId adt_id; _ } -> (
      match BV.to_z d with
      | Some d -> (
          let variants = Crate.as_enum adt_id in
          let v =
            List.find_opt
              (fun (v : Types.variant) ->
                Z.equal d (z_of_literal v.discriminant))
              variants
          in
          match v with
          | Some v -> List.concat_map2 f vs (field_tys Types.(v.fields))
          | None -> [])
      | None -> [])
  | Union (fid, v), TAdt { id = TAdtId adt_id; _ } ->
      let fields = Crate.as_union adt_id in
      let field = Types.FieldId.nth fields fid in
      f v field.field_ty
  | _ -> []

let rec update_ref_tys_in
    (fn :
      'acc ->
      'a full_ptr ->
      Types.ty ->
      Types.ref_kind ->
      ('a full_ptr * 'acc, 'e, 'f) Result.t) (init : 'acc) (v : 'a rust_val)
    (ty : Types.ty) : ('a rust_val * 'acc, 'e, 'f) Result.t =
  let open Rustsymex.Syntax in
  let f = update_ref_tys_in fn in
  let fs acc vs ty =
    let++ vs, acc =
      Result.fold_list vs ~init:([], acc) ~f:(fun (vs, acc) v ->
          let++ v, acc = f acc v ty in
          (v :: vs, acc))
    in
    (List.rev vs, acc)
  in
  let fs2 acc vs tys =
    let vs = List.combine vs tys in
    let++ vs, acc =
      Result.fold_list vs ~init:([], acc) ~f:(fun (vs, acc) (v, ty) ->
          let++ v, acc = f acc v ty in
          (v :: vs, acc))
    in
    (List.rev vs, acc)
  in
  match (v, ty) with
  | Ptr ptr, TRef (_, _, rk) ->
      let++ ptr, acc = fn init ptr (get_pointee ty) rk in
      (Ptr ptr, acc)
  | Struct vs, TAdt { id = TAdtId adt_id; _ } ->
      let fields = Crate.as_struct adt_id in
      let++ vs, acc = fs2 init vs (field_tys fields) in
      (Struct vs, acc)
  | ( Array vs,
      TAdt { id = TBuiltin (TArray | TSlice); generics = { types = [ ty ]; _ } }
    ) ->
      let++ vs, acc = fs init vs ty in
      (Array vs, acc)
  | Tuple vs, TAdt { id = TTuple; generics = { types; _ } } ->
      let++ vs, acc = fs2 init vs types in
      (Tuple vs, acc)
  | Enum (d, vs), TAdt { id = TAdtId adt_id; _ } -> (
      let variants = Crate.as_enum adt_id in
      let* var =
        match_on variants ~constr:(fun v -> BV.of_literal v.discriminant ==@ d)
      in
      match var with
      | Some var ->
          let++ vs, acc = fs2 init vs (field_tys Types.(var.fields)) in
          (Enum (d, vs), acc)
      | None -> Result.ok (v, init))
  | Union (fid, v), TAdt { id = TAdtId adt_id; _ } ->
      let fields = Crate.as_union adt_id in
      let field = Types.FieldId.nth fields fid in
      let++ v, acc = f init v field.field_ty in
      (Union (fid, v), acc)
  | v, _ -> Result.ok (v, init)

(** [is_abi_compatible ty1 ty2] is true if a function expecting an argument of
    type [ty1] can be called with an argument of type [ty2].

    The full specification is available at:
    https://doc.rust-lang.org/nightly/std/primitive.fn.html#abi-compatibility *)
let is_abi_compatible (ty1 : Types.ty) (ty2 : Types.ty) =
  let is_ptr_like : Types.ty -> bool = function
    | TRef _ | TRawPtr _ -> true
    | TAdt { id = TBuiltin TBox; _ } -> true
    | TAdt { id = TAdtId id; _ } ->
        let adt = Crate.get_adt id in
        adt.item_meta.lang_item = Some "owned_box"
        || Charon_util.meta_get_attr adt.item_meta "rustc_diagnostic_item"
           = Some "NonNull"
    | _ -> false
  in
  match (ty1, ty2) with
  (* Refs and raw pointers are ABI-compatible if they have the same metadata type *)
  | (TRef (_, ty1, _) | TRawPtr (ty1, _)), (TRef (_, ty2, _) | TRawPtr (ty2, _))
    ->
      dst_kind ty1 = dst_kind ty2
  | TLiteral (TUInt uint1), TLiteral (TUInt uint2) ->
      size_of_uint_ty uint1 = size_of_uint_ty uint2
  | TLiteral (TInt int1), TLiteral (TInt int2) ->
      size_of_int_ty int1 = size_of_int_ty int2
  | TLiteral (TUInt U32), TLiteral TChar | TLiteral TChar, TLiteral (TUInt U32)
    ->
      true
  (* We keep this later down to avoid the check for everything *)
  | ty1, ty2 when is_ptr_like ty1 && is_ptr_like ty2 -> true
  (* FIXME: Function pointers are compatible if they have the same ABI-string (unsupported) *)
  | TFnPtr _, TFnPtr _ -> true
  | _ ->
      let[@inline] is_zst ty =
        let layout = layout_of ty in
        layout.size = 0 && layout.align = 1
      in
      (* ZSTs with align 1 are compatible *)
      (is_zst ty1 && is_zst ty2) || Types.equal_ty ty1 ty2
