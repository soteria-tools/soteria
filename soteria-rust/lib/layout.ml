open Charon
open Typed.Infix
open Typed.Syntax
open Rustsymex
open Rust_val
open Charon_util

exception CantComputeLayout of string * Types.ty
exception InvalidLayout of Types.ty

(** We use a custom type for the member offsets for layouts; this allows us to
    use a more efficient representation for arrays [T; N], that doesn't require
    N offsets. *)
module Fields_shape = struct
  type t =
    | Primitive  (** No fields present *)
    | Arbitrary of int Array.t [@printer Fmt.(array ~sep:comma int)]
        (** Arbitrary field placement (structs, enums...) *)
    | Array of int  (** All fields are equally spaced (arrays, slices) *)
  [@@deriving show { with_path = false }]

  let offset_of f = function
    | Primitive -> failwith "This layout has no fields"
    | Arbitrary arr -> arr.(f)
    | Array stride -> f * stride
end

type layout = { size : int; align : int; fields : Fields_shape.t }

let pp_layout fmt { size; align; fields } =
  Format.fprintf fmt "{ size = %d; align = %d; fields = %a }" size align
    Fields_shape.pp fields

module Session = struct
  (* TODO: allow different caches for different crates *)
  (** Cache of (type or variant) -> layout *)
  let layout_cache : ((Types.ty, Types.variant) Either.t, layout) Hashtbl.t =
    Hashtbl.create 128

  let get_or_compute_cached_layout ty f =
    match Hashtbl.find_opt layout_cache ty with
    | Some layout -> layout
    | None ->
        let layout = f () in
        Hashtbl.add layout_cache ty layout;
        layout

  let get_or_compute_cached_layout_ty ty =
    get_or_compute_cached_layout (Left ty)

  let get_or_compute_cached_layout_var var =
    get_or_compute_cached_layout (Right var)
end

let is_int : Types.ty -> bool = function
  | TLiteral (TInt _ | TUInt _) -> true
  | _ -> false

let size_of_int_ty : Values.int_ty -> int = function
  | I128 -> 16
  | I64 -> 8
  | I32 -> 4
  | I16 -> 2
  | I8 -> 1
  | Isize -> Crate.pointer_size ()

let size_of_uint_ty : Values.u_int_ty -> int = function
  | U128 -> 16
  | U64 -> 8
  | U32 -> 4
  | U16 -> 2
  | U8 -> 1
  | Usize -> Crate.pointer_size ()

let size_of_literal_ty : Types.literal_type -> int = function
  | TInt int_ty -> size_of_int_ty int_ty
  | TUInt uint_ty -> size_of_uint_ty uint_ty
  | TBool -> 1
  | TChar -> 4
  | TFloat F16 -> 2
  | TFloat F32 -> 4
  | TFloat F64 -> 8
  | TFloat F128 -> 16

(* TODO: this is not really accurate, but good enough for now.
   See https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_literal_ty : Types.literal_type -> int = size_of_literal_ty
let empty_generics = TypesUtils.empty_generic_args

(** If this is a dynamically sized type (requiring a fat pointer) *)
let rec is_dst : Types.ty -> bool = function
  | TAdt { id = TBuiltin (TSlice | TStr); _ } | TDynTrait _ -> true
  | TAdt { id = TAdtId id; _ } when Crate.is_struct id -> (
      match List.last_opt (Crate.as_struct id) with
      | None -> false
      | Some last -> is_dst Types.(last.field_ty))
  | _ -> false

let size_to_fit ~size ~align =
  let ( % ) = Stdlib.( mod ) in
  if size % align = 0 then size else size + align - (size % align)

let max_array_len sub_size =
  (* We calculate the max array size for a 32bit architecture, like Miri does. *)
  let isize_bits = 32 - 1 in
  if sub_size = 0 then Z.of_int isize_bits
  else Z.((one lsl isize_bits) / of_int sub_size)

let enum_discr_ty adt_id : Types.ty =
  let adt = Crate.get_adt adt_id in
  match adt.layout with
  | Some { discriminant_layout = Some { tag_ty = Signed ty; _ }; _ } ->
      TLiteral (TInt ty)
  | Some { discriminant_layout = Some { tag_ty = Unsigned ty; _ }; _ } ->
      TLiteral (TUInt ty)
  | _ -> TLiteral (TUInt Usize)

let rec layout_of (ty : Types.ty) : layout =
  Session.get_or_compute_cached_layout_ty ty @@ fun () ->
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
  (* Tuples *)
  | TAdt { id = TTuple; generics = { types; _ } } -> layout_of_members types
  (* Custom ADTs (struct, enum, etc.) *)
  | TAdt { id = TAdtId id; _ } -> (
      let adt = Crate.get_adt id in
      match adt.kind with
      | Struct fields -> layout_of_members @@ field_tys fields
      | Enum [] -> { size = 0; align = 1; fields = Arbitrary [||] }
      (* fieldless enums with one variant are zero-sized *)
      | Enum variants ->
          let layouts = List.map (of_variant id) variants in
          List.fold_left
            (fun acc l -> if l.size > acc.size then l else acc)
            (List.hd layouts) (List.tl layouts)
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
          { size; align; fields = Arbitrary fields }
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
  (* FIXME: this is wrong but at least some more code runs... *)
  | TDynTrait _ -> { size = 0; align = 1; fields = Primitive }
  (* Others (unhandled for now) *)
  | TVar _ -> raise (CantComputeLayout ("De Bruijn variable", ty))
  | TError _ -> raise (CantComputeLayout ("error type", ty))
  | TTraitType (tref, ty_name) -> layout_of @@ resolve_trait_ty tref ty_name

and layout_of_members members =
  let rec aux offsets curr_size curr_align = function
    | [] -> (List.rev offsets, curr_size, curr_align)
    | ty :: rest ->
        let { size; align; _ } = layout_of ty in
        let offset = size_to_fit ~size:curr_size ~align in
        let new_size = offset + size in
        let new_align = max align curr_align in
        aux (offset :: offsets) new_size new_align rest
  in
  let offsets, size, align = aux [] 0 1 members in
  {
    size = size_to_fit ~size ~align;
    align;
    fields = Arbitrary (Array.of_list offsets);
  }

and of_variant adt_id (variant : Types.variant) =
  Session.get_or_compute_cached_layout_var variant @@ fun () ->
  let variants = Crate.as_enum adt_id in
  if
    match variants with
    | [ _single ] ->
        List.for_all (fun ty -> (layout_of ty).size = 0)
        @@ field_tys variant.fields
    | _ -> false
  then
    let align =
      List.fold_left (fun acc ty -> max acc (layout_of ty).align) 1
      @@ field_tys variant.fields
    in
    let fields = Array.make (List.length variant.fields + 1) 0 in
    { size = 0; align; fields = Arbitrary fields }
  else
    let discr_ty = enum_discr_ty adt_id in
    let members = discr_ty :: field_tys variant.fields in
    layout_of_members members

and of_enum_variant adt_id variant =
  let variants = Crate.as_enum adt_id in
  let variant = Types.VariantId.nth variants variant in
  of_variant adt_id variant

and resolve_trait_ty (tref : Types.trait_ref) ty_name =
  match tref.trait_id with
  | TraitImpl { id; _ } -> (
      let impl = Crate.get_trait_impl id in
      match List.find_opt (fun (n, _) -> ty_name = n) impl.types with
      | Some (_, ty) -> ty
      | None ->
          let msg =
            Fmt.str "missing type '%s' in impl %a" ty_name Crate.pp_name
              impl.item_meta.name
          in
          raise (CantComputeLayout (msg, TTraitType (tref, ty_name))))
  | BuiltinOrAuto (trait, _, _) when ty_name = "Metadata" ->
      (* We need to special-case the metadata type *)
      let ty = List.hd trait.binder_value.generics.types in
      if is_dst ty then TLiteral (TInt Isize)
      else TAdt { id = TTuple; generics = TypesUtils.empty_generic_args }
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
  Typed.int size

let align_of_s ty =
  let open Rustsymex.Syntax in
  let+ { align; _ } = layout_of_s ty in
  Typed.nonzero align

(** Assumes the input is a literal type of either TInt or TUInt *)
let[@inline] is_signed : Types.literal_type -> bool = function
  | TInt _ -> true
  | _ -> false

let min_value_z : Types.literal_type -> Z.t = function
  | TUInt _ -> Z.zero
  | TInt Isize -> Z.neg (Z.shift_left Z.one ((8 * Crate.pointer_size ()) - 1))
  | TInt I128 -> Z.neg (Z.shift_left Z.one 127)
  | TInt I64 -> Z.neg (Z.shift_left Z.one 63)
  | TInt I32 -> Z.neg (Z.shift_left Z.one 31)
  | TInt I16 -> Z.neg (Z.shift_left Z.one 15)
  | TInt I8 -> Z.neg (Z.shift_left Z.one 7)
  | _ -> failwith "Invalid integer type for min_value_z"

let min_value int_ty = Typed.int_z (min_value_z int_ty)

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

let max_value int_ty = Typed.nonzero_z (max_value_z int_ty)

let size_to_uint : int -> Types.ty = function
  | 1 -> TLiteral (TUInt U8)
  | 2 -> TLiteral (TUInt U16)
  | 4 -> TLiteral (TUInt U32)
  | 8 -> TLiteral (TUInt U64)
  | 16 -> TLiteral (TUInt U128)
  | _ -> failwith "Invalid integer size"

let lit_to_unsigned lit = size_to_uint @@ size_of_literal_ty lit

let int_constraints ty =
  let min = min_value ty in
  let max = max_value ty in
  fun x -> [ min <=@ x; x <=@ max ]

let constraints :
    Types.literal_type -> [< T.cval ] Typed.t -> T.sbool Typed.t list = function
  | (TInt _ | TUInt _) as ity -> (
      let constrs = int_constraints ity in
      fun x ->
        match Typed.cast_checked x Typed.t_int with
        | None -> [ Typed.v_false ]
        | Some x -> constrs x)
  | TBool -> (
      fun x ->
        match Typed.cast_checked x Typed.t_int with
        | None -> [ Typed.v_false ]
        (* Maybe worth checking which of these is better (if it matters at all)
          | Some x -> [ x ==@ 0s ||@ (x ==@ 1s) ]) *)
        | Some x -> [ 0s <=@ x; x <=@ 1s ])
  | TChar -> (
      (* A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than
       a surrogate code point. This has a fixed numerical definition: code points are in
       the range 0 to 0x10FFFF, inclusive. Surrogate code points, used by UTF-16, are in
       the range 0xD800 to 0xDFFF.
       https://doc.rust-lang.org/std/primitive.char.html *)
      let codepoint_min = Typed.zero in
      let codepoint_max = Typed.int 0x10FFFF in
      let surrogate_min = Typed.int 0xD800 in
      let surrogate_max = Typed.int 0xDFFF in
      fun x ->
        match Typed.cast_checked x Typed.t_int with
        | None -> [ Typed.v_false ]
        | Some x ->
            [
              codepoint_min <=@ x;
              x <=@ codepoint_max;
              Typed.not (surrogate_min <=@ x &&@ (x <=@ surrogate_max));
            ])
  | TFloat (F16 | F32 | F64 | F128) -> fun _ -> []

let nondet_literal_ty (ty : Types.literal_type) : T.cval Typed.t Rustsymex.t =
  let open Rustsymex.Syntax in
  let rty =
    match ty with
    | TInt _ | TUInt _ | TBool | TChar -> Typed.t_int
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
          let disc_ty = TypesUtils.ty_as_literal @@ enum_discr_ty t_id in
          let* disc_val = nondet_literal_ty disc_ty in
          let* res =
            match_on variants ~constr:(fun (v : Types.variant) ->
                disc_val ==@ value_of_scalar v.discriminant)
          in
          match res with
          | None -> vanish ()
          | Some variant ->
              let discr = value_of_scalar variant.discriminant in
              let+ fields = nondets @@ Charon_util.field_tys variant.fields in
              Enum (discr, fields))
      | Struct fields ->
          let+ fields = nondets @@ Charon_util.field_tys fields in
          Struct fields
      | ty ->
          Rustsymex.not_impl
            (Fmt.str "nondet: unsupported type %a" Types.pp_type_decl_kind ty))
  | ty -> Rustsymex.not_impl (Fmt.str "nondet: unsupported type %a" pp_ty ty)

and nondets tys =
  let open Rustsymex.Syntax in
  Rustsymex.fold_list tys ~init:[] ~f:(fun fields ty ->
      let+ f = nondet ty in
      f :: fields)

let zeroed_lit : Types.literal_type -> T.cval Typed.t = function
  | TInt _ | TUInt _ | TBool | TChar -> 0s
  | TFloat F16 -> Typed.f16 0.0
  | TFloat F32 -> Typed.f32 0.0
  | TFloat F64 -> Typed.f64 0.0
  | TFloat F128 -> Typed.f128 0.0

let rec zeroed ~(null_ptr : 'a) : Types.ty -> 'a rust_val option =
  let zeroeds tys = Monad.OptionM.all (zeroed ~null_ptr) tys in
  function
  | TLiteral lit_ty -> ( try Some (Base (zeroed_lit lit_ty)) with _ -> None)
  | TRawPtr _ -> Some (Ptr (null_ptr, None))
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
                 Z.equal Z.zero (z_of_scalar v.discriminant))
          |> Option.bind)
          @@ fun (v : Types.variant) ->
          v.fields
          |> Charon_util.field_tys
          |> zeroeds
          |> Option.map (fun fs -> Enum (value_of_scalar v.discriminant, fs))
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
      | Enum [] -> Some (Enum (0s, []))
      | Enum [ { fields; discriminant; _ } ] ->
          as_zsts @@ Charon_util.field_tys fields
          |> Option.map (fun fs -> Enum (value_of_scalar discriminant, fs))
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
      let min = int_of_string min in
      let* v = cast_checked ~ty:Typed.t_int v in
      if%sat v >=@ Typed.int min then Result.ok ()
      else Result.error (`StdErr "rustc_layout_scalar_valid_range_start")
  | ( Base v,
      AttrUnknown
        { path = "rustc_layout_scalar_valid_range_end"; args = Some max_s } ) ->
      let max = Z.of_string max_s in
      let* v = cast_checked ~ty:Typed.t_int v in
      if%sat v <=@ Typed.int_z max then Result.ok ()
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
      match Typed.kind d with
      | Int d -> (
          let variants = Crate.as_enum adt_id in
          let v =
            List.find_opt
              (fun (v : Types.variant) ->
                Z.equal d (z_of_scalar v.discriminant))
              variants
          in
          match v with
          | Some v -> List.concat_map2 f vs (field_tys Types.(v.fields))
          | None -> [])
      | _ -> [])
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
        match_on variants ~constr:(fun (v : Types.variant) ->
            value_of_scalar v.discriminant ==?@ d)
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
  match (ty1, ty2) with
  (* Refs and raw pointers are ABI-compatible if they have the same metadata type
    FIXME: we only handle slices/strings, so we can just check if they're both DSTs;
           once we handle [dyn] we need to actually check the metadata type *)
  | (TRef (_, ty1, _) | TRawPtr (ty1, _)), (TRef (_, ty2, _) | TRawPtr (ty2, _))
    ->
      is_dst ty1 = is_dst ty2
  | TLiteral (TUInt uint1), TLiteral (TUInt uint2) ->
      size_of_uint_ty uint1 = size_of_uint_ty uint2
  | TLiteral (TInt int1), TLiteral (TInt int2) ->
      size_of_int_ty int1 = size_of_int_ty int2
  | TLiteral (TUInt U32), TLiteral TChar | TLiteral TChar, TLiteral (TUInt U32)
    ->
      true
  (* FIXME: Function pointers are compatible if they have the same ABI-string (unsupported) *)
  | TFnPtr _, TFnPtr _ -> true
  | _ ->
      let[@inline] is_zst ty =
        let layout = layout_of ty in
        layout.size = 0 && layout.align = 1
      in
      (* ZSTs with align 1 are compatible *)
      if is_zst ty1 && is_zst ty2 then true else Types.equal_ty ty1 ty2
