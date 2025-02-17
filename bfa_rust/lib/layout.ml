open Charon
open Typed.Syntax

exception CantComputeLayout of Types.ty

module Archi = struct
  let word_size = 8
end

module Session = struct
  let current_crate : UllbcAst.crate ref =
    ref
      UllbcAst.
        {
          name = "";
          options =
            {
              ullbc = true;
              lib = false;
              bin = None;
              mir_promoted = false;
              mir_optimized = false;
              crate_name = None;
              input_file = None;
              read_llbc = None;
              dest_dir = None;
              dest_file = None;
              use_polonius = false;
              no_code_duplication = false;
              extract_opaque_bodies = false;
              translate_all_methods = false;
              included = [];
              opaque = [];
              exclude = [];
              remove_associated_types = [];
              hide_marker_traits = false;
              no_cargo = false;
              rustc_args = [];
              cargo_args = [];
              abort_on_error = false;
              error_on_warnings = false;
              no_serialize = false;
              print_original_ullbc = false;
              print_ullbc = false;
              print_built_llbc = false;
              print_llbc = false;
              no_merge_goto_chains = false;
            };
          declarations = [];
          type_decls = Types.TypeDeclId.Map.empty;
          fun_decls = FunDeclId.Map.empty;
          global_decls = GlobalDeclId.Map.empty;
          trait_decls = TraitDeclId.Map.empty;
          trait_impls = TraitImplId.Map.empty;
        }

  let set_crate = ( := ) current_crate
  let get_crate () = !current_crate
end

type layout = {
  size : int;
  align : int;
  members_ofs : (int * int) Array.t;
      (** Array of (member-index; offset in layout) *)
}

let is_int : Types.ty -> bool = function
  | TLiteral (TInteger _) -> true
  | _ -> false

let size_of_int_ty : Types.integer_type -> int = function
  | I128 | U128 -> 16
  | I64 | U64 -> 8
  | I32 | U32 -> 4
  | I16 | U16 -> 2
  | I8 | U8 -> 1
  | Isize | Usize -> Archi.word_size

(* TODO: this is not really accurate, but good enough for now.
   See https://doc.rust-lang.org/reference/type-layout.html#r-layout.primitive.align *)
let align_of_int_ty : Types.integer_type -> int = size_of_int_ty

let empty_generics : Types.generic_args =
  { regions = []; types = []; const_generics = []; trait_refs = [] }

let rec layout_of : Types.ty -> layout = function
  | TLiteral (TInteger inty) ->
      let size = size_of_int_ty inty in
      let align = align_of_int_ty inty in
      { size; align; members_ofs = [||] }
  | TLiteral TBool -> { size = 1; align = 1; members_ofs = [||] }
  | TAdt (TTuple, g) when g = empty_generics ->
      (* unit () *)
      { size = 1; align = 1; members_ofs = [||] }
  | TAdt (TAdtId id, g) when g = empty_generics -> (
      let crate = Session.get_crate () in
      let adt = Types.TypeDeclId.Map.find id crate.type_decls in
      match adt with
      | { kind = Struct fields; _ } ->
          let fields = List.map (fun (f : Types.field) -> f.field_ty) fields in
          layout_of_members fields
      | { kind = Enum variants; _ } ->
          (* TODO: empty enums? *)
          (* assume all discriminants are of equal size (should be ok?) *)
          let layouts =
            List.map
              (fun ({ fields; discriminant; _ } : Types.variant) ->
                let layout =
                  List.map (fun (f : Types.field) -> f.field_ty) fields
                  |> layout_of_members
                in
                let disc_size = size_of_int_ty discriminant.int_ty in
                { layout with size = layout.size + disc_size })
              variants
          in
          List.fold_left
            (fun acc l -> if l.size > acc.size then l else acc)
            (List.hd layouts) (List.tl layouts)
      | { kind; _ } ->
          Fmt.pr "Unspported ADT kind %a" Types.pp_type_decl_kind kind;
          failwith "Unsupported ADT kind")
  | ty ->
      L.debug (fun m -> m "Cannot compute layout of %a" Types.pp_ty ty);
      raise (CantComputeLayout ty)

and layout_of_members members =
  let rec aux i members_ofs (layout : layout) = function
    | [] -> (List.rev members_ofs, layout)
    | ty :: rest ->
        let { size = curr_size; align = curr_align; members_ofs = _ } =
          layout
        in
        let { size; align; _ } = layout_of ty in
        let mem_ofs = curr_size + (curr_size mod align) in
        let new_size = mem_ofs + size in
        let new_align = Int.max align curr_align in
        aux (i + 1)
          ((i, mem_ofs) :: members_ofs)
          { size = new_size; align = new_align; members_ofs = [||] }
          rest
  in
  let members_ofs, { size; align; members_ofs = _ } =
    aux 0 [] { size = 0; align = 1; members_ofs = [||] } members
  in
  {
    size = size + (size mod align);
    align;
    members_ofs = Array.of_list members_ofs;
  }

let size_of_s ty =
  try
    let { size; _ } = layout_of ty in
    Rustsymex.return (Typed.int size)
  with CantComputeLayout ty ->
    Fmt.kstr Rustsymex.not_impl "Cannot yet compute size of type %a" Types.pp_ty
      ty

let int_constraints (int_ty : Types.integer_type) =
  let open Typed.Infix in
  match int_ty with
  | I128 | I64 | I32 | I16 | I8 | Isize ->
      let size = size_of_int_ty int_ty in
      let min = Z.neg (Z.shift_left Z.one ((size * 8) - 1)) in
      let max = Z.pred (Z.shift_left Z.one ((size * 8) - 1)) in
      fun x -> [ Typed.int_z min <=@ x; x <=@ Typed.int_z max ]
  | U128 | U64 | U32 | U16 | U8 | Usize ->
      let size = size_of_int_ty int_ty in
      let max = Z.pred (Z.shift_left Z.one (size * 8)) in
      fun x -> [ 0s <=@ x; x <=@ Typed.int_z max ]

let constraints :
    Types.ty -> (Typed.T.cval Typed.t -> Typed.T.sbool Typed.t list) option =
  let open Typed.Infix in
  function
  | TNever -> Some (fun x -> [ x ==@ 0s ])
  | TRawPtr _ -> Some (fun _ -> [])
  | TLiteral (TInteger ity) ->
      let constrs = int_constraints ity in
      Some
        (fun x ->
          match Typed.cast_checked x Typed.t_int with
          | None -> [ Typed.v_false ]
          | Some x -> constrs x)
  | ty ->
      L.info (fun m ->
          m "No constraints implemented for type %a" Types.pp_ty ty);
      None

let nondet_ty : Types.ty -> Typed.T.cval Typed.t Rustsymex.t =
  let open Rustsymex.Syntax in
  function
  | TNever -> Rustsymex.return 0s
  | TRawPtr _ ->
      let* loc = Rustsymex.nondet Typed.t_loc in
      let* ofs = Rustsymex.nondet Typed.t_int in
      Rustsymex.return (Typed.Ptr.mk loc ofs)
  | TLiteral (TInteger ity) ->
      let constrs = int_constraints ity in
      let+ res = Rustsymex.nondet ~constrs Typed.t_int in
      (res :> Typed.T.cval Typed.t)
  | ty ->
      Rustsymex.not_impl
        (Fmt.str "nondet_ty: unsupported type %a" Types.pp_ty ty)
