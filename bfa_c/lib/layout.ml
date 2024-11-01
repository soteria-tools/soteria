module CF = Cerb_frontend
open CF.Ctype
open Typed.Syntax

module Archi = struct
  let word_size = 8
end

type layout = { size : int; align : int }

module Tag_defs = struct
  type def = Cerb_location.t * Cerb_frontend.Ctype.tag_definition

  let current_defs : (CF.Symbol.sym, def) Hashtbl.t = Hashtbl.create 1020
  let cached_layouts : (CF.Ctype.ctype, layout) Hashtbl.t = Hashtbl.create 1020
  let find_opt id = Hashtbl.find_opt current_defs id

  let add_defs defs =
    List.iter
      (fun (id, (loc, _, def)) -> Hashtbl.add current_defs id (loc, def))
      defs

  let cache_layout id layout = Hashtbl.add cached_layouts id layout
  let get_cached_layout id = Hashtbl.find_opt cached_layouts id

  let () =
    Initialize_analysis.register_resetter (fun () ->
        Hashtbl.clear current_defs;
        Hashtbl.clear cached_layouts)

  let () =
    Initialize_analysis.register_before_each_initialiser (fun sigma ->
        add_defs sigma.tag_definitions)
end

let is_int (Ctype (_, ty)) =
  match ty with Basic (Integer _) -> true | _ -> false

let size_of_int_ty (int_ty : integerType) =
  CF.Ocaml_implementation.DefaultImpl.impl.sizeof_ity int_ty

let align_of_int_ty (int_ty : integerType) =
  CF.Ocaml_implementation.DefaultImpl.impl.alignof_ity int_ty

(* TODO: unsupported things need to be signaled a bit better here. *)

let rec layout_of ty =
  let open Syntaxes.Option in
  (* If cache is found, function stops *)
  let/ () = Tag_defs.get_cached_layout ty in
  let+ result =
    let (Ctype (_, ty)) = ty in
    match ty with
    | Basic (Integer inty) ->
        let* size = size_of_int_ty inty in
        let+ align = align_of_int_ty inty in
        { size; align }
    | Pointer _ -> layout_of (Ctype ([], Basic (Integer Size_t)))
    | Struct tag ->
        let* loc, def = Tag_defs.find_opt tag in
        let* members, flexible_array_member =
          match def with StructDef (m, fam) -> Some (m, fam) | _ -> None
        in
        let* () =
          (* TODO: flexible array members *)
          if Option.is_some flexible_array_member then (
            Csymex.push_give_up ("Unsupported flexible array member", loc);
            None)
          else Some ()
        in
        layout_of_members members
    | _ -> None
  in
  Tag_defs.cache_layout ty result;
  result

(** From: https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Structure-Layout.html
The structure’s fields appear in the structure layout in the order they are declared.
When possible, consecutive fields occupy consecutive bytes within the structure.
However, if a field’s type demands more alignment than it would get that way,
C gives it the alignment it requires by leaving a gap after the previous field.

Once all the fields have been laid out, it is possible to determine the structure’s alignment and size.
The structure’s alignment is the maximum alignment of any of the fields in it.
Then the structure’s size is rounded up to a multiple of its alignment.
That may require leaving a gap at the end of the structure. *)
and layout_of_members members =
  let open Syntaxes.Option in
  let rec aux layout = function
    | [] -> layout
    | (_field_name, (_attrs, _align, _quals, ty)) :: rest ->
        let* { size = curr_size; align = curr_align } = layout in
        let* { size; align } = layout_of ty in
        let new_size = curr_size + (curr_size mod align) + size in
        let new_align = Int.max align curr_align in
        aux (Some { size = new_size; align = new_align }) rest
  in
  let+ { size; align } = aux (Some { size = 0; align = 1 }) members in
  { size = size + (size mod align); align }

let size_of_s ty =
  match layout_of ty with
  | Some { size; _ } -> Csymex.return (Typed.int size)
  | None ->
      Fmt.kstr Csymex.not_impl "Cannot yet compute size of type %a"
        Fmt_ail.pp_ty ty

let int_constraints (int_ty : integerType) =
  let open Typed.Infix in
  let open Syntaxes.Option in
  match int_ty with
  | Char -> Some (fun x -> [ 0s #<= x; x #< 256s ])
  | Bool -> Some (fun x -> [ 0s #<= x; x #< 2s ])
  | Signed _ ->
      let+ size = size_of_int_ty int_ty in
      let min = Z.neg (Z.shift_left Z.one ((size * 8) - 1)) in
      let max = Z.pred (Z.shift_left Z.one ((size * 8) - 1)) in
      fun x -> [ (Typed.int_z min) #<= x; x #<= (Typed.int_z max) ]
  | Unsigned _ ->
      let+ size = size_of_int_ty int_ty in
      let max = Z.pred (Z.shift_left Z.one (size * 8)) in
      fun x -> [ 0s #<= x; x #<= (Typed.int_z max) ]
  | _ -> None
