module Call_trace = Soteria_terminal.Call_trace
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Csymex
open State_intf.Template
module Agv = Aggregate_val

type 'a err = 'a * Cerb_location.t Call_trace.t

let add_to_call_trace (err, trace_elem) trace_elem' =
  (err, trace_elem' :: trace_elem)

module SPmap = Pmap_direct_access (struct
  include Typed
  module Symex = Csymex

  type t = T.sloc Typed.t

  let pp = ppa
  let fresh () = Csymex.nondet Typed.t_loc
end)

type t = (Block.t SPmap.t option, Globs.t) State_intf.Template.t
[@@deriving show { with_path = false }]

type serialized =
  (Block.serialized SPmap.serialized, Globs.serialized) State_intf.Template.t
[@@deriving show { with_path = false }]

let serialize (st : t) : serialized =
  let heap =
    match st.heap with
    | None -> []
    | Some st -> SPmap.serialize Block.serialize st
  in
  let globs = Globs.serialize st.globs in
  { heap; globs }

let subst_serialized (subst_var : Svalue.Var.t -> Svalue.Var.t)
    (serialized : serialized) : serialized =
  let heap =
    SPmap.subst_serialized Block.subst_serialized subst_var serialized.heap
  in
  let globs = Globs.subst_serialized subst_var serialized.globs in
  { heap; globs }

let iter_vars_serialized (s : serialized) :
    (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit =
  let iter_heap =
    SPmap.iter_vars_serialized Block.iter_vars_serialized s.heap
  in
  let iter_globs = Globs.iter_vars_serialized s.globs in
  Iter.append iter_heap iter_globs

let pp_pretty ~ignore_freed ft st =
  let ignore =
    if ignore_freed then fun (_, block) -> Block.is_freed block
    else fun _ -> false
  in
  match st.heap with
  | None -> Fmt.pf ft "Empty Heap"
  | Some st ->
      SPmap.pp ~ignore (With_origin.pp (Freeable.pp Tree_block.pp_pretty)) ft st

let empty = { heap = None; globs = Globs.empty }

let log action ptr st =
  L.debug (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>HEAP:@ %a@]" action
        Typed.ppa ptr Fmt_ail.pp_loc (get_loc ())
        (pp_pretty ~ignore_freed:true)
        st)

let with_heap st f =
  let+ res = f st.heap in
  match res with
  | Soteria_symex.Compo_res.Ok (v, h) ->
      Soteria_symex.Compo_res.Ok (v, { st with heap = h })
  | Missing fixes ->
      let fixes = List.map (fun fix -> { heap = fix; globs = [] }) fixes in
      Missing fixes
  | Error e -> Error e

let[@inline] check_non_null loc =
  if%sat Typed.Ptr.is_null_loc loc then (
    (L.debug (fun m -> m "Null dereference detected");
     Result.error `NullDereference)
    [@name "Null-deref case"])
  else Result.ok () [@name "Non-null case"]

let with_ptr (ptr : [< T.sptr ] Typed.t) (st : t)
    (f :
      ofs:[< T.sint ] Typed.t ->
      Tree_block.t option ->
      ('a * Tree_block.t option, 'err, 'fix list) Result.t) :
    ('a * t, 'err, serialized list) Result.t =
  let loc = Typed.Ptr.loc ptr in
  let ofs = Typed.Ptr.ofs ptr in
  let** () = check_non_null loc in
  let@ heap = with_heap st in
  (SPmap.wrap (With_origin.wrap (Freeable.wrap (f ~ofs)))) loc heap

let load ptr ty st =
  let@ () = with_error_loc_as_call_trace ~msg:"Triggering read" () in
  log "load" ptr st;
  with_ptr ptr st (fun ~ofs block -> Tree_block.load ofs ty block)

let load_aggregate (ptr : [< T.sptr ] Typed.t) ty state =
  let++ v, state = load ptr ty state in
  (Agv.Basic v, state)

let store ptr ty sval st =
  let@ () = with_error_loc_as_call_trace ~msg:"Triggering write" () in
  log "store" ptr st;
  with_ptr ptr st (fun ~ofs block -> Tree_block.store ofs ty sval block)

let deinit ptr len st =
  let@ () = with_error_loc_as_call_trace ~msg:"Triggering deinit" () in
  log "deinit" ptr st;
  with_ptr ptr st (fun ~ofs block -> Tree_block.deinit ofs len block)

let rec store_aggregate (ptr : [< T.sptr ] Typed.t) ty v state =
  match v with
  | Agv.Basic v -> store ptr ty v state
  | Struct values ->
      let* members, _ =
        Layout.get_struct_fields_ty ty
        |> Csymex.of_opt_not_impl ~msg:"Members of struct"
      in
      let* layout =
        Layout.layout_of ty |> Csymex.of_opt_not_impl ~msg:"Layout"
      in
      let rec aux members_ofs members values state =
        match (members_ofs, members, values) with
        | [], [], [] -> Result.ok ((), state)
        | ( (Layout.Field _, ofs) :: rest_ofs,
            (_, (_, _, _, mem_ty)) :: rest_mems,
            value :: rest_values ) ->
            let ptr = Typed.Ptr.add_ofs ptr (Typed.int ofs) in
            let** (), state = store_aggregate ptr mem_ty value state in
            aux rest_ofs rest_mems rest_values state
        | (Layout.Padding size, ofs) :: rest_ofs, members, values ->
            let ptr = Typed.Ptr.add_ofs ptr (Typed.int ofs) in
            let** (), state = deinit ptr (Typed.int size) state in
            aux rest_ofs members values state
        | _ -> failwith "Struct field mismatch"
      in
      aux layout.members_ofs members values state

let copy_nonoverlapping ~dst ~(src : [< T.sptr ] Typed.t) ~size st =
  let open Typed.Infix in
  let@ () = with_error_loc_as_call_trace ~msg:"Triggering copy" () in
  if%sat [@rname "Both pointers are non-null"]
    Typed.Ptr.is_at_null_loc dst ||@ Typed.Ptr.is_at_null_loc src
  then Result.error `NullDereference [@name "One of the pointers is null"]
  else
    let** tree_to_write, st =
      with_ptr src st (fun ~ofs block ->
          Tree_block.get_raw_tree_owned ofs size block)
    in
    with_ptr dst st (fun ~ofs block ->
        Tree_block.put_raw_tree ofs tree_to_write block)

let alloc ?(zeroed = false) size st =
  let loc = Csymex.get_loc () in
  let@ heap = with_heap st in
  let block = Block.alloc ~loc ~zeroed size in
  let** loc, st = SPmap.alloc ~new_codom:block heap in
  let ptr = Typed.Ptr.mk loc 0s in
  (* The pointer is necessarily not null *)
  let+ () = Typed.(assume [ not (loc ==@ Ptr.null_loc) ]) in
  Soteria_symex.Compo_res.ok (ptr, st)

let alloc_ty ty st =
  let* size = Layout.size_of_s ty in
  alloc size st

let free (ptr : [< T.sptr ] Typed.t) (st : t) :
    (unit * t, 'err, serialized list) Result.t =
  let@ () = with_error_loc_as_call_trace () in
  if%sat Typed.Ptr.ofs ptr ==@ 0s then
    let@ heap = with_heap st in
    (SPmap.wrap Block.free) (Typed.Ptr.loc ptr) heap
  else Result.error `InvalidFree

let error err _st =
  L.trace (fun m -> m "Using state to error!");
  let@ () = with_error_loc_as_call_trace () in
  Result.error err

let produce (serialized : serialized) (st : t) : t Csymex.t =
  L.debug (fun m -> m "Producing: %a" pp_serialized serialized);
  let non_null_locs =
    let locs =
      let heap_locs = List.to_seq serialized.heap |> Seq.map fst in
      let globs_locs = List.to_seq serialized.globs |> Seq.map snd in
      Seq.append heap_locs globs_locs
    in
    Seq.map (fun loc -> Typed.not (Typed.Ptr.null_loc ==@ loc)) locs
    |> List.of_seq
  in
  let* () = Csymex.assume non_null_locs in
  let* heap = SPmap.produce Block.produce serialized.heap st.heap in
  let+ globs = Globs.produce serialized.globs st.globs in
  { heap; globs }

let produce_basic_val loc offset ty v state =
  let block =
    With_origin.
      {
        node = Freeable.Alive [ Tree_block.TypedVal { offset; ty; v } ];
        info = None;
      }
  in
  let serialized : serialized = { heap = [ (loc, block) ]; globs = [] } in
  produce serialized state

let produce_padding loc ~offset ~len state =
  let block =
    With_origin.
      {
        node = Freeable.Alive [ Tree_block.Uninit { offset; len } ];
        info = None;
      }
  in
  let serialized : serialized = { heap = [ (loc, block) ]; globs = [] } in
  produce serialized state

let rec produce_aggregate (ptr : [< T.sptr ] Typed.t) ty (v : Agv.t) (state : t)
    =
  let open Csymex.Syntax in
  let loc = Typed.Ptr.loc ptr in
  let offset = Typed.Ptr.ofs ptr in
  match (v, ty) with
  | Basic v, _ -> produce_basic_val loc offset ty v state
  | Struct values, ty ->
      let* members, _ =
        Layout.get_struct_fields_ty ty
        |> Csymex.of_opt_not_impl ~msg:"Members of struct"
      in
      let* layout =
        Layout.layout_of ty |> Csymex.of_opt_not_impl ~msg:"Layout"
      in
      let rec aux members_ofs members values state =
        match (members_ofs, members, values) with
        | [], [], [] -> Csymex.return state
        | (Layout.Padding size, ofs) :: rest_ofs, members, values ->
            let* state =
              produce_padding loc ~offset:(Typed.int ofs) ~len:(Typed.int size)
                state
            in
            aux rest_ofs members values state
        | ( (Field _, ofs) :: rest_ofs,
            (_, (_, _, _, mem_ty)) :: rest_mems,
            value :: rest_values ) ->
            let* state =
              produce_aggregate
                (Typed.Ptr.mk loc (Typed.int ofs))
                mem_ty value state
            in
            aux rest_ofs rest_mems rest_values state
        | _ -> failwith "Struct field mismatch"
      in
      aux layout.members_ofs members values state

let consume (serialized : serialized) (st : t) :
    (t, 'err, serialized list) Csymex.Result.t =
  L.debug (fun m -> m "Consuming state from %a" pp_serialized serialized);
  let** globs =
    let+ res = Globs.consume serialized.globs st.globs in
    match res with
    | Ok globs -> Soteria_symex.Compo_res.Ok globs
    | Error e -> Error e
    | Missing fixes ->
        let fixes = List.map (fun fix -> { heap = []; globs = fix }) fixes in
        Missing fixes
  in
  let+ res = SPmap.consume Block.consume serialized.heap st.heap in
  match res with
  | Ok heap -> Soteria_symex.Compo_res.Ok { heap; globs }
  | Error e -> Error e
  | Missing fixes ->
      let fixes = List.map (fun fix -> { heap = fix; globs = [] }) fixes in
      Missing fixes

let get_global (sym : Cerb_frontend.Symbol.sym) (st : t) =
  let+ loc, globs = Globs.get sym st.globs in
  let ptr = Typed.Ptr.mk loc 0s in
  (ptr, { st with globs })
