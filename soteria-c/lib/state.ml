module Call_trace = Soteria.Terminal.Call_trace
module Compo_res = Soteria.Symex.Compo_res
open Typed.Infix
open Typed.Syntax
module BV = Typed.BitVec
module T = Typed.T
open Csymex
module Agv = Aggregate_val

module Heap =
  Pmap_direct_access
    (struct
      include Typed

      type t = T.sloc Typed.t

      let concrete_loc = ref 0
      let pp = ppa

      let fresh () =
        match Config.current_mode () with
        | Lsp | ShowAil | GenSummaries | CaptureDb -> Csymex.nondet Typed.t_loc
        | ExecMain ->
            (* If we are in non-compositional execution, we can use concrete
               locations. *)
            incr concrete_loc;
            return (Typed.Ptr.loc_of_int !concrete_loc)

      let simplify = Csymex.simplify
      let to_int = unique_tag
    end)
    (Block)

type t = { heap : Heap.t option; globs : Globs.t option }
[@@deriving show { with_path = false }]

let of_opt = function None -> { heap = None; globs = None } | Some v -> v
let to_opt = function { heap = None; globs = None } -> None | t -> Some t

type serialized = State_intf.serialized =
  | Ser_heap of Heap.serialized
  | Ser_globs of Globs.serialized
[@@deriving show { with_path = false }]

module SM =
  Soteria.Sym_states.State_monad.Make
    (Csymex)
    (struct
      type nonrec t = t option
    end)

open SM.Syntax

let[@inline] with_error_loc ?msg () (f : unit -> ('a, 'b, 'c) SM.Result.t) =
  let*^ loc = Csymex.get_loc () in
  let+- e = f () in
  Error.with_trace ?msg e loc

let serialize (st : t) : serialized list =
  let heaps =
    Option.fold ~none:[] ~some:Heap.serialize st.heap
    |> List.map (fun h -> Ser_heap h)
  in
  let globs =
    Option.fold ~none:[] ~some:Globs.serialize st.globs
    |> List.map (fun g -> Ser_globs g)
  in
  heaps @ globs

let subst_serialized (subst_var : Svalue.Var.t -> Svalue.Var.t)
    (serialized : serialized) : serialized =
  match serialized with
  | Ser_heap heap -> Ser_heap (Heap.subst_serialized subst_var heap)
  | Ser_globs globs -> Ser_globs (Globs.subst_serialized subst_var globs)

let iter_vars_serialized (s : serialized) :
    (Svalue.Var.t * 'a Typed.ty -> unit) -> unit =
  match s with
  | Ser_heap heap -> Heap.iter_vars_serialized heap
  | Ser_globs globs -> Globs.iter_vars_serialized globs

let pp_pretty ~ignore_freed ft st =
  let ignore =
    if ignore_freed then fun (_, block) -> Block.is_freed block
    else fun _ -> false
  in
  match st.heap with
  | None -> Fmt.pf ft "Empty Heap"
  | Some st ->
      (* FIXME: This used to call [Ctree_block.pp_pretty], but I can't
         parametrise the heap printer anymore... *)
      Heap.pp' ~ignore ~codom:Block.pp_pretty ft st

let empty = None

let log action ptr =
  let open SM.Syntax in
  let* st = SM.get_state () in
  let+^ loc = Csymex.get_loc () in
  L.debug (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>HEAP:@ %a@]" action
        Typed.ppa ptr Fmt_ail.pp_loc loc
        (Fmt.option ~none:(Fmt.any "Empty heap") (pp_pretty ~ignore_freed:true))
        st)

let with_heap (f : ('a, 'err, Heap.serialized list) Heap.SM.Result.t) :
    ('a, 'err, serialized list) SM.Result.t =
  let open SM.Syntax in
  let* st_opt = SM.get_state () in
  let { heap; globs } = of_opt st_opt in
  let*^ res, heap = f heap in
  let+ () = SM.set_state (to_opt { heap; globs }) in
  Compo_res.map_missing res (fun fix -> List.map (fun h -> Ser_heap h) fix)

let[@inline] check_non_null loc =
  let open SM.Syntax in
  if%sat Typed.Ptr.is_null_loc loc then (
    (L.debug (fun m -> m "Null dereference detected");
     SM.Result.error `NullDereference)
    [@name "Null-deref case"])
  else SM.Result.ok () [@name "Non-null case"]

let with_ptr (ptr : [< T.sptr ] Typed.t)
    (f :
      ofs:[< T.sint ] Typed.t -> ('a, 'err, 'fix list) Ctree_block.SM.Result.t)
    : ('a, 'err, serialized list) SM.Result.t =
  let loc = Typed.Ptr.loc ptr in
  let ofs = Typed.Ptr.ofs ptr in
  let** () = check_non_null loc in
  with_heap
    (Heap.wrap loc (Block.wrap (Block.Freeable_ctree_block.wrap (f ~ofs))))

let load_basic ptr ty =
  let@ () = with_error_loc ~msg:"Invalid memory load" () in
  let load_msg = Fmt.str "load of type %a" Fmt_ail.pp_ty ty in
  let* () = log load_msg ptr in
  with_ptr ptr (fun ~ofs -> Ctree_block.load ofs ty)

let load (ptr : [< T.sptr ] Typed.t) ty :
    (Aggregate_val.t, 'err, 'fix) SM.Result.t =
  let++ v = load_basic ptr ty in
  Agv.Basic v

let store_basic ptr ty sval =
  let@ () = with_error_loc ~msg:"Invalid memory write" () in
  let* () = log "store" ptr in
  with_ptr ptr (fun ~ofs -> Ctree_block.store ofs ty sval)

let zero_range ptr len =
  let@ () = with_error_loc ~msg:"Invalid memory write (zeroing)" () in
  let* () = log "zero_range" ptr in
  if%sat len ==@ Usize.(0s) then SM.Result.ok ()
  else with_ptr ptr (fun ~ofs -> Ctree_block.zero_range ofs len)

let deinit ptr len =
  let@ () = with_error_loc ~msg:"Invalid memory write (deinitialising)" () in
  let* () = log "deinit" ptr in
  with_ptr ptr (fun ~ofs -> Ctree_block.deinit ofs len)

let rec store (ptr : [< T.sptr ] Typed.t) ty v =
  match v with
  | Agv.Basic v -> store_basic ptr ty v
  | Agv.Array elems ->
      let* elem_ty, _ =
        Layout.get_array_info ty
        |> Csymex.of_opt_not_impl ~msg:"Array element type"
        |> SM.lift
      in
      let++ _ =
        SM.Result.fold_list elems ~init:ptr ~f:(fun ptr elem ->
            let** () = store ptr elem_ty elem in
            let*^ elem_size = Layout.size_of_s elem_ty in
            let ptr = Typed.Ptr.add_ofs ptr elem_size in
            SM.Result.ok ptr)
      in
      ()
  | Struct values ->
      let* members, _ =
        Layout.get_struct_fields_ty ty
        |> Csymex.of_opt_not_impl ~msg:"Members of struct"
        |> SM.lift
      in
      let* layout =
        Layout.layout_of ty |> Csymex.of_opt_not_impl ~msg:"Layout" |> SM.lift
      in
      let rec aux members_ofs members values =
        match (members_ofs, members, values) with
        | [], [], [] -> SM.Result.ok ()
        | ( (Layout.Field _, ofs) :: rest_ofs,
            (_, (_, _, _, mem_ty)) :: rest_mems,
            value :: rest_values ) ->
            let ptr = Typed.Ptr.add_ofs ptr (BV.usizei ofs) in
            let** () = store ptr mem_ty value in
            aux rest_ofs rest_mems rest_values
        | (Layout.Padding size, ofs) :: rest_ofs, members, values ->
            let ptr = Typed.Ptr.add_ofs ptr (BV.usizei ofs) in
            let** () = deinit ptr (BV.usizei size) in
            aux rest_ofs members values
        | _ -> failwith "Struct field mismatch"
      in
      aux layout.members_ofs members values

let copy_nonoverlapping ~dst ~(src : [< T.sptr ] Typed.t) ~size =
  let open Typed.Infix in
  L.trace (fun m ->
      m "copy_nonoverlapping: copying %a bytes from %a to %a" Typed.ppa size
        Typed.ppa src Typed.ppa dst);
  let@ () = with_error_loc ~msg:"Triggering copy" () in
  let** () =
    SM.assert_or_error
      Typed.(not (Ptr.is_at_null_loc dst ||@ Ptr.is_at_null_loc src))
      `NullDereference
  in
  if%sat size ==@ Usize.(0s) then SM.Result.ok ()
  else
    let** tree_to_write =
      with_ptr src (fun ~ofs -> Ctree_block.get_raw_tree_owned ofs size)
    in
    with_ptr dst (fun ~ofs -> Ctree_block.put_raw_tree ofs tree_to_write)

let alloc ?(zeroed = false) size =
  let*^ loc = Csymex.get_loc () in
  with_heap
    (let open Heap.SM.Syntax in
     let block = Block.alloc ~loc ~zeroed size in
     let** loc = Heap.alloc ~new_codom:block in
     let ptr = Typed.Ptr.mk loc Usize.(0s) in
     (* The pointer is necessarily not null *)
     let+ () = Heap.SM.assume Typed.[ not (Ptr.is_null_loc loc) ] in
     Soteria.Symex.Compo_res.ok ptr)

let alloc_ty ty =
  let*^ size = Layout.size_of_s ty in
  alloc size

let free (ptr : [< T.sptr ] Typed.t) : (unit, 'err, serialized list) SM.Result.t
    =
  let@ () = with_error_loc ~msg:"Invalid free" () in
  if%sat Typed.Ptr.ofs ptr ==@ Usize.(0s) then
    with_heap @@ Heap.wrap (Typed.Ptr.loc ptr) (Block.free ())
  else SM.Result.error `InvalidFree

let produce (serialized : serialized) : unit SM.t =
  L.debug (fun m -> m "Producing: %a" pp_serialized serialized);
  let* st_opt = SM.get_state () in
  let { heap; globs } = of_opt st_opt in

  match serialized with
  | Ser_heap sh ->
      let* () = SM.assume [ Typed.not (Typed.Ptr.is_null_loc (fst sh)) ] in
      let*^ (), heap = Heap.produce sh heap in
      SM.set_state (to_opt { heap; globs })
  | Ser_globs sg ->
      let* () = SM.assume [ Typed.not (Typed.Ptr.is_null_loc (snd sg)) ] in
      let*^ (), globs = Globs.produce sg globs in
      SM.set_state (to_opt { heap; globs })

let produce_basic_val loc offset ty v =
  let*^ len = Layout.size_of_s ty in
  let block : Block.serialized =
    { node = Alive (MemVal { offset; len; v = SInit (v, ty) }); info = None }
  in
  let serialized : serialized = Ser_heap (loc, block) in
  produce serialized

let produce_padding loc ~offset ~len =
  let block : Block.serialized =
    { node = Alive (MemVal { offset; len; v = SUninit }); info = None }
  in
  let serialized : serialized = Ser_heap (loc, block) in
  produce serialized

let rec produce_aggregate (ptr : [< T.sptr ] Typed.t) ty (v : Agv.t) =
  let loc = Typed.Ptr.loc ptr in
  let offset = Typed.Ptr.ofs ptr in
  match (v, ty) with
  | Basic v, _ -> produce_basic_val loc offset ty v
  | Array elems, ty ->
      let* elem_ty, _ =
        Layout.get_array_info ty
        |> Csymex.of_opt_not_impl ~msg:"Array element type"
        |> SM.lift
      in
      let+ _ =
        SM.fold_list elems ~init:ptr ~f:(fun ptr elem ->
            let* () = produce_aggregate ptr elem_ty elem in
            let+^ elem_size = Layout.size_of_s elem_ty in
            Typed.Ptr.add_ofs ptr elem_size)
      in
      ()
  | Struct values, ty ->
      let* members, _ =
        Layout.get_struct_fields_ty ty
        |> Csymex.of_opt_not_impl ~msg:"Members of struct"
        |> SM.lift
      in
      let* layout =
        Layout.layout_of ty |> Csymex.of_opt_not_impl ~msg:"Layout" |> SM.lift
      in
      let rec aux members_ofs members values =
        match (members_ofs, members, values) with
        | [], [], [] -> SM.return ()
        | (Layout.Padding size, ofs) :: rest_ofs, members, values ->
            let* () =
              produce_padding loc ~offset:(BV.usizei ofs) ~len:(BV.usizei size)
            in
            aux rest_ofs members values
        | ( (Field _, ofs) :: rest_ofs,
            (_, (_, _, _, mem_ty)) :: rest_mems,
            value :: rest_values ) ->
            let* () =
              produce_aggregate (Typed.Ptr.mk loc (BV.usizei ofs)) mem_ty value
            in
            aux rest_ofs rest_mems rest_values
        | _ -> failwith "Struct field mismatch"
      in
      aux layout.members_ofs members values

(* let consume (serialized : serialized) (st : t) :
 *     (t, [> Csymex.lfail ] err, serialized) Csymex.Result.t =
 *   let@ () = with_error_loc_as_call_trace () in
 *   L.debug (fun m -> m "Consuming state from %a" pp_serialized serialized);
 *   let** globs =
 *     let+ res = Globs.consume serialized.globs st.globs in
 *     match res with
 *     | Ok globs -> Soteria.Symex.Compo_res.Ok globs
 *     | Error e -> Error e
 *     | Missing fixes ->
 *         let fixes = List.map (fun fix -> { heap = []; globs = fix }) fixes in
 *         Missing fixes
 *   in
 *   let+ res = SPmap.consume Block.consume serialized.heap st.heap in
 *   match res with
 *   | Ok heap -> Soteria.Symex.Compo_res.Ok { heap; globs }
 *   | Error e -> Error e
 *   | Missing fixes ->
 *       let fixes = List.map (fun fix -> { heap = fix; globs = [] }) fixes in
 *       Missing fixes
 *)

let get_global (sym : Cerb_frontend.Symbol.sym) =
  let* st_opt = SM.get_state () in
  let st = of_opt st_opt in
  let*^ loc, globs = Globs.get sym st.globs in
  let ptr = Typed.Ptr.mk loc Usize.(0s) in
  let+ () = SM.set_state (to_opt { st with globs }) in
  ptr
