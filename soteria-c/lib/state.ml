module Call_trace = Soteria.Terminal.Call_trace
open Typed.Infix
open Typed.Syntax
module BV = Typed.BitVec
module T = Typed.T
open Csymex
module Agv = Aggregate_val

module Heap = struct
  include
    Pmap_direct_access
      (struct
        include Typed

        type t = T.sloc Typed.t [@@deriving show { with_path = false }]
        type syn = Expr.t [@@deriving show { with_path = false }]

        let to_syn (loc : t) : syn = Expr.of_value loc
        let exprs_syn (loc : syn) : Expr.t list = [ loc ]
        let learn_eq s l = Consumer.learn_eq s l
        let subst = Expr.subst

        let fresh () =
          match Config.current_mode () with
          | Compositional -> Csymex.nondet Typed.t_loc
          | Whole_program ->
              (* If we are in non-compositional execution, we can use concrete
                 locations. *)
              return (Csymex.Concrete_alloc_id.get_next ())

        let simplify = Csymex.simplify
        let to_int = unique_tag
      end)
      (Block)

  let produce ((loc, _) as syn) st =
    let open Producer in
    let open Syntax in
    let* loc = apply_subst Typed.Expr.subst loc in
    let*^ () = assume [ Typed.not (Typed.Ptr.is_null_loc loc) ] in
    produce syn st
end

type t = { heap : Heap.t option; globs : Globs.t option }
[@@deriving sym_state { symex = Csymex; syn = State_intf.syn }]

open SM.Syntax

let[@inline] with_error_loc ?msg () (f : unit -> ('a, 'b, 'c) SM.Result.t) =
  let*^ loc = Csymex.get_loc () in
  let+- e = f () in
  Error.with_trace ?msg e loc

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

let log action ptr =
  let open SM.Syntax in
  let* st = SM.get_state () in
  let+^ loc = Csymex.get_loc () in
  [%l.debug
    "About to execute action: %s at %a (%a)@\n@[<2>HEAP:@ %a@]" action Typed.ppa
      ptr Fmt_ail.pp_loc loc
      (Fmt.option ~none:(Fmt.any "Empty heap") (pp_pretty ~ignore_freed:true))
      st]

let[@inline] check_non_null loc =
  let open SM.Syntax in
  if%sat Typed.Ptr.is_null_loc loc then (
    ([%l.debug "Null dereference detected"];
     SM.Result.error `NullDereference)
    [@name "Null-deref case"])
  else SM.Result.ok () [@name "Non-null case"]

let with_ptr (ptr : [< T.sptr ] Typed.t)
    (f :
      ofs:[< T.sint ] Typed.t -> ('a, 'err, 'fix list) Ctree_block.SM.Result.t)
    : ('a, 'err, syn list) SM.Result.t =
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
  [%l.trace
    "copy_nonoverlapping: copying %a bytes from %a to %a" Typed.ppa size
      Typed.ppa src Typed.ppa dst];
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
     Compo_res.ok ptr)

let alloc_ty ty =
  let*^ size = Layout.size_of_s ty in
  alloc size

let free (ptr : [< T.sptr ] Typed.t) : (unit, 'err, syn list) SM.Result.t =
  let@ () = with_error_loc ~msg:"Invalid free" () in
  if%sat Typed.Ptr.ofs ptr ==@ Usize.(0s) then
    with_heap @@ Heap.wrap (Typed.Ptr.loc ptr) (Block.free ())
  else SM.Result.error `InvalidFree

let produce_basic_val loc offset ty v t =
  let open Producer.Syntax in
  let*^ len = Layout.size_of_s ty in
  let len = Typed.Expr.of_value len in
  let block : Block.syn =
    { node = Alive (MemVal { offset; len; v = SInit (v, ty) }); info = None }
  in
  let syn : syn = Ser_heap (loc, block) in
  produce syn t

let produce_padding loc ~offset ~len =
  let block : Block.syn =
    { node = Alive (MemVal { offset; len; v = SUninit }); info = None }
  in
  let serialized : syn = Ser_heap (loc, block) in
  produce serialized

let rec produce_aggregate (ptr : Typed.Expr.t) ty (v : Agv.syn) (st : t option)
    : t option Producer.t =
  let open Producer in
  let open Syntax in
  let syn v = Typed.Expr.of_value v in
  let loc = Svalue.Ptr.loc ptr in
  let offset = Svalue.Ptr.ofs ptr in
  match (v, ty) with
  | Basic v, _ -> produce_basic_val loc offset ty v st
  | Array elems, ty ->
      let*^ elem_ty, _ =
        Layout.get_array_info ty
        |> Csymex.of_opt_not_impl ~msg:"Array element type"
      in
      let+ _, st =
        fold_list elems ~init:(ptr, st) ~f:(fun (ptr, st) elem ->
            let* st = produce_aggregate ptr elem_ty elem st in
            let+^ elem_size = Layout.size_of_s elem_ty in
            (Svalue.Ptr.add_ofs ptr (syn elem_size), st))
      in
      st
  | Struct values, ty ->
      let*^ members, _ =
        Layout.get_struct_fields_ty ty
        |> Csymex.of_opt_not_impl ~msg:"Members of struct"
      in
      let*^ layout =
        Layout.layout_of ty |> Csymex.of_opt_not_impl ~msg:"Layout"
      in

      let rec aux members_ofs members values st =
        match (members_ofs, members, values) with
        | [], [], [] -> return st
        | (Layout.Padding size, ofs) :: rest_ofs, members, values ->
            let* st =
              produce_padding loc
                ~offset:(syn @@ BV.usizei ofs)
                ~len:(syn @@ BV.usizei size)
                st
            in
            aux rest_ofs members values st
        | ( (Field _, ofs) :: rest_ofs,
            (_, (_, _, _, mem_ty)) :: rest_mems,
            value :: rest_values ) ->
            let* st =
              produce_aggregate
                (Svalue.Ptr.mk loc (syn @@ BV.usizei ofs))
                mem_ty value st
            in
            aux rest_ofs rest_mems rest_values st
        | _ -> failwith "Struct field mismatch"
      in
      aux layout.members_ofs members values st

let get_global (sym : Cerb_frontend.Symbol.sym) =
  let+ loc = with_globs_sym (Globs.get sym) in
  Typed.Ptr.mk loc Usize.(0s)
