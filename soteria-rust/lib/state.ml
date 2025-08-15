open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Rustsymex
module Sptr = Sptr.ArithPtr
module Encoder = Encoder.Make (Sptr)

type 'a err = 'a * Charon.Meta.span Soteria_terminal.Call_trace.t

let add_to_call_trace (err, trace_elem) trace_elem' =
  (err, trace_elem' :: trace_elem)

let with_error_loc_as_call_trace ?(msg = "Triggering memory operation") st f =
  let open Rustsymex.Syntax in
  let+- err, loc = f () in
  ((err, Soteria_terminal.Call_trace.singleton ~loc ~msg ()), st)

module StateKey = struct
  include Typed
  module Symex = Rustsymex.SYMEX
  (* FIXME: Rustsymex.SYMEX instead of just Rustsymex because Rustsymex overrides the L module right now. *)

  type t = T.sloc Typed.t

  let pp = ppa

  (* let fresh ?constrs () = Rustsymex.nondet ?constrs Typed.t_loc *)
  let indices = ref 0

  (* We know all keys are distinct, so we avoid the extra assertion *)
  let distinct _ = Typed.v_true

  (* This *only* works in WPST!!! *)
  let fresh () =
    incr indices;
    let idx = !indices in
    Rustsymex.return (Ptr.loc_of_int idx)
end

module SPmap = Pmap_direct_access (StateKey)
module Tree_block = Rtree_block.Make (Sptr)

type global = String of string | Global of Charon.Types.global_decl_id
[@@deriving show { with_path = false }, ord]

module GlobMap = struct
  include Map.Make (struct
    type t = global

    let compare = compare_global
  end)

  let pp pp_v fmt m =
    let pp_pair = Fmt.pair ~sep:(Fmt.any " -> ") pp_global pp_v in
    (Fmt.iter_bindings ~sep:Fmt.comma iter pp_pair) fmt m
end

module FunBiMap = struct
  module LocMap = Map.Make (struct
    type t = T.sloc Typed.t

    let compare = Typed.compare
  end)

  module FunMap = Map.Make (struct
    type t = Charon.Types.fn_ptr

    let compare = Charon.Types.compare_fn_ptr
  end)

  type t = Charon.Types.fn_ptr LocMap.t * T.sloc Typed.t FunMap.t

  let empty = (LocMap.empty, FunMap.empty)

  let add loc fn_ptr (lmap, fmap) =
    let lmap' = LocMap.add loc fn_ptr lmap in
    let fmap' = FunMap.add fn_ptr loc fmap in
    (lmap', fmap')

  let get_fn loc (lmap, _) = LocMap.find_opt loc lmap
  let get_loc fn (_, fmap) = FunMap.find_opt fn fmap

  let pp fmt (lmap, _) =
    let pp_pair =
      Fmt.pair ~sep:(Fmt.any " -> ") Typed.ppa Charon.Types.pp_fn_ptr
    in
    (Fmt.iter_bindings ~sep:Fmt.comma LocMap.iter pp_pair) fmt lmap
end

type sub = Tree_block.t * Tree_borrow.t

and t = {
  state : sub Freeable.t SPmap.t option;
  functions : FunBiMap.t;
  globals : Sptr.t Rust_val.full_ptr GlobMap.t;
  errors : Error.t err list; [@printer Fmt.list Error.pp_err_and_call_trace]
}
[@@deriving show { with_path = false }]

type serialized = Tree_block.serialized Freeable.serialized SPmap.serialized
[@@deriving show { with_path = false }]

let pp_pretty ~ignore_freed ft { state; _ } =
  let ignore =
    if ignore_freed then function _, Freeable.Freed -> true | _ -> false
    else fun _ -> false
  in
  match state with
  | None -> Fmt.pf ft "Empty State"
  | Some st ->
      SPmap.pp ~ignore
        (Freeable.pp (fun fmt (tb, _) -> Tree_block.pp_pretty fmt tb))
        ft st

let empty =
  {
    state = None;
    functions = FunBiMap.empty;
    globals = GlobMap.empty;
    errors = [];
  }

let log action ptr st =
  L.trace (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>STATE:@ %a@]" action
        Sptr.pp ptr Charon_util.pp_span (get_loc ())
        (pp_pretty ~ignore_freed:true)
        st)

let with_state st f =
  let open Soteria_symex.Compo_res in
  let+ res = f st.state in
  match res with
  | Ok (v, h) -> Ok (v, { st with state = h })
  | Missing fixes -> Missing fixes
  | Error e -> Error e

let with_tbs b f =
  let block, tree_borrow =
    match b with
    | None -> (None, Tree_borrow.init ~state:UB ())
    | Some (block, tb) -> (Some block, tb)
  in
  let+ res = f (block, tree_borrow) in
  match res with
  | Soteria_symex.Compo_res.Ok (v, block) ->
      let block = Option.map (fun b -> (b, tree_borrow)) block in
      Soteria_symex.Compo_res.Ok (v, block)
  | Missing fixes -> Missing fixes
  | Error e -> Error e

let check_ptr_align (ptr : Sptr.t) ty st =
  let@ () = with_error_loc_as_call_trace st in
  let* expected_align = Layout.align_of_s ty in
  let loc, ofs = Typed.Ptr.decompose ptr.ptr in
  (* 0-based pointers are aligned up to their offset *)
  let align = Typed.ite (loc ==@ Typed.Ptr.null_loc) expected_align ptr.align in
  L.debug (fun m ->
      m "Checking pointer alignment of %a: ofs %a mod %a / expect %a for %a"
        Sptr.pp ptr Typed.ppa ofs Typed.ppa align Typed.ppa expected_align
        Charon_util.pp_ty ty);
  if%sat ofs %@ expected_align ==@ 0s &&@ (align %@ expected_align ==@ 0s) then
    Result.ok ((), st)
  else error `MisalignedPointer

let with_ptr (ptr : Sptr.t) (st : t)
    (f :
      [< T.sint ] Typed.t * sub option ->
      ('a * sub option, 'err, 'fix list) Result.t) :
    ('a * t, 'err, serialized list) Result.t =
  if%sat Sptr.sem_eq ptr Sptr.null_ptr then Result.error `NullDereference
  else
    let loc, ofs = Typed.Ptr.decompose ptr.ptr in
    let@ state = with_state st in
    let* res = (SPmap.wrap (Freeable.wrap (fun st -> f (ofs, st)))) loc state in
    match res with
    | Missing _ as miss ->
        if%sat Sptr.is_at_null_loc ptr then Result.error `UBDanglingPointer
        else return miss
    | ok_or_err -> return ok_or_err

(** This is used as a stopgap for cases where a function pointer is cast to a
    regular pointer and is used on the state; the location won't exist in tree
    block, and we don't want to add it there (I think), but we don't want to
    crash, so we just ignore the action.

    For instance, if a function pointer hiding as a pointer is passed to a
    function, protecting it should do nothing, and should be allowed. *)
let with_opt_or (x : 'a option) (otherwise : 'b)
    (f : 'a -> ('b * 'a option, 'err, 'f) Result.t) :
    ('b * 'a option, 'err, 'f) Result.t =
  match x with Some v -> f v | None -> Result.ok (otherwise, None)

let uninit (ptr, _) ty st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "uninit" ptr st;
  let* size = Layout.size_of_s ty in
  let@ ofs, block = with_ptr ptr st in
  let@ block, _ = with_tbs block in
  Tree_block.uninit_range ofs size block

let load ?(is_move = false) ?(ignore_borrow = false) (ptr, meta) ty st =
  let** (), st = check_ptr_align ptr ty st in
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "load" ptr st;
  let@ ofs, block = with_ptr ptr st in
  let@ block, tb = with_tbs block in
  L.debug (fun f ->
      f "Recursively reading %a from block tree at %a:@.%a" Charon_util.pp_ty ty
        Sptr.pp ptr
        Fmt.(option ~none:(any "None") Tree_block.pp)
        block);
  let handler (ty, ofs) block =
    L.debug (fun f ->
        f "Loading blocks %a:%a" Typed.ppa ofs Charon_util.pp_ty ty);
    Tree_block.load ~is_move ~ignore_borrow ofs ty ptr.tag tb block
  in
  let parser = Encoder.rust_of_cvals ~offset:ofs ?meta ty in
  let++ value, block = Encoder.ParserMonad.parse ~init:block ~handler parser in
  L.debug (fun f ->
      f "Finished reading rust value %a" (Rust_val.pp Sptr.pp) value);
  (value, block)

(** Performs a load at the tree borrow level, by updating the borrow state,
    without attempting to validate the values or checking uninitialised memory
    accesses; all of these are ignored. *)
let tb_load (ptr, _) ty st =
  let* size = Layout.size_of_s ty in
  if%sat size ==@ 0s then Result.ok ((), st)
  else
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    log "tb_load" ptr st;
    let@ ofs, block = with_ptr ptr st in
    let@ block, tb = with_tbs block in
    Tree_block.tb_access ofs size ptr.tag tb block

(** Performs a side-effect free ghost read -- this does not modify the state or
    the tree-borrow state. Returns true if the value was read successfully,
    false otherwise. *)
let is_valid_ptr st ptr ty =
  L.debug (fun m -> m "The following read is a GHOST read");
  let+ res = load ~ignore_borrow:true ptr ty st in
  match res with Ok _ -> true | _ -> false

let store (ptr, _) ty sval st =
  let parts = Encoder.rust_to_cvals sval ty in
  if List.is_empty parts then Result.ok ((), st)
  else
    let** (), st = check_ptr_align ptr ty st in
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    L.debug (fun f ->
        f "Parsed to parts [%a]"
          Fmt.(list ~sep:comma Encoder.pp_cval_info)
          parts);
    log "store" ptr st;
    let@ ofs, block = with_ptr ptr st in
    let@ block, tb = with_tbs block in
    let* size = Layout.size_of_s ty in
    (* We uninitialise the whole range before writing, to ensure padding bytes are copied if
           there are any. *)
    let** (), block = Tree_block.uninit_range ofs size block in
    Result.fold_list parts ~init:((), block)
      ~f:(fun ((), block) { value; ty; offset } ->
        Tree_block.store (offset +@ ofs) ty value ptr.tag tb block)

let copy_nonoverlapping ~dst:(dst, _) ~src:(src, _) ~size st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let** tree_to_write, st =
    let@ ofs, block = with_ptr src st in
    let@ block, _ = with_tbs block in
    let++ tree, _ = Tree_block.get_raw_tree_owned ofs size block in
    (tree, block)
  in
  let@ ofs, block = with_ptr dst st in
  let@ block, _ = with_tbs block in
  (* We need to be careful about tree borrows; the tree we copy has a tree borrow state inside,
     but we cannot copy that, since those tags don't belong to this allocation!
     Instead, we must first get the tree borrow states for the original area of memory,
     and update the copied tree with them.
     We only want to update the values in the tree, not the tree borrow state. *)
  let module Tree = Tree_block.Tree in
  let** original_tree, _ = Tree_block.get_raw_tree_owned ofs size block in
  (* Iterator over the tree borrow states in a tree. *)
  let collect_tb_states f =
    Tree.iter_leaves_rev original_tree @@ fun leaf ->
    match leaf.node with
    | Owned (_, tb) ->
        let range =
          Tree_block.Range.offset leaf.range ~-(fst original_tree.range)
        in
        f (tb, range)
    | NotOwned Totally -> failwith "Impossible: we framed the range"
    | NotOwned Partially ->
        failwith "Impossible: iterating over an intermediate node"
  in
  (* Update a tree and its children with the given tree borrow state. *)
  let put_tb tb t =
    let rec aux tb (t : Tree.t) =
      match t.node with
      | NotOwned _ -> failwith "Impossible: checked before"
      | Owned (v, _) ->
          let children =
            Option.map (fun (l, r) -> (aux tb l, aux tb r)) t.children
          in
          { t with children; node = Owned (v, tb) }
    in
    try Result.ok (aux tb t) with Failure msg -> not_impl msg
  in
  (* Applies all the tree borrow ranges to the tree we're writing, overwriting all
     previous states. *)
  let** tree_to_write =
    Result.fold_iter collect_tb_states ~init:tree_to_write
      ~f:(fun tree (tb, range) ->
        let replace_node = put_tb tb in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range ~rebuild_parent ~replace_node tree range
        in
        tree)
  in
  Tree_block.put_raw_tree ofs tree_to_write block

let alloc ?zeroed size align st =
  (* Commenting this out as alloc cannot fail *)
  (* let@ () = with_loc_err () in*)
  let@ state = with_state st in
  let@ () = with_error_loc_as_call_trace st in
  let tb = Tree_borrow.init ~state:Unique () in
  let block = Tree_block.alloc ?zeroed size in
  let block = Freeable.Alive (block, tb) in
  let** loc, state = SPmap.alloc ~new_codom:block state in
  let ptr = Typed.Ptr.mk loc 0s in
  let ptr : Sptr.t Rust_val.full_ptr =
    ({ ptr; tag = tb.tag; align; size }, None)
  in
  (* The pointer is necessarily not null *)
  let+ () = assume [ Typed.(not (loc ==@ Ptr.null_loc)) ] in
  Soteria_symex.Compo_res.ok (ptr, state)

let alloc_untyped ?zeroed ~size ~align st = alloc ?zeroed size align st

let alloc_ty ty st =
  let* layout = Layout.layout_of_s ty in
  alloc (Typed.int layout.size) (Typed.nonzero layout.align) st

let alloc_tys tys st =
  let@ state = with_state st in
  let@ () = with_error_loc_as_call_trace st in
  SPmap.allocs state ~els:tys ~fn:(fun ty loc ->
      (* make Tree_block *)
      let* layout = Layout.layout_of_s ty in
      let size = Typed.int layout.size in
      let tb = Tree_borrow.init ~state:Unique () in
      let block = Freeable.Alive (Tree_block.alloc size, tb) in
      (* create pointer *)
      let+ () = assume [ Typed.(not (loc ==@ Ptr.null_loc)) ] in
      let ptr = Typed.Ptr.mk loc 0s in
      let ptr : Sptr.t =
        {
          ptr;
          tag = tb.tag;
          align = Typed.nonzero layout.align;
          size = Typed.int layout.size;
        }
      in
      (block, (ptr, None)))

let free (({ ptr; _ } : Sptr.t), _) ({ state; _ } as st) =
  let@ () = with_error_loc_as_call_trace st in
  if%sat Typed.Ptr.ofs ptr ==@ 0s then
    let@ () = with_loc_err () in
    (* TODO: does the tag not play a role in freeing? *)
    let++ (), state =
      SPmap.wrap
        (Freeable.free ~assert_exclusively_owned:(fun t ->
             Tree_block.assert_exclusively_owned @@ Option.map fst t))
        (Typed.Ptr.loc ptr) state
    in
    ((), { st with state })
  else error `InvalidFree

let zeros (ptr, _) size st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "zeroes" ptr st;
  let@ ofs, block = with_ptr ptr st in
  let@ block, _ = with_tbs block in
  Tree_block.zero_range ofs size block

let error err st =
  let@ () = with_error_loc_as_call_trace st in
  L.info (fun m -> m "State errored: %a" Error.pp err);
  error err

let lift_err st (symex : ('a, 'e, 'f) Result.t) =
  let* res = symex in
  match res with
  | Error e -> error e st
  | Ok ok -> Result.ok ok
  | Missing fix -> Result.miss fix

let store_str_global str ptr ({ globals; _ } as st) =
  let globals = GlobMap.add (String str) ptr globals in
  Result.ok ((), { st with globals })

let store_global g ptr ({ globals; _ } as st) =
  let globals = GlobMap.add (Global g) ptr globals in
  Result.ok ((), { st with globals })

let load_str_global str ({ globals; _ } as st) =
  let ptr = GlobMap.find_opt (String str) globals in
  Result.ok (ptr, st)

let load_global g ({ globals; _ } as st) =
  let ptr = GlobMap.find_opt (Global g) globals in
  Result.ok (ptr, st)

let borrow (ptr, meta) (ty : Charon.Types.ty)
    (kind : Charon.Expressions.borrow_kind) st =
  (* &UnsafeCell<T> are treated as raw pointers, and reuse parent's tag! *)
  if Tree_borrow.is_disabled () || (kind = BShared && Layout.is_unsafe_cell ty)
  then Result.ok ((ptr, meta), st)
  else
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    let@ _, block = with_ptr ptr st in
    let* tag_st =
      match kind with
      | BShared -> return Tree_borrow.Frozen
      | (BTwoPhaseMut | BMut) when Layout.is_unsafe_cell ty ->
          return Tree_borrow.ReservedIM
      | BTwoPhaseMut | BMut -> return @@ Tree_borrow.Reserved false
      | BUniqueImmutable -> return @@ Tree_borrow.Reserved false
      | BShallow -> Fmt.kstr not_impl "Unhandled borrow kind: BShallow"
    in
    let node = Tree_borrow.init ~state:tag_st () in
    let@ block, tb = with_opt_or block (ptr, meta) in
    let tb' = Tree_borrow.add_child ~root:tb ~parent:ptr.tag node in
    let block = Some (block, tb') in
    let ptr' = { ptr with tag = node.tag } in
    L.debug (fun m ->
        m "Borrowed pointer %a -> %a (%a)" Sptr.pp ptr Sptr.pp ptr'
          Tree_borrow.pp_state tag_st);
    Result.ok ((ptr', meta), block)

let protect (ptr, meta) (ty : Charon.Types.ty) (mut : Charon.Types.ref_kind) st
    =
  if Tree_borrow.is_disabled () || Layout.is_unsafe_cell ty then
    Result.ok ((ptr, meta), st)
  else
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    let@ ofs, block = with_ptr ptr st in
    let@ block, tb = with_opt_or block (ptr, meta) in
    let tag_st =
      match mut with RMut -> Tree_borrow.Reserved false | RShared -> Frozen
    in
    let node = Tree_borrow.init ~state:tag_st ~protector:true () in
    let tb' = Tree_borrow.add_child ~root:tb ~parent:ptr.tag node in
    let ptr' = { ptr with tag = node.tag } in
    let* size = Layout.size_of_s ty in
    L.debug (fun m ->
        m "Protecting pointer %a -> %a, on [%a;%a]" Sptr.pp ptr Sptr.pp ptr'
          Typed.ppa ofs Typed.ppa size);
    let++ (), block' =
      (* nothing to protect *)
      if%sat size ==@ 0s then Result.ok ((), Some block)
      else Tree_block.protect ofs size node.tag tb' (Some block)
    in
    let block = Option.map (fun b' -> (b', tb')) block' in
    ((ptr', meta), block)

let unprotect (ptr, _) (ty : Charon.Types.ty) st =
  let lift_freed_err () f =
    let open Soteria_symex.Compo_res in
    let+ res = f () in
    match res with
    | Ok v -> Ok v
    | Missing fixes -> Missing fixes
    | Error `UseAfterFree -> Error `RefInvalidatedEarly
    | Error
        ((`AliasingError | `NullDereference | `OutOfBounds | `UBDanglingPointer)
         as e) ->
        Error e
  in
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let@ () = lift_freed_err () in
  let@ ofs, block = with_ptr ptr st in
  let* size = Layout.size_of_s ty in
  let@ block, tb = with_opt_or block () in
  let tb' =
    Tree_borrow.update tb (fun n -> { n with protector = false }) ptr.tag
  in
  let++ (), block' =
    if%sat size ==@ 0s then Result.ok ((), Some block)
    else Tree_block.unprotect ofs size ptr.tag tb' (Some block)
  in
  let block' = Option.map (fun b' -> (b', tb')) block' in
  L.debug (fun m -> m "Unprotected pointer %a" Sptr.pp ptr);
  ((), block')

let leak_check st =
  let@ () = with_error_loc_as_call_trace ~msg:"Leaking function" st in
  let@ () = with_loc_err () in
  let** global_addresses, st =
    Result.fold_list (GlobMap.bindings st.globals) ~init:([], st)
      ~f:(fun (acc, st) (g, ((ptr : Sptr.t), _)) ->
        let loc = Typed.Ptr.loc ptr.ptr in
        match g with
        | String _ -> Result.ok (loc :: acc, st)
        | Global g -> (
            let glob = Crate.get_global g in
            let* res = load ~ignore_borrow:true (ptr, None) glob.ty st in
            match res with
            | Ok (v, st) ->
                let ptrs = Layout.ref_tys_in ~include_ptrs:true v glob.ty in
                let ptrs =
                  List.map
                    (fun (((p : Sptr.t), _), _) -> Typed.Ptr.loc p.ptr)
                    ptrs
                in
                Result.ok (loc :: (ptrs @ acc), st)
            | _ -> Result.ok (acc, st)))
  in
  let@ state = with_state st in
  let** leaks =
    SPmap.fold
      (fun leaks (k, v) ->
        (* FIXME: This only works because our addresses are concrete *)
        if Freeable.Freed <> v && not (List.mem k global_addresses) then
          Result.ok (k :: leaks)
        else Result.ok leaks)
      [] state
  in
  if List.is_empty leaks then Result.ok ((), state)
  else (
    L.info (fun m ->
        m "Found leaks: %a" Fmt.(list ~sep:(any ", ") Typed.ppa) leaks);
    Result.error `MemoryLeak)

let add_error e ({ errors; _ } as st) =
  Result.ok ((), { st with errors = (e :> Error.t err) :: errors })

let pop_error ({ errors; _ } as st) =
  match errors with
  | e :: rest -> Result.error (e, { st with errors = rest })
  | _ -> failwith "pop_error with no errors?"

let unwind_with ~f ~fe symex =
  Result.bind_2 symex ~f ~fe:(fun (((err_ty, _) as err), state) ->
      if Error.is_unwindable err_ty then fe (err, state)
      else Result.error (err, state))

let declare_fn fn_ptr ({ functions; _ } as st) =
  let+ loc, st =
    match FunBiMap.get_loc fn_ptr functions with
    | Some loc -> return (loc, st)
    | None ->
        (* FIXME: once we stop having concrete locations, we'll need to add a distinct
       constraint here. *)
        let+ loc = StateKey.fresh () in
        let functions = FunBiMap.add loc fn_ptr functions in
        (loc, { st with functions })
  in
  (* FIXME: what is the size and align of a fn pointer?
     See https://github.com/rust-lang/rust/issues/82232 *)
  let ptr = Typed.Ptr.mk loc 0s in
  let ptr : Sptr.t =
    { ptr; tag = Tree_borrow.zero; align = Typed.cast 1s; size = 1s }
  in
  Soteria_symex.Compo_res.Ok ((ptr, None), st)

let lookup_fn (({ ptr; _ } as fptr : Sptr.t), _) ({ functions; _ } as st) =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  if%sat Typed.Ptr.ofs ptr ==@ 0s then
    let loc = Typed.Ptr.loc ptr in
    match FunBiMap.get_fn loc functions with
    | Some fn -> Result.ok (fn, st)
    | None -> (
        let@ _, block = with_ptr fptr st in
        (* If a block exists, we can be sure that this isn't a function pointer *)
        match block with
        | Some _ -> Result.error `NotAFnPointer
        | None -> Result.miss [])
  else Result.error `MisalignedFnPointer
