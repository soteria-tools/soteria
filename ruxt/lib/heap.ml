open Soteria_rust_lib
open Charon
open Soteria.Symex.Compo_res
open Rust_val
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Rustsymex

type 'a err = 'a * Charon.Meta.span_data Soteria.Terminal.Call_trace.t

let add_to_call_trace (err, trace_elem) trace_elem' =
  (err, trace_elem' :: trace_elem)

let with_error_loc_as_call_trace ?(msg = "Triggering memory operation") st f =
  let open Rustsymex.Syntax in
  let+- err, loc = f () in
  ((err, Soteria.Terminal.Call_trace.singleton ~loc ~msg ()), st)

let error err st =
  let@ () = with_error_loc_as_call_trace st in
  error err

let lift_err st (symex : ('a, 'e, 'f) Result.t) =
  let* res = symex in
  match res with
  | Error e -> error e st
  | Ok ok -> Result.ok ok
  | Missing fix -> Result.miss fix

let assert_ guard err st = lift_err st (assert_or_error guard err)

(* State details *)
module DecayMap = Sptr.DecayMap
module DecayMapMonad = Sptr.DecayMapMonad
module Sptr = Sptr.ArithPtr
module Encoder = Encoder.Make (Sptr)

(* State combinators *)

module StateKey = struct
  include Typed

  type t = T.sloc Typed.t

  let pp = ppa
  let fresh_rsym () = nondet (Typed.t_loc (8 * Lc.size_of_uint_ty Usize))
  let fresh () = DecayMapMonad.lift @@ fresh_rsym ()
  let simplify = DecayMapMonad.simplify
end

module Freeable = Soteria.Sym_states.Freeable.Make (DecayMapMonad)
module SPmap = Soteria.Sym_states.Pmap.Direct_access (DecayMapMonad) (StateKey)
module Bi = Soteria.Sym_states.Bi_abd.Make (DecayMapMonad)
module Tree_block = Rtree_block.Make (Sptr)

module Meta = struct
  type t = {
    align : Typed.T.nonzero Typed.t;
    size : Typed.T.sint Typed.t;
    tb_root : Tree_borrow.tag;
    kind : Alloc_kind.t;
    span : Meta.span_data;
  }
  [@@deriving show { with_path = false }]
end

module With_meta = Soteria.Sym_states.With_info.Make (DecayMapMonad) (Meta)

type global = String of string | Global of Types.global_decl_id

module GlobMap = Map.MakePp (struct
  type t = global = String of string | Global of Types.global_decl_id
  [@@deriving show { with_path = false }, ord]
end)

module FunBiMap = struct
  include
    Bimap.Make
      (struct
        type t = T.sloc Typed.t

        let compare = Typed.compare
        let pp = Typed.ppa
      end)
      (struct
        type t = Types.fun_decl_ref

        let compare = Types.compare_fun_decl_ref
        let pp = Types.pp_fun_decl_ref
      end)

  let get_fn = find_l
  let get_loc = find_r
end

type sub = Tree_block.t * Tree_borrow.t [@@deriving show { with_path = false }]
type block = sub Freeable.t With_meta.t [@@deriving show { with_path = false }]

type t = {
  state : block SPmap.t option;
  functions : FunBiMap.t;
  globals : Sptr.t Rust_val.full_ptr GlobMap.t;
  errors : Error.t err list; [@printer Fmt.list Error.pp_err_and_call_trace]
  pointers : DecayMap.t option;
}
[@@deriving show { with_path = false }]

type serialized = serialized_atom list * serialized_globals
[@@deriving show { with_path = false }]

and serialized_atom =
  T.sloc Typed.t
  * Tree_block.serialized Freeable.serialized With_meta.serialized
[@@deriving show { with_path = false }]

and serialized_globals = T.sloc Typed.t list
[@@deriving show { with_path = false }]

let serialize_globals globals : serialized_globals =
  ListLabels.fold_left (GlobMap.bindings globals) ~init:[]
    ~f:(fun acc (_, ((ptr : Sptr.t), _)) -> Typed.Ptr.loc ptr.ptr :: acc)

let lift_fix_globals globals res =
  let open DecayMapMonad.Syntax in
  let+? heap = res in
  (heap, serialize_globals globals)

let serialize st : serialized =
  let heap =
    match st.state with
    | None -> []
    | Some st ->
        let serialize_freeable (b, _) = Tree_block.serialize b in
        SPmap.serialize
          (With_meta.serialize (Freeable.serialize serialize_freeable))
          st
  in
  let globals = serialize_globals st.globals in
  (heap, globals)

let subst_serialized (subst_var : Svalue.Var.t -> Svalue.Var.t)
    ((heap, globals) : serialized) : serialized =
  let heap =
    let subst_serialized_block subst_var f =
      With_meta.subst_serialized
        (Freeable.subst_serialized Tree_block.subst_serialized)
        subst_var f
    in
    SPmap.subst_serialized subst_serialized_block subst_var heap
  in
  let globals = List.map (Typed.subst subst_var) globals in
  (heap, globals)

let iter_vars_serialized ((heap, globals) : serialized) :
    (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit =
  let iter_heap =
    let iter_vars_serialized_block s =
      With_meta.iter_vars_serialized
        (Freeable.iter_vars_serialized Tree_block.iter_vars_serialized)
        s
    in
    SPmap.iter_vars_serialized iter_vars_serialized_block heap
  in
  let iter_globals =
    let append acc x = Iter.append acc (Typed.iter_vars x) in
    List.fold_left append Iter.empty globals
  in
  Iter.append iter_heap iter_globals

let pp_pretty ~ignore_freed ft { state; _ } =
  let ignore =
    if ignore_freed then function
      | _, With_meta.{ node = Freeable.Freed; _ } -> true | _ -> false
    else fun _ -> false
  in
  match state with
  | None -> Fmt.pf ft "Empty State"
  | Some st ->
      SPmap.pp ~ignore
        (fun ft block ->
          match block with
          | { info = Some { kind = Function fn; _ }; _ } ->
              Fmt.pf ft "function %a" Crate.pp_fun_decl_ref fn
          | { node; _ } ->
              (Freeable.pp (fun fmt (tb, _) -> Tree_block.pp_pretty fmt tb))
                ft node)
        ft st

let empty =
  {
    state = None;
    functions = FunBiMap.empty;
    globals = GlobMap.empty;
    errors = [];
    pointers = None;
  }

let log action ptr st =
  L.trace (fun m ->
      m "About to execute action: %s at %a (%a)@\n@[<2>STATE:@ %a@]" action
        Sptr.pp ptr Charon_util.pp_span_data (get_loc ())
        (pp_pretty ~ignore_freed:true)
        st)

let with_state st f =
  let+ res, pointers =
    DecayMapMonad.with_state ~state:st.pointers @@ f st.state
  in
  match res with
  | Ok (v, state) -> Ok (v, { st with state; pointers })
  | Missing fixes -> Missing fixes
  | Error e -> Error e

let with_tbs b f =
  let open DecayMapMonad.Syntax in
  let block, tree_borrow =
    match b with
    | None -> (None, Tree_borrow.ub_state)
    | Some (block, tb) -> (Some block, tb)
  in
  let+ res = f (block, tree_borrow) in
  match res with
  | Ok (v, b) -> Ok (v, Option.map (fun b -> (b, tree_borrow)) b)
  | Missing fixes -> Missing fixes
  | Error e -> Error e

let with_decay_map f st =
  let+ res, pointers = DecayMapMonad.with_state ~state:st.pointers f in
  (res, { st with pointers })

let with_ptr (ptr : Sptr.t) (st : t)
    (f :
      [< T.sint ] Typed.t * sub option ->
      ('a * sub option, 'err, 'fix list) DecayMapMonad.Result.t) :
    ('a * t, 'err, serialized) Result.t =
  let** () =
    assert_or_error
      Typed.(not (Sptr.sem_eq ptr (Sptr.null_ptr ())))
      `NullDereference
  in
  let loc, ofs = Typed.Ptr.decompose ptr.ptr in
  let@ state = with_state st in
  let open DecayMapMonad in
  let open DecayMapMonad.Syntax in
  let* res =
    (SPmap.wrap (function
      | Some ({ info = Some { kind = Function _; _ }; _ } : block) ->
          Result.error `AccessedFnPointer
      | block -> With_meta.wrap (Freeable.wrap (fun st -> f (ofs, st))) block))
      loc state
    |> lift_fix_globals st.globals
  in
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
    (f : 'a -> ('b * 'a option, 'err, 'f) DecayMapMonad.Result.t) :
    ('b * 'a option, 'err, 'f) DecayMapMonad.Result.t =
  match x with
  | Some v -> f v
  | None -> DecayMapMonad.Result.ok (otherwise, None)

let uninit (ptr, _) ty st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "uninit" ptr st;
  let** size = Layout.size_of ty in
  let@ ofs, block = with_ptr ptr st in
  let@ block, _ = with_tbs block in
  Tree_block.uninit_range ofs size block

let apply_parser (type a) ?(ignore_borrow = false) ptr
    (parser : offset:T.sint Typed.t -> (a, 's, 'e, 'f) Encoder.ParserMonad.t) st
    : (a * t, 'e, serialized) Result.t =
  log "load" ptr st;
  let@ offset, block = with_ptr ptr st in
  let@ block, tb = with_tbs block in
  let handler (ty, ofs) = Tree_block.load ~ignore_borrow ofs ty ptr.tag tb in
  let get_all (size, ofs) = Tree_block.get_init_leaves ofs size in
  Encoder.ParserMonad.parse ~init:block ~handler ~get_all @@ parser ~offset

let rec check_ptr_align ((ptr, meta) : 'a full_ptr) (ty : Types.ty) st =
  (* The expected alignment of a dyn pointer is stored inside the VTable  *)
  let** exp_align, st =
    match (ty, meta) with
    | TDynTrait _, VTable vt ->
        let usize = Types.TLiteral (TUInt Usize) in
        let** ptr = Sptr.offset ~ty:usize ~signed:false vt Usize.(2s) in
        let++ align, st = load (ptr, Thin) usize st in
        (Typed.cast (as_base_i Usize align), st)
    | _ ->
        let++ align = Layout.align_of ty in
        (align, st)
  in
  L.debug (fun m ->
      m "Checking pointer alignment of %a: expect %a for %a" Sptr.pp ptr
        Typed.ppa exp_align Charon_util.pp_ty ty);
  (* 0-based pointers are aligned up to their offset *)
  let loc, ofs = Typed.Ptr.decompose ptr.ptr in
  let align = Typed.ite (Typed.Ptr.is_null_loc loc) exp_align ptr.align in
  let++ () =
    assert_or_error
      (Sptr.is_aligned exp_align ptr)
      (`MisalignedPointer (exp_align, align, ofs))
  in
  ((), st)

and load ?ignore_borrow ?(ref_checks = true) ((ptr, meta) as fptr) ty st :
    (Sptr.t rust_val * t, Error.t, serialized) Result.t =
  let** (), st = check_ptr_align fptr ty st in
  let is_valid_ptr =
    if ref_checks then fun ptr ty -> fake_read ptr ty st
    else fun _ _ -> return None
  in
  let parser ~offset = Encoder.rust_of_cvals ~meta ~offset ~is_valid_ptr ty in
  let++ value, st = apply_parser ?ignore_borrow ptr parser st in
  L.debug (fun f ->
      f "Finished reading rust value %a" (Rust_val.pp Sptr.pp) value);
  (value, st)

and load_discriminant ((ptr, _) as fptr) ty st =
  let** (), st = check_ptr_align fptr ty st in
  let parser ~offset = Encoder.variant_of_enum ty ~offset in
  apply_parser ptr parser st

(** Performs a side-effect free ghost read -- this does not modify the state or
    the tree-borrow state. Returns [Some error] if an error occurred, and [None]
    otherwise *)
(* We can't return a [Rustsymex.Result.t] here, because it's used in [load] which
   expects a [Tree_block.serialized_atom list] for the [Missing] case, while the
   external signature expects a [serialized].
   This could be fixed by lifting all misses individually inside [handler] and
   [get_all] in [apply_parser], but that's kind of a mess to change and not really
   worth it I believe; I don't think these misses matter at all (TBD). *)
and fake_read ptr ty st =
  (* FIXME: i am not certain how one checks for the validity of a DST *)
  if Layout.is_dst ty || Option.is_some (Layout.as_zst ty) then return None
  else (
    L.debug (fun m ->
        m "Checking validity of %a for %a" (pp_full_ptr Sptr.pp) ptr
          Charon_util.pp_ty ty);
    let+ res = load ~ignore_borrow:true ~ref_checks:false ptr ty st in
    match res with
    | Ok _ -> None
    | Error e -> Some e
    | Missing _ -> failwith "Miss in fake_read")

let check_ptr_align ptr ty st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  check_ptr_align ptr ty st

let load ?ignore_borrow ptr ty st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  load ?ignore_borrow ptr ty st

let load_discriminant ptr ty st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  load_discriminant ptr ty st

(** Performs a load at the tree borrow level, by updating the borrow state,
    without attempting to validate the values or checking uninitialised memory
    accesses; all of these are ignored. *)
let tb_load ((ptr : Sptr.t), _) ty st =
  match ptr.tag with
  | None -> Result.ok ((), st)
  | Some tag ->
      let@ () = with_error_loc_as_call_trace st in
      let@ () = with_loc_err () in
      let** size = Layout.size_of ty in
      if%sat size ==@ Usize.(0s) then Result.ok ((), st)
      else (
        log "tb_load" ptr st;
        let@ ofs, block = with_ptr ptr st in
        let@ block, tb = with_tbs block in
        Tree_block.tb_access ofs size tag tb block)

let store ((ptr, _) as fptr) ty sval st =
  let** parts = lift_err st @@ Encoder.rust_to_cvals sval ty in
  if List.is_empty parts then Result.ok ((), st)
  else
    let** (), st = check_ptr_align fptr ty st in
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    L.debug (fun f ->
        f "Parsed to parts [%a]"
          Fmt.(list ~sep:comma Encoder.pp_cval_info)
          parts);
    log "store" ptr st;
    let** size = Layout.size_of ty in
    let@ ofs, block = with_ptr ptr st in
    let@ block, tb = with_tbs block in
    let open DecayMapMonad in
    let open DecayMapMonad.Syntax in
    (* We uninitialise the whole range before writing, to ensure padding bytes are copied if
           there are any. *)
    let** (), block = Tree_block.uninit_range ofs size block in
    Result.fold_list parts ~init:((), block)
      ~f:(fun ((), block) (value, offset) ->
        Tree_block.store (offset +!!@ ofs) value ptr.tag tb block)

let copy_nonoverlapping ~dst:(dst, _) ~src:(src, _) ~size st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let** tree_to_write, st =
    let open DecayMapMonad.Syntax in
    let@ ofs, block = with_ptr src st in
    let@ block, _ = with_tbs block in
    let++ tree, _ = Tree_block.get_raw_tree_owned ofs size block in
    (tree, block)
  in
  let open DecayMapMonad in
  let open DecayMapMonad.Syntax in
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
          Tree_block.Range.offset leaf.range ~-!(fst original_tree.range)
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

let mk_block ?(kind = Alloc_kind.Heap) ?span ?zeroed ~size ~align () :
    block * Tree_borrow.tag =
  let tb, tag = Tree_borrow.init ~state:Unique () in
  let block = Tree_block.alloc ?zeroed size in
  let span = Option.value span ~default:(get_loc ()) in
  let info : Meta.t = { align; size; kind; span; tb_root = tag } in
  ({ node = Alive (block, tb); info = Some info }, tag)

let alloc ?kind ?span ?zeroed size align st =
  (* Commenting this out as alloc cannot fail *)
  (* let@ () = with_loc_err () in*)
  let@ () = with_error_loc_as_call_trace st in
  let globals = st.globals in
  let@ state = with_state st in
  let open DecayMapMonad in
  let open DecayMapMonad.Syntax in
  let block, tag = mk_block ?kind ?span ?zeroed ~align ~size () in
  let** loc, state =
    SPmap.alloc ~new_codom:block state |> lift_fix_globals globals
  in
  let ptr = Typed.Ptr.mk loc Usize.(0s) in
  let ptr : Sptr.t = { ptr; tag = Some tag; align; size } in
  (* The pointer is necessarily not null *)
  let+ () = assume [ Typed.(not (Ptr.is_null_loc loc)) ] in
  ok ((ptr, Thin), state)

let alloc_untyped ?kind ?span ~zeroed ~size ~align st =
  alloc ?kind ?span ~zeroed size align st

let alloc_ty ?kind ?span ty st =
  let** layout = lift_err st @@ Layout.layout_of ty in
  alloc ?kind ?span layout.size layout.align st

let alloc_tys ?kind ?span tys st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let** layouts =
    Result.fold_list tys ~init:[] ~f:(fun acc ty ->
        let++ layout = Layout.layout_of ty in
        layout :: acc)
  in
  let layouts = List.rev layouts in
  let globals = st.globals in
  let@ st = with_state st in
  SPmap.allocs st ~els:layouts ~fn:(fun layout loc ->
      let open DecayMapMonad in
      let open DecayMapMonad.Syntax in
      (* make Tree_block *)
      let { size; align; _ } : Layout.t = layout in
      let block, tag = mk_block ?kind ?span ~align ~size () in
      (* create pointer *)
      let+ () = assume [ Typed.(not (Ptr.is_null_loc loc)) ] in
      let ptr = Typed.Ptr.mk loc Usize.(0s) in
      let ptr : Sptr.t = { ptr; tag = Some tag; align; size } in
      (block, (ptr, Thin)))
  |> lift_fix_globals globals

let free (({ ptr; _ } : Sptr.t), _) st =
  let** () = assert_ (Typed.Ptr.ofs ptr ==@ Usize.(0s)) `InvalidFree st in
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let globals = st.globals in
  let@ st = with_state st in
  L.trace (fun m -> m "Freeing pointer %a" Typed.ppa ptr);
  (* TODO: does the tag not play a role in freeing? *)
  SPmap.wrap
    (With_meta.wrap
       (Freeable.free ~assert_exclusively_owned:(fun t ->
            Tree_block.assert_exclusively_owned @@ Option.map fst t)))
    (Typed.Ptr.loc ptr) st
  |> lift_fix_globals globals

let zeros (ptr, _) size st =
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  log "zeroes" ptr st;
  let@ ofs, block = with_ptr ptr st in
  let@ block, _ = with_tbs block in
  Tree_block.zero_range ofs size block

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

let borrow ((ptr : Sptr.t), meta) (ty : Types.ty)
    (kind : Expressions.borrow_kind) st =
  (* &UnsafeCell<T> are treated as raw pointers, and reuse parent's tag! *)
  if
    Option.is_none ptr.tag
    || Tree_borrow.is_disabled ()
    || (kind = BShared && Layout.is_unsafe_cell ty)
  then Result.ok ((ptr, meta), st)
  else
    let* tag_st =
      match kind with
      | BShared -> return Tree_borrow.Frozen
      | (BTwoPhaseMut | BMut) when Layout.is_unsafe_cell ty ->
          return Tree_borrow.ReservedIM
      | BTwoPhaseMut | BMut -> return @@ Tree_borrow.Reserved false
      | BUniqueImmutable -> return @@ Tree_borrow.Reserved false
      | BShallow -> Fmt.kstr not_impl "Unhandled borrow kind: BShallow"
    in
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    let@ _, block = with_ptr ptr st in
    let@ block, tb = with_opt_or block (ptr, meta) in
    let parent = Option.get ptr.tag in
    let tb', tag = Tree_borrow.add_child ~parent ~state:tag_st tb in
    let block = Some (block, tb') in
    let ptr' = { ptr with tag = Some tag } in
    L.debug (fun m ->
        m "Borrowed pointer %a -> %a (%a)" Sptr.pp ptr Sptr.pp ptr'
          Tree_borrow.pp_state tag_st);
    DecayMapMonad.Result.ok ((ptr', meta), block)

let protect ((ptr : Sptr.t), meta) (ty : Types.ty) (mut : Types.ref_kind) st =
  if
    Option.is_none ptr.tag
    || Tree_borrow.is_disabled ()
    || Layout.is_unsafe_cell ty
  then Result.ok ((ptr, meta), st)
  else
    let@ () = with_error_loc_as_call_trace st in
    let@ () = with_loc_err () in
    let** size = Layout.size_of ty in
    let@ ofs, block = with_ptr ptr st in
    let@ block, tb = with_opt_or block (ptr, meta) in
    let open DecayMapMonad.Syntax in
    let state =
      match mut with RMut -> Tree_borrow.Reserved false | RShared -> Frozen
    in
    let parent = Option.get ptr.tag in
    let tb', tag = Tree_borrow.add_child ~parent ~state ~protector:true tb in
    let ptr' = { ptr with tag = Some tag } in
    L.debug (fun m ->
        m "Protecting pointer %a -> %a, on [%a;%a]" Sptr.pp ptr Sptr.pp ptr'
          Typed.ppa ofs Typed.ppa size);
    let++ (), block' =
      (* nothing to protect *)
      if%sat size ==@ Usize.(0s) then DecayMapMonad.Result.ok ((), Some block)
      else Tree_block.protect ofs size tag tb' (Some block)
    in
    let block = Option.map (fun b' -> (b', tb')) block' in
    ((ptr', meta), block)

let unprotect ((ptr : Sptr.t), _) (ty : Types.ty) st =
  match ptr.tag with
  | None -> Result.ok ((), st)
  | Some tag ->
      let lift_freed_err () f =
        let+ res = f () in
        match res with
        | Error `UseAfterFree -> Error `RefInvalidatedEarly
        | v -> v
      in
      let@ () = with_error_loc_as_call_trace st in
      let@ () = with_loc_err () in
      let** size = Layout.size_of ty in
      let@ () = lift_freed_err () in
      let@ ofs, block = with_ptr ptr st in
      let@ block, tb = with_opt_or block () in
      let open DecayMapMonad.Syntax in
      let tb' = Tree_borrow.unprotect tag tb in
      let++ (), block' =
        if%sat size ==@ Usize.(0s) then DecayMapMonad.Result.ok ((), Some block)
        else Tree_block.unprotect ofs size tag tb' (Some block)
      in
      let block' = Option.map (fun b' -> (b', tb')) block' in
      L.debug (fun m -> m "Unprotected pointer %a" Sptr.pp ptr);
      ((), block')

let with_exposed addr state =
  let@ () = with_error_loc_as_call_trace state in
  let@ () = with_loc_err () in
  match !Config.current.provenance with
  | Strict -> Result.error `UBIntToPointerStrict
  | Permissive -> (
      let globals = state.globals in
      let@ st = with_state state in
      let open DecayMapMonad in
      let open DecayMapMonad.Syntax in
      let* res = DecayMap.from_exposed addr in
      match res with
      | None -> Result.ok ((Sptr.null_ptr_of addr, Thin), st)
      | Some (loc, ofs) -> (
          let ofs = addr -!@ ofs in
          let ptr = Typed.Ptr.mk loc ofs in
          let** block, st =
            SPmap.wrap (fun b -> Result.ok (b, b)) loc st
            |> lift_fix_globals globals
          in
          match block with
          | None | Some { info = None; _ } -> Result.miss []
          | Some { info = Some { size; align; _ }; _ } ->
              let ptr : Sptr.t = { ptr; tag = None; align; size } in
              Result.ok ((ptr, Thin), st)))

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
            let* res = load ~ignore_borrow:true (ptr, Thin) glob.ty st in
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
  let globals = st.globals in
  let@ st = with_state st in
  let open DecayMapMonad in
  let open DecayMapMonad.Syntax in
  let** leaks =
    SPmap.fold
      (fun leaks (k, (v : block)) ->
        (* FIXME: This only works because our addresses are concrete *)
        match v with
        | { node = Alive _; info = Some { kind = Heap; span; _ }; _ }
          when not (List.mem k global_addresses) ->
            Result.ok ((k, span) :: leaks)
        | _ -> Result.ok leaks)
      [] st
    |> lift_fix_globals globals
  in
  if List.is_empty leaks then Result.ok ((), st)
  else (
    L.info (fun m ->
        let pp_leak ft (k, span) =
          Fmt.pf ft "%a (allocated at %a)" Typed.ppa k Charon_util.pp_span_data
            span
        in
        m "Found leaks: %a" Fmt.(list ~sep:(any ", ") pp_leak) leaks);
    let spans = List.map snd leaks in
    Result.error (`MemoryLeak spans))

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

let declare_fn fn_def ({ functions; _ } as st) =
  match FunBiMap.get_loc fn_def functions with
  | Some loc ->
      let ptr : Sptr.t =
        {
          ptr = Typed.Ptr.mk loc Usize.(0s);
          tag = None;
          align = Usize.(1s);
          size = Usize.(0s);
        }
      in
      Result.ok ((ptr, Thin), st)
  | None ->
      (* FIXME: once we stop having concrete locations, we'll need to add a distinct
       constraint here. *)
      (* FIXME: should we use [SPmap.alloc] here instead? What would go in the map? *)
      let fn = Crate.get_fun fn_def.id in
      let++ (ptr, meta), st =
        alloc_untyped ~kind:(Function fn_def) ~span:fn.item_meta.span.data
          ~zeroed:false
          ~size:Usize.(0s)
          ~align:Usize.(1s)
          st
      in
      let ptr = { ptr with tag = None } in
      let loc = Typed.Ptr.loc ptr.ptr in
      let functions = FunBiMap.add loc fn_def functions in
      ((ptr, meta), { st with functions })

let lookup_fn (({ ptr; _ } : Sptr.t), _) ({ functions; _ } as st) =
  let** () =
    assert_ (Typed.Ptr.ofs ptr ==@ Usize.(0s)) `MisalignedFnPointer st
  in
  let@ () = with_error_loc_as_call_trace st in
  let@ () = with_loc_err () in
  let loc = Typed.Ptr.loc ptr in
  match FunBiMap.get_fn loc functions with
  | Some fn -> Result.ok (fn, st)
  | None -> Result.error `NotAFnPointer

let tb_perform f map serialized o =
  (f serialized @@ Option.map fst o |> map) @@ fun res ->
  let default, _ = Tree_borrow.init ~state:Unique () in
  let tb = o |> Option.map snd |> Option.value ~default in
  res |> Option.map (fun block -> (block, tb))

let produce ((heap, _) : serialized) (st : t) : t Rustsymex.t =
  let tb_produce = tb_perform Tree_block.produce DecayMapMonad.map in
  let+ res =
    let@ state = with_state st in
    let open DecayMapMonad.Syntax in
    let* state =
      SPmap.produce (With_meta.produce (Freeable.produce tb_produce)) heap state
    in
    DecayMapMonad.Result.ok ((), state)
  in
  match res with
  | Ok ((), state) -> state
  | _ -> failwith "produce: unexpected error"

let consume ((loc, ser) : serialized_atom) (st : t) :
    (t, [> Rustsymex.lfail ], serialized) Rustsymex.Result.t =
  let tb_consume = tb_perform Tree_block.consume DecayMapMonad.Result.map in
  let++ (), state =
    let@ state = with_state st in
    let open DecayMapMonad.Syntax in
    let** state =
      SPmap.consume
        (With_meta.consume (Freeable.consume tb_consume))
        [ (loc, ser) ]
        state
      |> lift_fix_globals st.globals
    in
    DecayMapMonad.Result.ok ((), state)
  in
  state
