open Soteria.Symex.Compo_res
open Rust_val
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Rustsymex
open Charon

(* State details *)
module DecayMap = Sptr.DecayMap
module DecayMapMonad = Sptr.DecayMapMonad
module Sptr = Sptr.ArithPtr
module Encoder = Value_codec.Encoder (Sptr)

(* State combinators *)

module StateKey = struct
  include Typed

  type t = T.sloc Typed.t

  let pp = ppa
  let to_int = unique_tag
  let i = ref 0

  let fresh_rsym () =
    incr i;
    return (Ptr.loc_of_int !i)

  let distinct _ = v_true
  let simplify = DecayMapMonad.simplify
  let fresh () = DecayMapMonad.lift @@ fresh_rsym ()
  (* The above only works in WPST -- otherwise, use:
   * let fresh () = nondet (Typed.t_sloc ()) *)
end

module Freeable = Soteria.Sym_states.Freeable.Make (DecayMapMonad)
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

type global = String of string | Global of Types.global_decl_id

module GlobMap = Map.MakePp (struct
  type t = global = String of string | Global of Types.global_decl_id
  [@@deriving show { with_path = false }, ord]
end)

module FunBiMap = struct
  include
    Bimap.MakePp
      (struct
        type t = T.sloc Typed.t

        let compare = Typed.compare
        let pp = Typed.ppa
      end)
      (Fun_kind)

  let get_fn = find_l
  let get_loc = find_r
end

module Block = struct
  type t = Tree_block.t option * Tree_borrow.t
  [@@deriving show { with_path = false }]

  let of_opt = function
    | None -> (None, Tree_borrow.ub_state)
    | Some (b1, b2) -> (b1, b2)

  let to_opt : t -> t option =
    (* FIXME: This is off for compositionality, there is no zero in TBs yet. *)
    function
    | None, _b -> None
    | b1, b2 -> Some (b1, b2)

  type serialized = Tree_block.serialized
  [@@deriving show { with_path = false }]
  (* TODO: serialize Tree_borrow as well *)

  module SM =
    Soteria.Sym_states.State_monad.Make
      (DecayMapMonad)
      (struct
        type nonrec t = t option
      end)

  open SM.Syntax

  let with_tree_block (f : ('a, 'err, 'fix) Tree_block.SM.Result.t) :
      ('a, 'err, 'fix) SM.Result.t =
    let* t_opt = SM.get_state () in
    let tree, tb = of_opt t_opt in
    let*^ v, tree = f tree in
    let+ () = SM.set_state (to_opt (tree, tb)) in
    v

  let with_tree_block_read_tb
      (f : Tree_borrow.t -> ('a, 'err, 'fix) Tree_block.SM.Result.t) :
      ('a, 'err, 'fix) SM.Result.t =
    let* t_opt = SM.get_state () in
    let tree, tb = of_opt t_opt in
    let*^ v, tree = f tb tree in
    let+ () = SM.set_state (to_opt (tree, tb)) in
    v

  let borrow ((ptr : Sptr.t), meta) ty (kind : Expressions.borrow_kind) =
    let open SM in
    let open SM.Syntax in
    let* tag_st =
      match kind with
      | BShared -> return Tree_borrow.Frozen
      | (BTwoPhaseMut | BMut) when Layout.is_unsafe_cell ty ->
          return Tree_borrow.ReservedIM
      | BTwoPhaseMut | BMut -> return @@ Tree_borrow.Reserved false
      | BUniqueImmutable -> return @@ Tree_borrow.Reserved false
      | BShallow ->
          SM.lift @@ DecayMapMonad.not_impl "Unhandled borrow kind: BShallow"
    in
    let* t_opt = SM.get_state () in
    let block, tb = of_opt t_opt in
    let parent = Option.get ptr.tag in
    let tb', tag = Tree_borrow.add_child ~parent ~state:tag_st tb in
    let ptr' = { ptr with tag = Some tag } in
    L.debug (fun m ->
        m "Borrowed pointer %a -> %a (%a)" Sptr.pp ptr Sptr.pp ptr'
          Tree_borrow.pp_state tag_st);
    let+ () = SM.set_state (to_opt (block, tb')) in
    Ok (ptr', meta)

  let protect ((ptr : Sptr.t), meta) (mut : Types.ref_kind) ofs size =
    let open SM in
    let open SM.Syntax in
    let state =
      match mut with RMut -> Tree_borrow.Reserved false | RShared -> Frozen
    in
    let* t_opt = SM.get_state () in
    let block, tb = of_opt t_opt in
    let parent = Option.get ptr.tag in
    let tb', tag = Tree_borrow.add_child ~parent ~state ~protector:true tb in
    let ptr' = { ptr with tag = Some tag } in
    L.debug (fun m ->
        m "Protecting pointer %a -> %a, on [%a;%a]" Sptr.pp ptr Sptr.pp ptr'
          Typed.ppa ofs Typed.ppa size);
    if%sat size ==@ Usize.(0s) then
      let+ () = SM.set_state (to_opt (block, tb')) in
      Ok (ptr', meta)
    else
      let*^ res, block' = Tree_block.protect ofs size tag tb' block in
      match res with
      | Ok () ->
          let+ () = SM.set_state (to_opt (block', tb')) in
          Ok (ptr', meta)
      | Error e -> Result.error e
      | Missing f -> Result.miss f

  let unprotect ofs tag size =
    let open SM in
    let open SM.Syntax in
    let* t_opt = SM.get_state () in
    let block, tb = of_opt t_opt in
    let tb' = Tree_borrow.unprotect tag tb in
    let** (), block' =
      if%sat size ==@ Usize.(0s) then SM.Result.ok ((), block)
      else
        let*^ res, block' = Tree_block.unprotect ofs size tag tb' block in
        match res with
        | Ok () -> Result.ok ((), block')
        | Error e -> Result.error e
        | Missing f -> Result.miss f
    in
    SM.Result.set_state (to_opt (block', tb'))

  let assert_exclusively_owned t_opt =
    let a, _ = of_opt t_opt in
    Tree_block.assert_exclusively_owned a

  let serialize (x, _) = Option.fold ~none:[] ~some:Tree_block.serialize x
  let subst_serialized f a = Tree_block.subst_serialized f a
  let iter_vars_serialized f a = Tree_block.iter_vars_serialized f a
  let produce _ _ = failwith "Not implemented"
end

module Freeable_block = Soteria.Sym_states.Freeable.Make (DecayMapMonad) (Block)

module Freeable_block_with_meta = struct
  include
    Soteria.Sym_states.With_info.Make (DecayMapMonad) (Meta) (Freeable_block)

  let make ?(kind = Alloc_kind.Heap) ?span ?zeroed ~size ~align () :
      (t * Tree_borrow.tag option) DecayMapMonad.t =
    let open DecayMapMonad.Syntax in
    let tb, tag = Tree_borrow.init ~state:Unique () in
    let block = Tree_block.alloc ?zeroed size in
    let+^ span =
      match span with Some span -> return span | None -> get_loc ()
    in
    let info : Meta.t = { align; size; kind; span; tb_root = tag } in
    let tag = if (Config.get ()).ignore_aliasing then None else Some tag in
    (({ node = Alive (Some block, tb); info = Some info } : t), tag)
end

module Heap = struct
  include
    Soteria.Sym_states.Pmap.Direct_access_patricia_tree
      (DecayMapMonad)
      (StateKey)
      (Freeable_block_with_meta)

  let with_ptr (ptr : Sptr.t)
      (f : [< T.sint ] Typed.t -> ('a, 'err, 'fix list) Block.SM.Result.t) :
      ('a, 'err, serialized list) SM.Result.t =
    let open SM in
    let open SM.Syntax in
    let** () =
      assert_or_error
        Typed.(not (Sptr.sem_eq ptr (Sptr.null_ptr ())))
        `NullDereference
    in
    let loc, ofs = Typed.Ptr.decompose ptr.ptr in
    let* res =
      wrap loc
        (let open Freeable_block_with_meta in
         let open SM.Syntax in
         let* block = SM.get_state () in
         match block with
         | Some { info = Some { kind = Function _; _ }; _ } ->
             SM.Result.error `AccessedFnPointer
         | _ -> Freeable_block_with_meta.wrap @@ Freeable_block.wrap (f ofs))
    in
    match res with
    | Missing _ as miss ->
        (* FIXME: this is wrong in compositional? *)
        if%sat Sptr.is_at_null_loc ptr then Result.error `UBDanglingPointer
        else return miss
    | ok_or_err -> return ok_or_err

  module Decoder =
    Value_codec.Decoder
      (Sptr)
      (struct
        module SM = SM

        type fix = serialized list
      end)
end

type serialized = Heap.serialized [@@deriving show { with_path = false }]

type t = {
  heap : Heap.t option;
  functions : FunBiMap.t;
  globals : Sptr.t Rust_val.full_ptr GlobMap.t;
  errors : Error.with_trace list;
      [@printer Fmt.list Error.pp_err_and_call_trace]
  pointers : DecayMap.t option;
  thread_destructor :
    unit ->
    t option ->
    ((unit, Error.with_trace, serialized list) Soteria.Symex.Compo_res.t
    * t option)
    Rustsymex.t;
      [@printer Fmt.any "code"]
  const_generics : Sptr.t rust_val Types.ConstGenericVarId.Map.t;
}
[@@deriving show { with_path = false }]

module SM =
  Soteria.Sym_states.State_monad.Make
    (Rustsymex)
    (struct
      type nonrec t = t option
    end)

let pp_pretty ~ignore_freed ft { heap; _ } =
  let (ignore : 'a * Freeable_block_with_meta.t -> bool) =
    if ignore_freed then function _, { node = Freed; _ } -> true | _ -> false
    else fun _ -> false
  in
  match heap with
  | None -> Fmt.pf ft "Empty State"
  | Some st ->
      Heap.pp' ~ignore
        ~codom:(fun ft block ->
          match (block : Freeable_block_with_meta.t) with
          | { info = Some { kind = Function fn; _ }; _ } ->
              Fmt.pf ft "function %a" Fun_kind.pp fn
          | { node; _ } ->
              Freeable_block.pp'
                ~inner:(fun fmt (tb, _) ->
                  Fmt.option ~none:(Fmt.any "Empty Block") Tree_block.pp_pretty
                    fmt tb)
                ft node)
        ft st

let empty_state =
  {
    heap = None;
    functions = FunBiMap.empty;
    globals = GlobMap.empty;
    errors = [];
    pointers = None;
    thread_destructor = (fun () -> SM.Result.ok ());
    const_generics = Types.ConstGenericVarId.Map.empty;
  }

let empty : t option = None
let of_opt = function None -> empty_state | Some st -> st

open SM
open SM.Syntax

let log action ptr =
  let+ st = SM.get_state () in
  L.trace (fun m ->
      m "About to execute action: %s (%a)@\n@[<2>STATE:@ %a@]" action Sptr.pp
        ptr
        (Fmt.Dump.option (pp_pretty ~ignore_freed:true))
        st)

let[@inline] with_loc_err ?trace () (f : unit -> ('a, Error.t, 'f) SM.Result.t)
    : ('a, Error.with_trace, 'f) SM.Result.t =
  let*- err = f () in
  let+^ loc = get_loc () in
  Error (decorate_error ?trace err loc)

let with_heap (f : ('a, 'b, 'c) Heap.SM.Result.t) : ('a, 'b, 'c) Result.t =
  let* st = SM.get_state () in
  let st = of_opt st in
  let*^ (res, heap), pointers =
    DecayMapMonad.run_with_state ~state:st.pointers @@ f st.heap
  in
  let+ () = SM.set_state (Some { st with heap; pointers }) in
  res

let apply_parser (type a) ?(ignore_borrow = false) ptr
    (parser : offset:T.sint Typed.t -> a Heap.Decoder.ParserMonad.t) :
    (a, Error.t, serialized list) Result.t =
  let* () = log "load" ptr in
  let handler (ty, ofs) =
    let@ _ofs = Heap.with_ptr ptr in
    Block.with_tree_block_read_tb
      (Tree_block.load ~ignore_borrow ofs ty ptr.tag)
  in
  let get_all (size, ofs) =
    let@ _ofs = Heap.with_ptr ptr in
    Block.with_tree_block (Tree_block.get_init_leaves ofs size)
  in
  let offset = Typed.Ptr.ofs ptr.ptr in
  with_heap
  @@ Heap.Decoder.ParserMonad.parse ~handler ~get_all
  @@ parser ~offset

let with_decay_map (f : 'a DecayMapMonad.t) : 'a SM.t =
  let* st = SM.get_state () in
  let st = of_opt st in
  let*^ v, pointers = f st.pointers in
  let+ () = SM.set_state (Some { st with pointers }) in
  v

let with_ptr ptr f = with_heap @@ Heap.with_ptr ptr f

let uninit ((ptr, _) : Sptr.t * 'a) (ty : Types.ty) :
    (unit, 'err, 'fix) Result.t =
  let@ () = with_loc_err ~trace:"Uninitialising memory" () in
  let* () = log "uninit" ptr in
  let**^ size = Layout.size_of ty in
  let@ ofs = with_ptr ptr in
  Block.with_tree_block @@ Tree_block.uninit_range ofs size

let rec check_ptr_align ((ptr, meta) : 'a full_ptr) (ty : Types.ty) =
  (* The expected alignment of a dyn pointer is stored inside the VTable *)
  let** exp_align =
    match (ty, meta) with
    | TDynTrait _, VTable vt ->
        let usize = Types.TLiteral (TUInt Usize) in
        let**^ ptr = Sptr.offset ~ty:usize ~signed:false vt Usize.(2s) in
        let++ align = load (ptr, Thin) usize in
        Typed.cast (as_base_i Usize align)
    | _ -> SM.lift @@ Layout.align_of ty
  in
  L.debug (fun m ->
      m "Checking pointer alignment of %a: expect %a for %a" Sptr.pp ptr
        Typed.ppa exp_align Charon_util.pp_ty ty);
  (* 0-based pointers are aligned up to their offset *)
  let loc, ofs = Typed.Ptr.decompose ptr.ptr in
  let align = Typed.ite (Typed.Ptr.is_null_loc loc) exp_align ptr.align in
  assert_or_error
    (Sptr.is_aligned exp_align ptr)
    (`MisalignedPointer (exp_align, align, ofs))

and load ?ignore_borrow ?(ref_checks = true) ((ptr, meta) as fptr) ty :
    (Sptr.t rust_val, Error.t, serialized list) Result.t =
  let** () = check_ptr_align fptr ty in
  let parser ~offset = Heap.Decoder.decode ~meta ~offset ty in
  let** value = apply_parser ?ignore_borrow ptr parser in
  L.debug (fun f ->
      f "Finished reading rust value %a" (Rust_val.pp Sptr.pp) value);
  if ref_checks then
    let++ () = Encoder.check_valid ~fake_read value ty in
    value
  else Result.ok value

and load_discriminant ((ptr, _) as fptr) ty =
  let** () = check_ptr_align fptr ty in
  let parser ~offset = Heap.Decoder.variant_of_enum ty ~offset in
  apply_parser ptr parser

(** Performs a side-effect free ghost read -- this does not modify the state or
    the tree-borrow state. Returns [Some error] if an error occurred, and [None]
    otherwise *)
(* We can't return a [Rustsymex.Result.t] here, because it's used in [load]
   which expects a [Tree_block.serialized_atom list] for the [Missing] case,
   while the external signature expects a [serialized]. This could be fixed by
   lifting all misses individually inside [handler] and [get_all] in
   [apply_parser], but that's kind of a mess to change and not really worth it I
   believe; I don't think these misses matter at all (TBD). *)
and fake_read ((_, meta) as ptr) ty =
  let open Syntax in
  let is_uncheckable_dst =
    match meta with
    | Thin -> false
    | Len l ->
        (* TODO: we don't support symbolic slices *)
        Option.is_none (Typed.BitVec.to_z l)
    | VTable _ ->
        (* FIXME: i am not certain how one checks for the validity of a &dyn *)
        true
  in
  if is_uncheckable_dst then return None
  else (
    L.debug (fun m ->
        m "Checking validity of %a for %a" (pp_full_ptr Sptr.pp) ptr
          Charon_util.pp_ty ty);
    let+ res = load ~ignore_borrow:true ~ref_checks:false ptr ty in
    match res with
    | Ok _ -> None
    | Error e -> Some e
    | Missing _ -> failwith "Miss in fake_read")

(** Performs a load at the tree borrow level, by updating the borrow state,
    without attempting to validate the values or checking uninitialised memory
    accesses; all of these are ignored. *)
let tb_load ((ptr : Sptr.t), _) ty =
  let@ () = with_loc_err ~trace:"Tree Borrow access" () in
  let open SM in
  match ptr.tag with
  | None -> Result.ok ()
  | Some tag ->
      let**^ size = Layout.size_of ty in
      if%sat size ==@ Usize.(0s) then Result.ok ()
      else
        let* () = log "tb_load" ptr in
        let@ ofs = with_ptr ptr in
        Block.with_tree_block_read_tb (Tree_block.tb_access ofs size tag)

let store ((ptr, _) as fptr) ty sval :
    (unit, Error.with_trace, serialized list) Result.t =
  let@ () = with_loc_err ~trace:"Memory store" () in
  let**^ parts = Encoder.encode ~offset:Usize.(0s) sval ty in
  if Iter.is_empty parts then Result.ok ()
  else
    let** () = check_ptr_align fptr ty in
    (* L.debug (fun f ->
     *   f "Parsed to parts [%a]"
     *     Fmt.(list ~sep:comma Encoder.pp_cval_info)
     *     parts); *)
    let* () = log "store" ptr in
    let**^ size = Layout.size_of ty in
    let@ ofs = with_ptr ptr in
    Block.with_tree_block_read_tb (fun tb ->
        let open Tree_block.SM in
        let open Tree_block.SM.Syntax in
        (* We uninitialise the whole range before writing, to ensure padding
           bytes are copied if there are any. *)
        let** () = Tree_block.uninit_range ofs size in
        Result.iter_iter parts ~f:(fun (value, offset) ->
            Tree_block.store (offset +!!@ ofs) value ptr.tag tb))

let check_ptr_align ptr ty =
  let@ () = with_loc_err ~trace:"Requires well-aligned pointer" () in
  check_ptr_align ptr ty

let load ?ignore_borrow ptr ty =
  let@ () = with_loc_err ~trace:"Memory load" () in
  load ?ignore_borrow ptr ty

let load_discriminant ptr ty =
  let@ () = with_loc_err ~trace:"Memory load (discriminant)" () in
  load_discriminant ptr ty

let copy_nonoverlapping ~dst:(dst, _) ~src:(src, _) ~size :
    (unit, Error.with_trace, serialized list) Result.t =
  let@ () = with_loc_err ~trace:"Non-overlapping copy" () in
  let** tree_to_write =
    let@ ofs = with_ptr src in
    Block.with_tree_block (fun tree_block ->
        let open DecayMapMonad.Syntax in
        let+ res, _ = Tree_block.get_raw_tree_owned ofs size tree_block in
        (res, tree_block))
  in
  let@ ofs = with_ptr dst in
  Block.with_tree_block
    (let open Tree_block.SM in
     let open Tree_block.SM.Syntax in
     (* We need to be careful about tree borrows; the tree we copy has a tree
        borrow state inside, but we cannot copy that, since those tags don't
        belong to this allocation! Instead, we must first get the tree borrow
        states for the original area of memory, and update the copied tree with
        them. We only want to update the values in the tree, not the tree borrow
        state. *)
     let module Tree = Tree_block.Tree in
     let** original_tree =
       let* state = get_state () in
       with_state ~state @@ Tree_block.get_raw_tree_owned ofs size
     in
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
       try DecayMapMonad.Result.ok (aux tb t)
       with Failure msg -> DecayMapMonad.not_impl msg
     in
     (* Applies all the tree borrow ranges to the tree we're writing,
        overwriting all previous states. *)
     let**^ tree_to_write =
       DecayMapMonad.Result.fold_iter collect_tb_states ~init:tree_to_write
         ~f:(fun tree (tb, range) ->
           let open DecayMapMonad.Syntax in
           let replace_node = put_tb tb in
           let rebuild_parent = Tree.of_children in
           let++ _, tree =
             Tree.frame_range tree ~rebuild_parent ~replace_node range
           in
           tree)
     in
     Tree_block.put_raw_tree ofs tree_to_write)

let alloc ?kind ?span ?zeroed size align =
  with_heap
    (let open Heap.SM in
     let open Heap.SM.Syntax in
     let*^ block, tag =
       Freeable_block_with_meta.make ?kind ?span ?zeroed ~align ~size ()
     in
     let** loc = Heap.alloc ~new_codom:block in
     let ptr = Typed.Ptr.mk loc Usize.(0s) in
     let ptr : Sptr.t = { ptr; tag; align; size } in
     (* The pointer is necessarily not null *)
     let+ () = assume [ Typed.(not (Ptr.is_null_loc loc)) ] in
     ok (ptr, Thin))

let alloc_untyped ?kind ?span ~zeroed ~size ~align =
  alloc ?kind ?span ~zeroed size align

let alloc_ty ?kind ?span ty =
  let@ () = with_loc_err ~trace:"Allocation" () in
  let**^ layout = Layout.layout_of ty in
  alloc ?kind ?span layout.size layout.align

let alloc_tys ?kind ?span tys : ('a, Error.with_trace, serialized list) Result.t
    =
  let@ () = with_loc_err ~trace:"Allocation" () in
  let**^ layouts = Rustsymex.Result.map_list tys ~f:Layout.layout_of in
  let layouts = List.rev layouts in
  with_heap
    (Heap.allocs ~els:layouts ~fn:(fun layout loc ->
         let open DecayMapMonad in
         let open DecayMapMonad.Syntax in
         (* make Tree_block *)
         let { size; align; _ } : Layout.t = layout in
         let* block, tag =
           Freeable_block_with_meta.make ?kind ?span ~align ~size ()
         in
         (* create pointer *)
         let* () = assume [ Typed.(not (Ptr.is_null_loc loc)) ] in
         let ptr = Typed.Ptr.mk loc Usize.(0s) in
         let ptr : Sptr.t = { ptr; tag; align; size } in
         return ((ptr, Thin), block)))

let free (({ ptr; _ } : Sptr.t), _) =
  let@ () = with_loc_err ~trace:"Freeing memory" () in
  let** () = assert_or_error (Typed.Ptr.ofs ptr ==@ Usize.(0s)) `InvalidFree in
  with_heap
    (L.trace (fun m -> m "Freeing pointer %a" Typed.ppa ptr);
     (* TODO: freeing encurs a write access on the whole allocation, and also
        requires there to be no strong protectors in the tree. We currently
        don't have a notion of strong or weak protector.

        See:
        https://github.com/minirust/minirust/blob/master/spec/mem/tree_borrows/memory.md *)
     Heap.wrap (Typed.Ptr.loc ptr)
       (Freeable_block_with_meta.wrap (Freeable_block.free ())))

let zeros (ptr, _) size =
  let@ () = with_loc_err ~trace:"Memory store (0s)" () in
  let* () = log "zeroes" ptr in
  let@ ofs = with_ptr ptr in
  Block.with_tree_block (Tree_block.zero_range ofs size)

let with_globals () f =
  let* st = SM.get_state () in
  let st = of_opt st in
  let res, globals = f st.globals in
  let+ () = SM.set_state (Some { st with globals }) in
  Ok res

let store_str_global str ptr =
  let@ globals = with_globals () in
  let globals = GlobMap.add (String str) ptr globals in
  ((), globals)

let store_global g ptr =
  let@ globals = with_globals () in
  let globals = GlobMap.add (Global g) ptr globals in
  ((), globals)

let load_str_global str =
  let@ globals = with_globals () in
  let ptr = GlobMap.find_opt (String str) globals in
  (ptr, globals)

let load_global g =
  let@ globals = with_globals () in
  let ptr = GlobMap.find_opt (Global g) globals in
  (ptr, globals)

let borrow ((ptr : Sptr.t), meta) (ty : Types.ty)
    (kind : Expressions.borrow_kind) =
  let@ () = with_loc_err ~trace:"Borrow" () in
  (* &UnsafeCell<T> are treated as raw pointers, and reuse parent's tag! *)
  if Option.is_none ptr.tag || (kind = BShared && Layout.is_unsafe_cell ty) then
    Result.ok (ptr, meta)
  else
    let@ _ = with_ptr ptr in
    Block.borrow (ptr, meta) ty kind

let protect ((ptr : Sptr.t), meta) (ty : Types.ty) (mut : Types.ref_kind) =
  let@ () = with_loc_err ~trace:"Reference protection" () in
  if Option.is_none ptr.tag || Layout.is_unsafe_cell ty then
    Result.ok (ptr, meta)
  else
    let**^ size = Layout.size_of ty in
    let@ ofs = with_ptr ptr in
    Block.protect (ptr, meta) mut ofs size

let unprotect ((ptr : Sptr.t), _) (ty : Types.ty) =
  let@ () = with_loc_err ~trace:"Reference unprotection" () in
  match ptr.tag with
  | None -> Result.ok ()
  | Some tag ->
      let**^ size = Layout.size_of ty in
      let@ ofs = with_ptr ptr in
      L.debug (fun m -> m "Unprotecting pointer %a" Sptr.pp ptr);
      Block.unprotect ofs tag size

let with_exposed addr =
  let@ () = with_loc_err ~trace:"Casting integer to pointer" () in
  match (Config.get ()).provenance with
  | Strict -> Result.error `UBIntToPointerStrict
  | Permissive ->
      with_heap
        (let open Heap.SM in
         let open Heap.SM.Syntax in
         let*^ res = DecayMap.from_exposed addr in
         match res with
         | None -> Result.ok (Sptr.null_ptr_of addr, Thin)
         | Some (loc, ofs) -> (
             let ofs = addr -!@ ofs in
             let ptr = Typed.Ptr.mk loc ofs in
             let** block =
               Heap.wrap loc @@ Freeable_block_with_meta.SM.Result.get_state ()
             in
             match (block : Freeable_block_with_meta.t option) with
             | None | Some { info = None; _ } -> Result.miss []
             | Some { info = Some { size; align; _ }; _ } ->
                 let ptr : Sptr.t = { ptr; tag = None; align; size } in
                 Result.ok (ptr, Thin)))

let leak_check () : (unit, Error.with_trace, serialized list) Result.t =
  let@ () = with_loc_err ~trace:"Leaking function" () in
  let* st = SM.get_state () in
  let st = of_opt st in
  let** global_addresses =
    Result.fold_list (GlobMap.bindings st.globals) ~init:[]
      ~f:(fun acc (g, ((ptr : Sptr.t), _)) ->
        let loc = Typed.Ptr.loc ptr.ptr in
        match g with
        | String _ -> Result.ok (loc :: acc)
        | Global g -> (
            let glob = Crate.get_global g in
            let* res = load ~ignore_borrow:true (ptr, Thin) glob.ty in
            match res with
            | Ok v ->
                let ptrs = Encoder.ref_tys_in ~include_ptrs:true v glob.ty in
                let ptrs =
                  List.map
                    (fun (((p : Sptr.t), _), _) -> Typed.Ptr.loc p.ptr)
                    ptrs
                in
                Result.ok (loc :: (ptrs @ acc))
            | _ -> Result.ok acc))
  in
  with_heap
    (let open Heap.SM in
     let open Heap.SM.Syntax in
     let* heap = get_state () in
     let**^ leaks =
       Heap.fold
         (fun leaks (k, (v : Freeable_block_with_meta.t)) ->
           (* FIXME: This only works because our addresses are concrete *)
           let open DecayMapMonad in
           match v with
           | { node = Alive _; info = Some { kind = Heap; span; _ }; _ }
             when not (List.mem k global_addresses) ->
               Result.ok ((k, span) :: leaks)
           | _ -> Result.ok leaks)
         [] heap
     in
     if List.is_empty leaks then Result.ok ()
     else (
       L.info (fun m ->
           let pp_leak ft (k, span) =
             Fmt.pf ft "%a (allocated at %a)" Typed.ppa k
               Charon_util.pp_span_data span
           in
           m "Found leaks: %a" Fmt.(list ~sep:(any ", ") pp_leak) leaks);
       let spans = List.map snd leaks in
       Result.error (`MemoryLeak spans)))

let with_errors () (f : Error.with_trace list -> 'a * Error.with_trace list) :
    ('a, Error.with_trace, serialized list) Result.t =
  let* st_opt = SM.get_state () in
  let st = of_opt st_opt in
  let res, errors = f st.errors in
  let+ () = SM.set_state (Some { st with errors }) in
  Ok res

let add_error e =
  let@ errors = with_errors () in
  ((), e :: errors)

let pop_error () =
  let** error =
    let@ errors = with_errors () in
    match errors with
    | e :: rest -> (e, rest)
    | _ -> failwith "pop_error with no errors?"
  in
  Result.error error

let with_functions (type a) (f : FunBiMap.t -> a * FunBiMap.t) : a SM.t =
  let* st = SM.get_state () in
  let st = of_opt st in
  let res, functions = f st.functions in
  let+ () = SM.set_state (Some { st with functions }) in
  res

let declare_fn fn_def =
  let align = Usize.(16s) in
  let* result =
    with_functions (fun fns -> (FunBiMap.get_loc fn_def fns, fns))
  in
  match result with
  | Some loc ->
      let ptr : Sptr.t =
        {
          ptr = Typed.Ptr.mk loc Usize.(0s);
          tag = None;
          align;
          size = Usize.(0s);
        }
      in
      Result.ok (ptr, Thin)
  | None ->
      let span =
        match fn_def with
        | Real fn -> Some (Crate.get_fun fn.id).item_meta.span.data
        | Synthetic _ -> None
      in
      let** ptr, meta =
        alloc_untyped ~kind:(Function fn_def) ?span ~zeroed:false
          ~size:Usize.(0s)
          ~align
      in
      let ptr = { ptr with tag = None } in
      let loc = Typed.Ptr.loc ptr.ptr in
      let* () = with_functions (fun fns -> ((), FunBiMap.add loc fn_def fns)) in
      Result.ok (ptr, meta)

let lookup_fn (({ ptr; _ } : Sptr.t), _) =
  let@ () = with_loc_err ~trace:"Accessing function pointer" () in
  let* st_opt = SM.get_state () in
  let st = of_opt st_opt in
  let** () =
    assert_or_error (Typed.Ptr.ofs ptr ==@ Usize.(0s)) `MisalignedFnPointer
  in
  let loc = Typed.Ptr.loc ptr in
  match FunBiMap.get_fn loc st.functions with
  | Some fn -> Result.ok fn
  | None -> Result.error `NotAFnPointer

let lookup_const_generic id ty =
  let@ () = with_loc_err ~trace:"Accessing const generic" () in
  let* st_opt = SM.get_state () in
  let st = of_opt st_opt in
  match Types.ConstGenericVarId.Map.find_opt id st.const_generics with
  | Some v -> Result.ok v
  | None ->
      let**^ v = Encoder.nondet ty in
      let const_generics =
        Types.ConstGenericVarId.Map.add id v st.const_generics
      in
      let+ () = SM.set_state (Some { st with const_generics }) in
      Ok v

let register_thread_exit callback =
  let* st_opt = SM.get_state () in
  let st = of_opt st_opt in
  let thread_destructor () =
    let** () = st.thread_destructor () in
    callback ()
  in
  SM.Result.set_state (Some { st with thread_destructor })

let run_thread_exits () =
 fun st_opt ->
  let st = of_opt st_opt in
  st.thread_destructor () st_opt
