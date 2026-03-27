open Soteria.Symex.Compo_res
open Rust_val
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Rustsymex
open Charon
open Common
open Sptr

module Make (Tree_borrows : Tree_borrows.T) = struct
  module Tree_borrows = Tree_borrows (DecayMapMonad)

  (* Pointer implementation *)

  module Sptr_base = struct
    open Rustsymex.Syntax

    type t = {
      ptr : Typed.(T.sptr t);
      tag : Tree_borrows.tag option;
      align : Typed.(T.nonzero t);
      size : Typed.(T.sint t);
    }

    let pp fmt { ptr; tag; _ } =
      Fmt.pf fmt "%a[%a]" Typed.ppa ptr
        Fmt.(option ~none:(any "*") Tree_borrows.pp_tag)
        tag

    let null_ptr () =
      {
        ptr = Typed.Ptr.null ();
        tag = None;
        align = Usize.(1s);
        size = Usize.(0s);
      }

    let of_address ofs =
      let null_ptr = null_ptr () in
      let ptr = Typed.Ptr.add_ofs null_ptr.ptr ofs in
      { null_ptr with ptr }

    let is_null { ptr; _ } = Typed.Ptr.is_null ptr
    let has_provenance { ptr; _ } = Typed.not (Typed.Ptr.is_at_null_loc ptr)

    let have_same_provenance { ptr = ptr1; _ } { ptr = ptr2; _ } =
      Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2

    (** A simplified (and unsafe) version of [offset], that adds a signed
        bitvector to this pointer's offset. *)
    let raw_offset ptr off_by =
      let loc, ofs = Typed.Ptr.decompose ptr.ptr in
      let ofs', ovf = ofs +$?@ off_by in
      let++ () = assert_or_error (Typed.not ovf) `UBDanglingPointer in
      { ptr with ptr = Typed.Ptr.mk loc ofs' }

    let[@inline] _decay ~expose { ptr; align; size; _ } decay_map =
      let loc, ofs = Typed.Ptr.decompose ptr in
      let+ loc_int, decay_map =
        DecayMap.decay ~expose ~size ~align loc decay_map
      in
      L.debug (fun fmt -> fmt "Decay %a -> %a" Typed.ppa loc Typed.ppa loc_int);
      (loc_int +!!@ ofs, decay_map)

    let decay p = _decay ~expose:false p
    let expose p = _decay ~expose:true p

    let distance ({ ptr = ptr1; _ } as p1) ({ ptr = ptr2; _ } as p2) =
      let open DecayMapMonad.Syntax in
      if%sat have_same_provenance p1 p2 then
        DecayMapMonad.return (Typed.Ptr.ofs ptr1 -!@ Typed.Ptr.ofs ptr2)
      else
        let* ptr1 = decay p1 in
        let+ ptr2 = decay p2 in
        ptr1 -!@ ptr2

    let as_id { ptr; _ } = Typed.cast @@ Typed.Ptr.loc ptr
    let allocation_info { size; align; _ } = (Typed.cast size, Typed.cast align)

    let nondet ty =
      let** layout = Layout.layout_of ty in
      let* loc = nondet (Typed.t_loc ()) in
      let* ofs = nondet (Typed.t_usize ()) in
      let* tag, _ =
        DecayMapMonad.run_with_state ~state:DecayMap.empty
        @@ Tree_borrows.nondet_tag ()
      in
      let ptr = Typed.Ptr.mk loc ofs in
      let ptr = { ptr; tag; align = layout.align; size = layout.size } in
      Result.ok ptr

    let iter_vars { ptr; align; size; tag = _ } f =
      Typed.iter_vars ptr f;
      Typed.iter_vars align f;
      Typed.iter_vars size f

    let subst subst_var p =
      let ptr = Typed.subst subst_var p.ptr in
      let align = Typed.subst subst_var p.align in
      let size = Typed.subst subst_var p.size in
      { p with ptr; align; size }
  end

  (* State details *)
  module Encoder = Value_codec.Encoder (Sptr_base)

  (* State combinators *)

  module StateKey = struct
    include Typed

    type t = T.sloc Typed.t

    let pp = ppa
    let to_int = unique_tag
    let concrete_loc = ref 0
    let simplify = DecayMapMonad.simplify

    let distinct vs =
      match Config.get_mode () with
      | Compositional -> distinct vs
      | Whole_program -> v_true

    let fresh () =
      match Config.get_mode () with
      | Compositional -> DecayMapMonad.nondet (Typed.t_loc ())
      | Whole_program ->
          incr concrete_loc;
          DecayMapMonad.return (Ptr.loc_of_int !concrete_loc)
  end

  module Freeable = Soteria.Sym_states.Freeable.Make (DecayMapMonad)
  module Bi = Soteria.Sym_states.Bi_abd.Make (DecayMapMonad)
  module Tree_block = Rtree_block.Make (Tree_borrows) (Sptr_base)

  module Meta = struct
    type t = {
      align : Typed.T.nonzero Typed.t;
      size : Typed.T.sint Typed.t;
      tb_root : Tree_borrows.tag;
      kind : Alloc_kind.t;
      trace : Trace.t;
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
    type t = Tree_block.t option * Tree_borrows.t option
    [@@deriving show { with_path = false }]

    let of_opt = function None -> (None, None) | Some (b1, b2) -> (b1, b2)

    let to_opt : t -> t option = function
      | None, None -> None
      | b1, b2 -> Some (b1, b2)

    type serialized =
      | Block of Tree_block.serialized
      | Borrow of Tree_borrows.serialized
    [@@deriving show { with_path = false }]

    module SM =
      Soteria.Sym_states.State_monad.Make
        (DecayMapMonad)
        (struct
          type nonrec t = t option
        end)

    open SM
    open Syntax

    let lift_fix_borrow s = Borrow s

    let lift_fix_block (s : Tree_block.serialized) =
      (* HACK: this is quite ugly; tree borrow misses relating to the structure
         (not the state!) can occur within [Tree_block], however fixing them
         there is not possible, since the structure is stored here, in [Block].
         As such we catch those, and lift them appropriately here. *)
      match s with
      | MemVal { v = STree_borrow s; _ } -> Borrow s
      | s -> Block s

    let lift_fix_borrow_r r =
      Soteria.Symex.Compo_res.map_missing r (List.map lift_fix_borrow)

    let lift_fix_block_r r =
      Soteria.Symex.Compo_res.map_missing r (List.map lift_fix_block)

    let with_tree_block (f : ('a, 'err, 'fix) Tree_block.SM.Result.t) :
        ('a, 'err, serialized list) Result.t =
      let* t_opt = SM.get_state () in
      let tree, tb = of_opt t_opt in
      let*^ v, tree = f tree in
      let+ () = SM.set_state (to_opt (tree, tb)) in
      lift_fix_block_r v

    let with_tree_block_read_tb
        (f : Tree_borrows.t option -> ('a, 'err, 'fix) Tree_block.SM.Result.t) :
        ('a, 'err, serialized list) SM.Result.t =
      let* t_opt = SM.get_state () in
      let tree, tb = of_opt t_opt in
      let*^ v, tree = f tb tree in
      let+ () = SM.set_state (to_opt (tree, tb)) in
      lift_fix_block_r v

    (** Borrows a given pointer. [ty] is the type of the pointer/reference/box
        being reborrowed. *)
    let borrow ?(protect = false) ((ptr : Sptr_base.t), meta) tag
        (ty : Types.ty) ofs =
      let pointee = Charon_util.get_pointee ty in
      let state =
        match (ty, Layout.is_unsafe_cell pointee) with
        | TRef (_, _, RShared), false -> Tree_borrows.Frozen
        | TRef (_, _, RShared), true -> Tree_borrows.Cell
        | _, false -> Tree_borrows.Reserved false
        | _, true -> Tree_borrows.ReservedIM
      in
      let protector =
        match (protect, ty) with
        | false, _ -> None
        | true, TRef _ -> Some Tree_borrows.Strong
        | true, TAdt adt when Charon_util.adt_is_box adt ->
            Some Tree_borrows.Weak
        | true, _ -> failwith "Non-ref or box in borrow?"
      in
      let* t_opt = SM.get_state () in
      let block, tb = of_opt t_opt in
      let*^ res, tb' = Tree_borrows.borrow ~state ?protector tag tb in
      let** tag = return (lift_fix_borrow_r res) in
      let ptr' = { ptr with tag = Some tag } in
      L.debug (fun m ->
          m "%s pointer %a -> %a (%a)"
            (if protect then "Protecting" else "Borrowing")
            Sptr_base.pp ptr Sptr_base.pp ptr' Tree_borrows.pp_state state);
      if not protect then
        let+ () = SM.set_state (to_opt (block, tb')) in
        Ok (ptr', meta)
      else
        let**^ size = DecayMapMonad.lift @@ Layout.size_of pointee in
        if%sat size ==@ Usize.(0s) then
          let+ () = SM.set_state (to_opt (block, tb')) in
          Ok (ptr', meta)
        else
          let*^ res, block' = Tree_block.tb_access ofs size tag tb' block in
          let** () = return (lift_fix_block_r res) in
          let+ () = SM.set_state (to_opt (block', tb')) in
          Ok (ptr', meta)

    let unprotect ofs tag size =
      let* t_opt = SM.get_state () in
      let block, tb = of_opt t_opt in
      let*^ res, tb' = Tree_borrows.unprotect tag tb in
      let** () = return (lift_fix_borrow_r res) in
      let** (), block' =
        if%sat size ==@ Usize.(0s) then SM.Result.ok ((), block)
        else
          let*^ res, block' = Tree_block.unprotect ofs size tag tb' block in
          let++ () = return (lift_fix_block_r res) in
          ((), block')
      in
      SM.Result.set_state (to_opt (block', tb'))

    let assert_exclusively_owned t_opt =
      let open DecayMapMonad.Syntax in
      let a, _ = of_opt t_opt in
      let+ res = Tree_block.assert_exclusively_owned a in
      lift_fix_block_r res

    let serialize (block, borrow) =
      let s_block = Option.fold ~none:[] ~some:Tree_block.serialize block in
      let s_borrow = Option.fold ~none:[] ~some:Tree_borrows.serialize borrow in
      List.map lift_fix_block s_block @ List.map lift_fix_borrow s_borrow

    let subst_serialized f = function
      | Block s -> Block (Tree_block.subst_serialized f s)
      | Borrow s -> Borrow (Tree_borrows.subst_serialized f s)

    let iter_vars_serialized s f =
      match s with
      | Block s -> Tree_block.iter_vars_serialized s f
      | Borrow s -> Tree_borrows.iter_vars_serialized s f

    let produce s =
      let* st = get_state () in
      let st, tb = of_opt st in
      match s with
      | Block s ->
          let*^ (), st' = Tree_block.produce s st in
          set_state (to_opt (st', tb))
      | Borrow s ->
          let*^ (), tb' = Tree_borrows.produce s tb in
          set_state (to_opt (st, tb'))
  end

  module Freeable_block =
    Soteria.Sym_states.Freeable.Make (DecayMapMonad) (Block)

  module Freeable_block_with_meta = struct
    include
      Soteria.Sym_states.With_info.Make (DecayMapMonad) (Meta) (Freeable_block)

    let make ?(kind = Alloc_kind.Heap) ?span ?zeroed ~size ~align () :
        (t * Tree_borrows.tag option) DecayMapMonad.t =
      let open DecayMapMonad.Syntax in
      let* tb, tag = Tree_borrows.init () in
      let block = Tree_block.alloc ?zeroed size in
      let+^ trace = get_trace () in
      let trace = Trace.rename 0 "Allocation" trace in
      let trace = Trace.move_to_opt span trace in
      let info : Meta.t = { align; size; kind; trace; tb_root = tag } in
      let tag = if (Config.get ()).ignore_aliasing then None else Some tag in
      (({ node = Alive (Some block, Some tb); info = Some info } : t), tag)
  end

  module Heap = struct
    include
      Soteria.Sym_states.Pmap.Direct_access_patricia_tree
        (DecayMapMonad)
        (StateKey)
        (Freeable_block_with_meta)

    let with_ptr (ptr : Sptr_base.t)
        (f : [< T.sint ] Typed.t -> ('a, 'err, 'fix list) Block.SM.Result.t) :
        ('a, 'err, serialized list) SM.Result.t =
      let open SM in
      let open SM.Syntax in
      let** () =
        assert_or_error (Typed.not (Sptr_base.is_null ptr)) `NullDereference
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
          if%sat Typed.not (Sptr_base.has_provenance ptr) then
            Result.error `UBDanglingPointer
          else return miss
      | ok_or_err -> return ok_or_err

    let is_freed loc =
      wrap loc
        (let open Freeable_block_with_meta in
         let open SM.Syntax in
         let* block = SM.get_state () in
         match block with
         | Some { node = Freed; _ } -> SM.Result.ok true
         | _ -> SM.Result.ok false)

    module Decoder =
      Value_codec.Decoder
        (Sptr_base)
        (struct
          module SM = SM

          type fix = serialized list
        end)
  end

  type serialized = Heap of Heap.serialized
  [@@deriving show { with_path = false }]

  let subst_serialized subst = function
    | Heap s -> Heap (Heap.subst_serialized subst s)

  let iter_vars_serialized ser iter =
    match ser with Heap s -> Heap.iter_vars_serialized s iter

  type t = {
    heap : Heap.t option;
    functions : FunBiMap.t;
    globals : Sptr_base.t Rust_val.full_ptr GlobMap.t;
    errors : Error.with_trace list;
    pointers : DecayMap.t;
    thread_destructor :
      unit ->
      t option ->
      ((unit, Error.with_trace, serialized list) Soteria.Symex.Compo_res.t
      * t option)
      Rustsymex.t;
        [@printer Fmt.any "code"]
    const_generics : Sptr_base.t rust_val Types.ConstGenericVarId.Map.t;
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
      if ignore_freed then function
        | _, { node = Freed; _ } -> true | _ -> false
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
                    Fmt.option ~none:(Fmt.any "Empty Block")
                      Tree_block.pp_pretty fmt tb)
                  ft node)
          ft st

  let empty_state =
    {
      heap = None;
      functions = FunBiMap.empty;
      globals = GlobMap.empty;
      errors = [];
      pointers = DecayMap.empty;
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
        m "About to execute action: %s (%a)@\n@[<2>STATE:@ %a@]" action
          Sptr_base.pp ptr
          (Fmt.Dump.option (pp_pretty ~ignore_freed:true))
          st)

  let[@inline] with_loc_err ?trace:msg ()
      (f : unit -> ('a, Error.t, 'f) SM.Result.t) :
      ('a, Error.with_trace, 'f) SM.Result.t =
    let*- err = f () in
    let+^ trace = get_trace () in
    let trace =
      Option.fold ~none:trace ~some:(fun t -> Trace.set_op t trace) msg
    in
    Error.log_at trace err;
    Error (Error.decorate trace err)

  let with_heap_symex (f : 'a Heap.SM.t) : 'a SM.t =
    let* st = SM.get_state () in
    let st = of_opt st in
    let*^ (res, heap), pointers =
      DecayMapMonad.run_with_state ~state:st.pointers @@ f st.heap
    in
    let+ () = SM.set_state (Some { st with heap; pointers }) in
    res

  let with_heap (f : ('a, 'b, Heap.serialized list) Heap.SM.Result.t) :
      ('a, 'b, serialized list) Result.t =
    SM.Result.map_missing (with_heap_symex f) (fun fix ->
        List.map (fun h -> Heap h) fix)

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

  let uninit ((ptr, _) : Sptr_base.t * 'a) (ty : Types.ty) :
      (unit, 'err, 'fix) Result.t =
    let@ () = with_loc_err ~trace:"Uninitialising memory" () in
    let* () = log "uninit" ptr in
    let**^ size = Layout.size_of ty in
    let@ ofs = with_ptr ptr in
    Block.with_tree_block @@ Tree_block.uninit_range ofs size

  let rec size_and_align_of_val t meta =
    let* st = get_state () in
    let load_vtable field ptr =
      let open Rustsymex.Syntax in
      let usize : Types.ty = TLiteral (TUInt Usize) in
      let** field_size = Layout.size_of usize in
      let ofs = match field with `Size -> Usize.(1s) | `Align -> Usize.(2s) in
      let** ptr' = Sptr_base.raw_offset ptr (ofs *!!@ field_size) in
      let ptr' = (ptr', Thin) in
      let+ res, _ = load ~ignore_borrow:true ~check_refs:false ptr' usize st in
      res
    in
    lift @@ Encoder.size_and_align_of_val ~load_vtable ~t ~meta

  and check_ptr_align ((ptr, meta) : 'a full_ptr) (ty : Types.ty) =
    (* The expected alignment of a dyn pointer is stored inside the VTable *)
    let** _, exp_align = size_and_align_of_val ty meta in
    L.debug (fun m ->
        m "Checking pointer alignment of %a: expect %a for %a" Sptr_base.pp ptr
          Typed.ppa exp_align Common.Charon_util.pp_ty ty);
    let loc, ofs = Typed.Ptr.decompose ptr.ptr in
    (* A pointer with no provenance is aligned to it's offset *)
    let align = Typed.(ite (Ptr.is_null_loc loc) exp_align (cast ptr.align)) in
    let is_aligned =
      ofs %@ exp_align ==@ Usize.(0s) &&@ (align %@ exp_align ==@ Usize.(0s))
    in
    assert_or_error is_aligned (`MisalignedPointer (exp_align, align, ofs))

  and check_non_dangling_untyped ((ptr : Sptr_base.t), _) size =
    if%sat size ==@ Usize.(0s) then Result.ok ()
    else
      let** ptr, size =
        if%sat size >$@ Usize.(0s) then Result.ok (ptr, size)
        else
          let++^ ptr' = Sptr_base.raw_offset ptr size in
          (ptr', Typed.(cast (BV.neg size)))
      in
      let open Block.SM.Syntax in
      let@ ofs = with_ptr ptr in
      let+- _ =
        Block.with_tree_block (Tree_block.check_owned ofs (Typed.cast size))
      in
      `UBDanglingPointer

  and check_non_dangling ((_, meta) as ptr) (ty : Types.ty) =
    let** size, _ = size_and_align_of_val ty meta in
    check_non_dangling_untyped ptr size

  and load ?ignore_borrow ?(check_refs = true) ((ptr, meta) as fptr) ty :
      (Sptr_base.t rust_val, Error.t, serialized list) Result.t =
    let** () = check_ptr_align fptr ty in
    let parser ~offset = Heap.Decoder.decode ~meta ~offset ty in
    let** value = apply_parser ?ignore_borrow ptr parser in
    L.debug (fun f ->
        f "Finished reading rust value %a" (Rust_val.pp Sptr_base.pp) value);
    let check_ref =
      if (Config.get ()).recursive_validity <> Allow && check_refs then
        fun ptr ty ->
        (* we still need to check it's non-dangling! *)
        let** () = check_ptr_align ptr ty in
        let** () = check_non_dangling ptr ty in
        fake_read ptr ty
      else fun ptr ty ->
        let** () = check_ptr_align ptr ty in
        check_non_dangling ptr ty
    in
    let++ () = Encoder.check_validity ~check_ref ty value in
    value

  and load_discriminant ((ptr, _) as fptr) ty =
    let** () = check_ptr_align fptr ty in
    let parser ~offset = Heap.Decoder.variant_of_enum ty ~offset in
    apply_parser ptr parser

  (** Performs a side-effect free ghost read -- this does not modify the state
      or the tree-borrow state. Returns [Some error] if an error occurred, and
      [None] otherwise. Will wrap whatever error happened in [`InvalidRef]. *)
  and fake_read ((ptr, meta) as fptr) ty =
    let open Syntax in
    let skip_check =
      match (meta, (Config.get ()).recursive_validity) with
      | _, Allow -> true
      | _, Warn when Config.get_mode () = Compositional -> true
      | Thin, _ -> false
      | Len l, _ ->
          (* TODO: we don't support symbolic slices *)
          Option.is_none (Typed.BitVec.to_z l)
      | VTable _, _ ->
          (* FIXME: i am not certain how one checks for the validity of a
             &dyn *)
          true
    in
    if skip_check then Result.ok ()
    else (
      L.debug (fun m ->
          m "Checking validity of %a for %a" (pp_full_ptr Sptr_base.pp) fptr
            Charon_util.pp_ty ty);
      let*- err =
        let++ _ = load ~ignore_borrow:true ~check_refs:false fptr ty in
        ()
      in
      match (Config.get ()).recursive_validity with
      | Allow -> failwith "Unreachable, handled above"
      | Deny -> Result.error (`InvalidRef err)
      | Warn ->
          let*^ trace = get_trace () in
          let err = Error.decorate trace (`InvalidRef err) in
          let loc = Typed.Ptr.loc ptr.ptr in
          Error.Diagnostic.warn_trace_once ~reason:(InvalidReference loc) err;
          Result.ok ())

  (** Performs a load at the tree borrow level, by updating the borrow state,
      without attempting to validate the values or checking uninitialised memory
      accesses; all of these are ignored. *)
  let tb_load_untyped (ptr : Sptr_base.t) size =
    let open SM in
    match ptr.tag with
    | None -> Result.ok ()
    | Some tag ->
        if%sat size ==@ Usize.(0s) then Result.ok ()
        else
          let* () = log "tb_load" ptr in
          let@ ofs = with_ptr ptr in
          Block.with_tree_block_read_tb (Tree_block.tb_access ofs size tag)

  (** Performs a load at the tree borrow level, by updating the borrow state,
      without attempting to validate the values or checking uninitialised memory
      accesses; all of these are ignored. *)
  let tb_load ((ptr : Sptr_base.t), _) ty =
    let@ () = with_loc_err ~trace:"Tree Borrow access" () in
    let**^ size = Layout.size_of ty in
    tb_load_untyped ptr size

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

  module Sptr = struct
    include Sptr_base

    let offset ?(check = true) ?(ty = Types.TLiteral (TUInt U8)) ~signed off_by
        ({ ptr; _ } as fptr) =
      let@ () = with_loc_err ~trace:"Pointer offset" () in
      let**^ size = Layout.size_of ty in
      let loc, off = Typed.Ptr.decompose ptr in
      let ( *? ), ( +? ) =
        if signed then (( *$?@ ), ( +$?@ )) else (( *?@ ), ( +?@ ))
      in
      let off_by, off_by_ovf = size *? off_by in
      let off, off_ovf = off +? off_by in
      let++ () =
        if check then
          let** () =
            assert_or_error
              (off_by
              ==@ Usize.(0s)
              ||@ (Typed.not off_by_ovf &&@ Typed.not off_ovf))
              `UBDanglingPointer
          in
          check_non_dangling_untyped (fptr, Thin) off_by
        else Result.ok ()
      in
      let ptr' = Typed.Ptr.mk loc off in
      { fptr with ptr = ptr' }

    let check_aligned ptr ty =
      let@ () = with_loc_err ~trace:"Requires well-aligned pointer" () in
      check_ptr_align ptr ty

    let check_non_dangling_untyped ptr size =
      let@ () = with_loc_err ~trace:"Dangling check" () in
      check_non_dangling_untyped ptr size

    let check_non_dangling ptr ty =
      let@ () = with_loc_err ~trace:"Dangling check" () in
      check_non_dangling ptr ty
  end

  let size_and_align_of_val ty meta =
    let@ () = with_loc_err ~trace:"Size and alignment check" () in
    size_and_align_of_val ty meta

  let load ?ignore_borrow ptr ty =
    let@ () = with_loc_err ~trace:"Memory load" () in
    load ?ignore_borrow ptr ty

  let load_discriminant ptr ty =
    let@ () = with_loc_err ~trace:"Memory load (discriminant)" () in
    load_discriminant ptr ty

  let fake_read ptr ty =
    let@ () = with_loc_err ~trace:"Fake read" () in
    fake_read ptr ty

  let copy_nonoverlapping ~src:(src, _) ~dst:(dst, _) ~size :
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
          states for the original area of memory, and update the copied tree
          with them. We only want to update the values in the tree, not the tree
          borrow state. *)
       let module Tree = Tree_block.Tree in
       let** original_tree =
         let* state = get_state () in
         with_state ~state @@ Tree_block.get_raw_tree_owned ofs size
       in
       (* Iterator over the tree borrow states in a tree. *)
       let collect_tb_states f =
         Tree.iter_leaves_rev original_tree @@ fun (range, _, tb) ->
         let range =
           Tree_block.Range.offset range ~-!(fst original_tree.range)
         in
         f (tb, range)
       in
       (* Update a tree and its children with the given tree borrow state. *)
       let put_tb tb t =
         let rec aux tb (t : Tree.t) =
           match t.node with
           | NotOwned _ -> failwith "impossible: checked before"
           | Owned Lazy ->
               let l, r = Option.get t.children in
               { t with children = Some (aux tb l, aux tb r) }
           | Owned (Leaf (v, _)) -> { t with node = Owned (Leaf (v, tb)) }
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
       let ptr : Sptr_base.t = { ptr; tag; align; size } in
       (* The pointer is necessarily not null *)
       let+ () = assume [ Typed.(not (Ptr.is_null_loc loc)) ] in
       ok (ptr, Thin))

  let alloc_untyped ?kind ?span ~zeroed ~size ~align =
    alloc ?kind ?span ~zeroed size align

  let alloc_ty ?kind ?span ty =
    let@ () = with_loc_err ~trace:"Allocation" () in
    let**^ layout = Layout.layout_of ty in
    alloc ?kind ?span layout.size layout.align

  let alloc_tys ?kind ?span tys :
      ('a, Error.with_trace, serialized list) Result.t =
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
           let ptr : Sptr_base.t = { ptr; tag; align; size } in
           return ((ptr, Thin), block)))

  let free ((ptr : Sptr_base.t), _) =
    let@ () = with_loc_err ~trace:"Freeing memory" () in
    let loc, ofs = Typed.Ptr.decompose ptr.ptr in
    let** () = assert_or_error (ofs ==@ Usize.(0s)) `InvalidFree in
    (* Freeing encurs a write access on the whole allocation. *)
    let** () = tb_load_untyped ptr ptr.size in
    (* Freeing also requires there to be no strong protectors in the tree. See:
       https://github.com/minirust/minirust/blob/master/spec/mem/tree_borrows/memory.md *)
    let** () =
      with_ptr ptr (fun _ ->
          Block.with_tree_block_read_tb (fun tb ->
              if Tree_borrows.strong_protector_exists tb then
                Tree_block.SM.Result.error `InvalidFreeStrongProtector
              else Tree_block.SM.Result.ok ()))
    in
    L.debug (fun m -> m "Freeing pointer %a" Sptr_base.pp ptr);
    with_heap
      (Heap.wrap loc (Freeable_block_with_meta.wrap (Freeable_block.free ())))

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

  let borrow ?protect (((ptr : Sptr_base.t), _) as fptr) (ty : Types.ty) =
    let@ () = with_loc_err ~trace:"Borrow" () in
    (* &UnsafeCell<T> are treated as raw pointers, and reuse parent's tag! *)
    match ptr.tag with
    | None -> Result.ok fptr
    | Some tag ->
        let@ ofs = with_ptr ptr in
        Block.borrow ?protect fptr tag ty ofs

  let unprotect ((ptr : Sptr_base.t), _) (ty : Types.ty) =
    let@ () = with_loc_err ~trace:"Reference unprotection" () in
    match ptr.tag with
    | None -> Result.ok ()
    | Some tag ->
        let** freed = with_heap @@ Heap.is_freed (Typed.Ptr.loc ptr.ptr) in
        if freed then Result.ok ()
        else
          let**^ size = Layout.size_of ty in
          let@ ofs = with_ptr ptr in
          L.debug (fun m -> m "Unprotecting pointer %a" Sptr_base.pp ptr);
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
           | None -> Result.ok (Sptr_base.of_address addr, Thin)
           | Some (loc, ofs) -> (
               let ofs = addr -!@ ofs in
               let ptr = Typed.Ptr.mk loc ofs in
               let** block =
                 Heap.wrap loc
                 @@ Freeable_block_with_meta.SM.Result.get_state ()
               in
               match (block : Freeable_block_with_meta.t option) with
               | None | Some { info = None; _ } ->
                   Result.miss_no_fix ()
                     ~reason:
                       "Get a pointer from exposed with no matching allocation?"
               | Some { info = Some { size; align; _ }; _ } ->
                   let ptr : Sptr_base.t = { ptr; tag = None; align; size } in
                   Result.ok (ptr, Thin)))

  let leak_check () : (unit, Error.with_trace, serialized list) Result.t =
    (* FIXME: this is an unnecessarily complicated function; what we should do
       is properly track what allocations come from a const/static (with
       Alloc_kind), and then simply iterate over all allocations and look for
       non-const/static allocations. *)
    let* st = SM.get_state () in
    let st = of_opt st in
    let* global_addresses =
      fold_list (GlobMap.bindings st.globals) ~init:[]
        ~f:(fun acc (g, ((ptr : Sptr_base.t), _)) ->
          let loc = Typed.Ptr.loc ptr.ptr in
          match g with
          | String _ -> return (loc :: acc)
          | Global g -> (
              let glob = Crate.get_global g in
              let* res = load ~ignore_borrow:true (ptr, Thin) glob.ty in
              match res with
              | Ok v ->
                  let ptrs = Encoder.ref_tys_in ~include_ptrs:true v glob.ty in
                  let ptrs =
                    List.map
                      (fun (((p : Sptr_base.t), _), _) -> Typed.Ptr.loc p.ptr)
                      ptrs
                  in
                  return (loc :: (ptrs @ acc))
              | _ -> return acc))
    in
    with_heap
      (let open Heap.SM in
       let open Heap.SM.Syntax in
       let* heap = get_state () in
       let*^ leaks =
         Heap.fold
           (fun leaks (k, (v : Freeable_block_with_meta.t)) ->
             (* FIXME: This only works because our addresses are concrete *)
             let open DecayMapMonad in
             match v with
             | { node = Alive _; info = Some { kind = Heap; trace; _ }; _ }
               when not (List.mem k global_addresses) ->
                 return ((k, trace) :: leaks)
             | _ -> return leaks)
           [] heap
       in
       if List.is_empty leaks then Result.ok ()
       else (
         L.info (fun m ->
             let pp_leak ft (k, trace) =
               Fmt.pf ft "%a (allocated at %a)" Typed.ppa k Trace.pp trace
             in
             m "Found leaks: %a" Fmt.(list ~sep:(any ", ") pp_leak) leaks);
         let wheres = List.map snd leaks in
         let* leak_trace = branches (List.map (fun t () -> return t) wheres) in
         let leak_trace =
           Trace.rename ~rev:true 0 "Leaking function" leak_trace
         in
         Result.error (Error.decorate leak_trace `MemoryLeak)))

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
        let ptr : Sptr_base.t =
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
        let* () =
          with_functions (fun fns -> ((), FunBiMap.add loc fn_def fns))
        in
        Result.ok (ptr, meta)

  let lookup_fn (({ ptr; _ } : Sptr_base.t), _) =
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
        let**^ v = Encoder.nondet_valid ty in
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

  let serialize { heap; _ } =
    Heap.of_opt heap |> Heap.serialize |> List.map (fun h -> Heap h)

  let produce s st =
    match s with Heap h -> with_heap_symex (Heap.produce h) st
end
