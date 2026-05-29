open Compo_res
open Svalue
open Rust_val
open Typed.Infix
open Typed.Syntax
module T = Typed.T
open Rustsymex
open Charon
open Common
open Charon_util
open Sptr

module Make (Borrows : Tree_borrows.T) = struct
  module Borrows = Borrows (DecayMap.SM)

  (* Pointer implementation *)

  module Sptr_base = struct
    type ('sptr, 'snonzero, 'sint) base = {
      ptr : 'sptr;
      tag : Ptr_tag.t option;
      align : 'snonzero;
      size : 'sint;
    }

    type t = (T.sptr Typed.t, T.nonzero Typed.t, T.sint Typed.t) base
    type syn = (Typed.Expr.t, Typed.Expr.t, Typed.Expr.t) base

    let pp' pp_v fmt { ptr; tag; _ } =
      Fmt.pf fmt "%a[%a]" pp_v ptr Fmt.(option ~none:(any "*") Ptr_tag.pp) tag

    let pp = pp' Typed.ppa
    let show = Fmt.to_to_string pp
    let pp_syn = pp' Typed.Expr.pp
    let show_syn = Fmt.to_to_string pp_syn

    let to_syn { ptr; tag; align; size } =
      {
        ptr = Typed.Expr.of_value ptr;
        align = Typed.Expr.of_value align;
        size = Typed.Expr.of_value size;
        (* TODO: for now, pointer tags are never serialized *)
        tag = None;
      }

    let learn_eq syn t =
      let open DecayMap.SM.Consumer in
      let open Syntax in
      let* () =
        if Option.equal Ptr_tag.equal syn.tag t.tag then ok ()
        else lfail Typed.v_false
      in
      let* () = learn_eq syn.ptr t.ptr in
      let* () = learn_eq syn.align t.align in
      learn_eq syn.size t.size

    (* TODO: tags are concrete *)
    let exprs_syn { ptr; align; size; tag = _ } = [ ptr; align; size ]
    let fresh () = failwith "Fresh unimplemented for sptr (for now)"

    let subst subst_val p =
      let se = Typed.Expr.subst subst_val in
      let ptr = se p.ptr in
      let align = se p.align in
      let size = se p.size in
      { ptr; align; size; tag = p.tag }

    let null () =
      {
        ptr = Typed.Ptr.null ();
        tag = None;
        align = Usize.(1s);
        size = Usize.(0s);
      }

    let of_address ofs =
      let null_ptr = null () in
      let ptr = Typed.Ptr.add_ofs null_ptr.ptr ofs in
      { null_ptr with ptr }

    let is_null { ptr; _ } = Typed.Ptr.is_null ptr
    let has_provenance { ptr; _ } = Typed.not (Typed.Ptr.is_at_null_loc ptr)

    let have_same_provenance { ptr = ptr1; _ } { ptr = ptr2; _ } =
      Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2

    (** A simplified (and unsafe) version of [offset], that adds a signed
        bitvector to this pointer's offset. *)
    let raw_offset ptr off_by =
      let open Rustsymex.Syntax in
      let loc, ofs = Typed.Ptr.decompose ptr.ptr in
      let ofs', ovf = ofs +$?@ off_by in
      let++ () = assert_or_error (Typed.not ovf) `UBDanglingPointer in
      { ptr with ptr = Typed.Ptr.mk loc ofs' }

    let[@inline] _decay ~expose { ptr; align; size; _ } =
      let open DecayMap.SM.Syntax in
      let loc, ofs = Typed.Ptr.decompose ptr in
      let+ loc_int = DecayMap.decay ~expose ~size ~align loc in
      [%l.debug "Decay %a -> %a" Typed.ppa loc Typed.ppa loc_int];
      loc_int +!!@ ofs

    let decay p = _decay ~expose:false p
    let expose p = _decay ~expose:true p

    let distance ({ ptr = ptr1; _ } as p1) ({ ptr = ptr2; _ } as p2) =
      let open DecayMap.SM.Syntax in
      if%sat have_same_provenance p1 p2 then
        DecayMap.SM.return (Typed.Ptr.ofs ptr1 -!@ Typed.Ptr.ofs ptr2)
      else
        let* ptr1 = decay p1 in
        let+ ptr2 = decay p2 in
        ptr1 -!@ ptr2

    let as_id { ptr; _ } = Typed.cast @@ Typed.Ptr.loc ptr
    let allocation_info { size; align; _ } = (Typed.cast size, Typed.cast align)

    let nondet ty =
      let open Rustsymex.Syntax in
      let** layout = Layout.layout_of ty in
      let* loc = nondet (Typed.t_loc ()) in
      let* ofs = nondet (Typed.t_usize ()) in
      (* TODO: nondet pointer tags? *)
      let tag = None in
      let ptr = Typed.Ptr.mk loc ofs in
      let ptr = { ptr; tag; align = layout.align; size = layout.size } in
      Result.ok ptr
  end

  (* State details *)
  module Encoder = Value_codec.Encoder (Sptr_base)

  (* State combinators *)

  module StateKey = struct
    include Typed

    type t = T.sloc Typed.t
    type syn = Expr.t [@@deriving show]

    let to_syn v = Expr.of_value v
    let learn_eq (s : syn) (v : t) = DecayMap.SM.Consumer.learn_eq s v
    let exprs_syn v = [ v ]
    let subst = Expr.subst
    let pp = ppa
    let show = Fmt.to_to_string pp
    let to_int = unique_tag
    let concrete_loc = ref 0
    let simplify = DecayMap.SM.simplify

    let distinct_seq vs =
      match Config.get_mode () with
      | Compositional -> distinct_seq vs
      | Whole_program -> v_true

    let fresh () =
      match Config.get_mode () with
      | Compositional -> DecayMap.SM.nondet (Typed.t_loc ())
      | Whole_program ->
          incr concrete_loc;
          DecayMap.SM.return (Ptr.loc_of_int !concrete_loc)
  end

  module Freeable = Soteria.Sym_states.Freeable.Make (DecayMap.SM)
  module Tree_block = Rtree_block.Make (Borrows) (Sptr_base)

  module Meta = struct
    type t = {
      align : Typed.T.nonzero Typed.t;
      size : Typed.T.sint Typed.t;
      tb_root : Ptr_tag.t;
      kind : Alloc_kind.t;
      trace : Trace.t;
    }
    [@@deriving show { with_path = false }]
  end

  type global = String of string | Global of Types.global_decl_id

  module GlobMap = Map.Make (struct
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
          let show = Fmt.to_to_string pp
        end)
        (Fun_kind)

    let get_fn = find_l
    let get_loc = find_r
  end

  module Block = struct
    type t = { block : Tree_block.t option; borrow : Borrows.Tree.t option }
    [@@deriving sym_state { symex = DecayMap.SM }]

    let pp_pretty ft { block; _ } =
      Fmt.option ~none:(Fmt.any "Empty Block") Tree_block.pp_pretty ft block

    open SM
    open Syntax

    let lift_fix_block (s : Tree_block.syn) =
      (* HACK: this is quite ugly; tree borrow misses relating to the structure
         (not the state!) can occur within [Tree_block], however fixing them
         there is not possible, since the structure is stored here, in [Block].
         As such we catch those, and lift them appropriately here. *)
      match s with
      | MemVal { v = STree_borrow s; _ } -> Ser_borrow s
      | s -> Ser_block s

    let lift_fix_block_r r = map_missing (List.map lift_fix_block) r

    let with_block (f : ('a, 'err, 'fix) Tree_block.SM.Result.t) :
        ('a, 'err, syn list) Result.t =
      let* t_opt = SM.get_state () in
      let { block; borrow } = of_opt t_opt in
      let*^ v, block = f block in
      let+ () = SM.set_state (to_opt { block; borrow }) in
      lift_fix_block_r v

    let with_block_read_tb
        (f : Borrows.Tree.t option -> ('a, 'err, 'fix) Tree_block.SM.Result.t) :
        ('a, 'err, syn list) SM.Result.t =
      let* t_opt = SM.get_state () in
      let { block; borrow } = of_opt t_opt in
      let*^ v, block = f borrow block in
      let+ () = SM.set_state (to_opt { block; borrow }) in
      lift_fix_block_r v

    let alloc ?zeroed size =
      let open DecayMap.SM.Syntax in
      let* block = Tree_block.alloc ?zeroed size in
      let+ tag, borrow = Borrows.Tree.init () in
      (tag, { block = Some block; borrow = Some borrow })

    (** Borrows a given pointer. [ty] is the type of the pointer/reference/box
        being reborrowed. *)
    let borrow ?(protect = false) ((ptr : Sptr_base.t), meta) tag
        (ty : Types.ty) ofs =
      let pointee = get_pointee ty in
      (* FIXME: this logic is tree borrows related and should be handled there.
         https://github.com/soteria-tools/soteria/issues/301 *)
      let state : Tree_borrows.state =
        match (ty, Layout.is_unsafe_cell pointee) with
        | TRef (_, _, RShared), false -> Frozen
        | TRef (_, _, RShared), true -> Cell
        | _, false -> Reserved false
        | _, true -> ReservedIM
      in
      let protector : Tree_borrows.protector option =
        match (protect, ty) with
        | false, _ -> None
        | true, TRef _ -> Some Strong
        | true, TAdt adt when adt_is_box adt -> Some Weak
        | true, _ -> failwith "Non-ref or box in borrow?"
      in
      let** tag = with_borrow (Borrows.Tree.borrow ~state ?protector tag) in
      let ptr' = { ptr with tag = Some tag } in
      [%l.debug
        "%s pointer %a -> %a (%a)"
          (if protect then "Protecting" else "Borrowing")
          Sptr_base.pp ptr Sptr_base.pp ptr' Tree_borrows.pp_state state];
      if not protect then Result.ok (ptr', meta)
      else
        let**^ size = DecayMap.SM.lift @@ Layout.size_of pointee in
        if%sat size ==@ Usize.(0s) then Result.ok (ptr', meta)
        else
          let++ () = with_block_read_tb (Tree_block.tb_access ofs size tag) in
          (ptr', meta)

    let unprotect ofs tag size =
      let** () = with_borrow (Borrows.Tree.unprotect tag) in
      if%sat size ==@ Usize.(0s) then SM.Result.ok ()
      else with_block_read_tb (Tree_block.unprotect ofs size tag)

    let assert_exclusively_owned () =
      let** () = with_block (Tree_block.assert_exclusively_owned ()) in
      with_borrow (Borrows.Tree.assert_exclusively_owned ())
  end

  module Freeable_block = Soteria.Sym_states.Freeable.Make (DecayMap.SM) (Block)

  module Freeable_block_with_meta = struct
    include
      Soteria.Sym_states.With_info.Make (DecayMap.SM) (Meta) (Freeable_block)

    type 'a raw = ('a Soteria.Sym_states.Freeable.freeable, Meta.t) with_info

    let[@inline] is_leakable : 'a. 'a raw -> bool = function
      | { node = Alive _; info = Some { kind = Heap; _ } } -> true
      | _ -> false

    let[@inline] trace : 'a. 'a raw -> Trace.t option = function
      | { info = Some { trace; _ }; _ } -> Some trace
      | _ -> None

    let[@inline] alloc_kind : t option -> Alloc_kind.t option = function
      | Some { info = Some { kind; _ }; _ } -> Some kind
      | _ -> None

    let make ?span ?zeroed ~size ~align () :
        (t * Ptr_tag.t option) DecayMap.SM.t =
      let open DecayMap.SM.Syntax in
      let* tag, block = Block.alloc ?zeroed size in
      let*^ kind = get_alloc_kind () in
      let+^ trace = get_trace () in
      let trace = Trace.rename 0 "Allocation" trace in
      let trace = Trace.move_to_opt span trace in
      let info : Meta.t = { align; size; kind; trace; tb_root = tag } in
      let tag = if (Config.get ()).ignore_aliasing then None else Some tag in
      (({ node = Alive block; info = Some info } : t), tag)
  end

  module Heap = struct
    include
      Soteria.Sym_states.Pmap.Direct_access_patricia_tree
        (DecayMap.SM)
        (StateKey)
        (Freeable_block_with_meta)

    type access = Ghost | Read | Write

    let with_ptr (access : access) (ptr : Sptr_base.t)
        (f : [< T.sint ] Typed.t -> ('a, 'err, 'fix list) Block.SM.Result.t) :
        ('a, 'err, syn list) SM.Result.t =
      let open SM in
      let open SM.Syntax in
      let** () =
        assert_or_error Typed.(not (Typed.Ptr.is_null ptr.ptr)) `NullDereference
      in
      let loc, ofs = Typed.Ptr.decompose ptr.ptr in
      let* res =
        wrap loc
          (let open Freeable_block_with_meta in
           let open SM.Syntax in
           let* block = SM.get_state () in
           match (alloc_kind block, access) with
           | Some (Function _), (Read | Write) ->
               SM.Result.error `AccessedFnPointer
           | Some ((Const _ | AnonConst | StaticString) as k), Write ->
               let*^ cur_kind = DecayMap.SM.lift @@ get_alloc_kind () in
               if cur_kind <> k then SM.Result.error `WriteToReadOnly
               else Freeable_block_with_meta.wrap @@ Freeable_block.wrap (f ofs)
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

          type fix = syn list
        end)
  end

  type t = {
    heap : Heap.t option; [@sym_state.context { field = pointers }]
    functions : FunBiMap.t;
        [@sym_state.ignore
          {
            empty = FunBiMap.empty;
            is_empty = FunBiMap.is_empty;
            pp = FunBiMap.pp;
          }]
    globals : Sptr_base.t Rust_val.full_ptr GlobMap.t;
        [@sym_state.ignore
          {
            empty = GlobMap.empty;
            is_empty = GlobMap.is_empty;
            pp = GlobMap.pp (pp_full_ptr Sptr_base.pp);
          }]
    errors : Error.with_trace list;
        [@sym_state.ignore
          { empty = []; pp = Fmt.Dump.list Error.pp_with_trace }]
    pointers : DecayMap.t option;
    thread_destructor :
      (unit ->
      t option ->
      ((unit, Error.with_trace, unit) Compo_res.t * t option) Rustsymex.t)
      option;
        [@sym_state.ignore { empty = None }]
    const_generics : Sptr_base.t rust_val Types.ConstGenericVarId.Map.t;
        [@sym_state.ignore
          {
            empty = Types.ConstGenericVarId.Map.empty;
            pp = Types.ConstGenericVarId.Map.pp (pp_rust_val Sptr_base.pp);
          }]
  }
  [@@deriving sym_state { symex = Rustsymex }]

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
            | { node; _ } -> Freeable_block.pp' ~inner:Block.pp_pretty ft node)
          ft st

  open SM
  open SM.Syntax

  let log action ptr =
    let+ st = SM.get_state () in
    [%l.trace
      "About to execute action: %s (%a)@\n@[<2>STATE:@ %a@]" action Sptr_base.pp
        ptr
        (Fmt.Dump.option (pp_pretty ~ignore_freed:true))
        st]

  let[@inline] with_alloc_kind kind (f : unit -> 'a t) : 'a t =
   fun st -> with_alloc_kind kind (f () st)

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

  let apply_parser (type a) ?(ignore_borrow = false) ptr
      (parser : offset:T.sint Typed.t -> a Heap.Decoder.ParserMonad.t) :
      (a, Error.t, syn list) Result.t =
    let* () = log "load" ptr in
    let handler (ty, ofs) =
      let@ _ofs = Heap.with_ptr Read ptr in
      Block.with_block_read_tb (Tree_block.load ~ignore_borrow ofs ty ptr.tag)
    in
    let get_all (size, ofs) =
      let@ _ofs = Heap.with_ptr Read ptr in
      Block.with_block (Tree_block.get_init_leaves ofs size)
    in
    let offset = Typed.Ptr.ofs ptr.ptr in
    with_heap
    @@ Heap.Decoder.ParserMonad.parse ~handler ~get_all
    @@ parser ~offset

  let with_ptr access ptr f = with_heap @@ Heap.with_ptr access ptr f

  let uninit ((ptr, _) : Sptr_base.t * 'a) (ty : Types.ty) :
      (unit, 'err, 'fix) Result.t =
    let@ () = with_loc_err ~trace:"Uninitialising memory" () in
    let* () = log "uninit" ptr in
    let**^ size = Layout.size_of ty in
    let@ ofs = with_ptr Write ptr in
    Block.with_block @@ Tree_block.uninit_range ofs size

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
    [%l.debug
      "Checking pointer alignment of %a: expect %a for %a" Sptr_base.pp ptr
        Typed.ppa exp_align pp_ty ty];
    let loc, ofs = Typed.Ptr.decompose ptr.ptr in
    (* A pointer with no provenance is aligned to it's offset *)
    let align = Typed.(ite (Ptr.is_null_loc loc) exp_align (cast ptr.align)) in
    let is_aligned =
      ofs %@ exp_align ==@ Usize.(0s) &&@ (align %@ exp_align ==@ Usize.(0s))
    in
    if%sat Typed.not is_aligned then
      (* If we can't guarantee the pointer is aligned from the type's alignment,
         we try decaying it and seeing if its (symbolic) address **implies** its
         alignment. Note this is not OX sound; we only take the ok path if the
         pointer **must** be aligned, rather than also when it may be aligned.

         This avoids going through extra branches, as a pointer can pretty much
         always be aligned; what matters most is whether it is guaranteed to be
         aligned. *)
      let* address = with_pointers_sym @@ Sptr_base.decay ptr in
      if%sure address %@ exp_align ==@ Usize.(0s) then Result.ok ()
      else Result.error (`MisalignedPointer (exp_align, align, ofs))
    else Result.ok ()

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
      let@ ofs = with_ptr Ghost ptr in
      let+- _ =
        Block.with_block (Tree_block.check_owned ofs (Typed.cast size))
      in
      `UBDanglingPointer

  and check_non_dangling ((_, meta) as ptr) (ty : Types.ty) =
    let** size, _ = size_and_align_of_val ty meta in
    check_non_dangling_untyped ptr size

  and check_validity ~check_refs ty value =
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
    Encoder.check_validity ~check_ref ty value

  and load ?ignore_borrow ?(check_refs = true) ((ptr, meta) as fptr) ty :
      (Sptr_base.t rust_val, Error.t, syn list) Result.t =
    let** () = check_ptr_align fptr ty in
    let parser ~offset = Heap.Decoder.decode ~meta ~offset ty in
    let** value = apply_parser ?ignore_borrow ptr parser in
    [%l.debug "Finished reading rust value %a" (Rust_val.pp Sptr_base.pp) value];
    let++ () = check_validity ~check_refs ty value in
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
      [%l.debug
        "Checking validity of %a for %a" (pp_full_ptr Sptr_base.pp) fptr pp_ty
          ty];
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
          let@ ofs = with_ptr Ghost ptr in
          Block.with_block_read_tb (Tree_block.tb_access ofs size tag)

  (** Performs a load at the tree borrow level, by updating the borrow state,
      without attempting to validate the values or checking uninitialised memory
      accesses; all of these are ignored. *)
  let tb_load ((ptr : Sptr_base.t), _) ty =
    let@ () = with_loc_err ~trace:"Tree Borrow access" () in
    let**^ size = Layout.size_of ty in
    tb_load_untyped ptr size

  let store ((ptr, _) as fptr) ty sval :
      (unit, Error.with_trace, syn list) Result.t =
    let@ () = with_loc_err ~trace:"Memory store" () in
    let**^ parts = Encoder.encode ~offset:Usize.(0s) sval ty in
    if Iter.is_empty parts then Result.ok ()
    else
      let** () = check_ptr_align fptr ty in
      (* [%l.debug "Parsed to parts [%a]" (Iter.pp_seq ~sep:", " (Fmt.Dump.pair
         Encoder.pp_rust_val Typed.ppa)) parts]; *)
      let* () = log "store" ptr in
      let**^ size = Layout.size_of ty in
      let@ ofs = with_ptr Write ptr in
      Block.with_block_read_tb (fun tb ->
          let open Tree_block.SM in
          let open Tree_block.SM.Syntax in
          (* We uninitialise the whole range before writing, to ensure padding
             bytes are copied if there are any. *)
          let** () = Tree_block.uninit_range ofs size in
          Result.iter_iter parts ~f:(fun (value, offset) ->
              Tree_block.store (offset +!!@ ofs) value ptr.tag tb))

  (** We can't use {!Heap.Decoder} for [transmute], since the transmute happens
      regardless of the heap's state, so we need to re-instantiate it for tree
      block instead *)
  module Tree_block_decoder =
    Value_codec.Decoder
      (Sptr_base)
      (struct
        module SM = Tree_block.SM

        type fix = Tree_block.syn list
      end)

  let transmute ~from ~to_ v =
    (* a transmute is just a write of one type with a read of another type; we
       provide a function to do it that avoids allocating, checking alignment
       etc. *)
    [%l.debug
      "Transmuting %a: %a -> %a" (pp_rust_val Sptr_base.pp) v pp_ty from pp_ty
        to_];
    let@ () = with_loc_err ~trace:"Transmute" () in
    (* We pick [from] rather than [to_], because we can transmute to a smaller
       type, but not to a larger one, so it's guaranteed that [size(from) >=
       size(to_)] *)
    let**^ size = Layout.size_of from in
    let** value =
      with_pointers
        (let open DecayMap.SM in
         let open Syntax in
         let* block = Tree_block.alloc size in
         Tree_block.SM.Result.run_with_state ~state:(Some block)
           (let open Tree_block.SM in
            let open Syntax in
            let open Tree_block_decoder in
            (* first, we write *)
            let**^ parts =
              DecayMap.SM.lift @@ Encoder.encode ~offset:Usize.(0s) v from
            in
            let** () =
              Result.iter_iter parts ~f:(fun (value, offset) ->
                  Tree_block.store offset value None None)
            in
            (* next, we read *)
            let handler (ty, ofs) =
              Tree_block.load ~ignore_borrow:true ofs ty None None
            in
            let get_all (size, ofs) = Tree_block.get_init_leaves ofs size in
            ParserMonad.parse ~handler ~get_all
            @@ decode ~meta:Thin ~offset:Usize.(0s) to_)
         |> map (function
           | Ok (value, _block) -> Ok value
           | Error (e, _block) -> Error e
           (* HACK: we add this because we can't lift misses for a part of state
              that doesn't exist (and misses can't happen here anyways) *)
           | Missing _ -> failwith "impossible : miss"))
    in
    let++ () = check_validity ~check_refs:true to_ value in
    value

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
      (unit, Error.with_trace, syn list) Result.t =
    let@ () = with_loc_err ~trace:"Non-overlapping copy" () in
    let** tree_to_write =
      let@ ofs = with_ptr Read src in
      Block.with_block (fun tree_block ->
          let open DecayMap.SM.Syntax in
          let+ res, _ = Tree_block.get_raw_tree_owned ofs size tree_block in
          (res, tree_block))
    in
    let@ ofs = with_ptr Write dst in
    Block.with_block
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
               Tree.make ~node:t.node ~range:t.range
                 ~children:(aux tb l, aux tb r)
                 ()
           | Owned (Leaf (v, _)) ->
               let open Tree_block.MemVal in
               (* SACHA: Why isn't this updating the children as well? *)
               Tree.make
                 ~node:(TB.Owned (Leaf (v, tb)))
                 ~range:t.range ?children:t.children ()
         in
         try DecayMap.SM.Result.ok (aux tb t)
         with Failure msg -> DecayMap.SM.not_impl msg
       in
       (* Applies all the tree borrow ranges to the tree we're writing,
          overwriting all previous states. *)
       let**^ tree_to_write =
         DecayMap.SM.Result.fold_iter collect_tb_states ~init:tree_to_write
           ~f:(fun tree (tb, range) ->
             let open DecayMap.SM.Syntax in
             let replace_node = put_tb tb in
             let rebuild_parent = Tree.of_children in
             let++ _, tree =
               Tree.frame_range tree ~rebuild_parent ~replace_node range
             in
             tree)
       in
       Tree_block.put_raw_tree ofs tree_to_write)

  let alloc ?span ?zeroed size align =
    with_heap
      (let open Heap.SM in
       let open Heap.SM.Syntax in
       let*^ block, tag =
         Freeable_block_with_meta.make ?span ?zeroed ~align ~size ()
       in
       let** loc = Heap.alloc ~new_codom:block in
       let ptr = Typed.Ptr.mk loc Usize.(0s) in
       let ptr : Sptr_base.t = { ptr; tag; align; size } in
       (* The pointer is necessarily not null *)
       let+ () = assume [ Typed.(not (Ptr.is_null_loc loc)) ] in
       ok (ptr, Thin))

  let alloc_untyped ?span ~zeroed ~size ~align = alloc ?span ~zeroed size align

  let alloc_ty ?span ty =
    let@ () = with_loc_err ~trace:"Allocation" () in
    let**^ layout = Layout.layout_of ty in
    alloc ?span layout.size layout.align

  let alloc_tys ?span tys : ('a, Error.with_trace, syn list) Result.t =
    let@ () = with_loc_err ~trace:"Allocation" () in
    let**^ layouts = Rustsymex.Result.map_list tys ~f:Layout.layout_of in
    let layouts = List.rev layouts in
    with_heap
      (Heap.allocs ~els:layouts ~fn:(fun layout loc ->
           let open DecayMap.SM in
           let open DecayMap.SM.Syntax in
           (* make Tree_block *)
           let { size; align; _ } : Layout.t = layout in
           let* block, tag =
             Freeable_block_with_meta.make ?span ~align ~size ()
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

    (* FIXME: this currently causes errors in LinkedList? Unsure why, would
       required a length investigation... minimal repro:
       LinkedList::from([42]).pop_back() *)

    (* let** () =
     *   with_ptr ptr (fun _ ->
     *       Block.with_block_read_tb (fun tb ->
     *           [%l.warn "%a" Tree_borrow.pp tb];
     *           if Tree_borrow.strong_protector_exists tb then
     *             Tree_block.SM.Result.error `InvalidFreeStrongProtector
     *           else Tree_block.SM.Result.ok ()))
     * in *)
    [%l.debug "Freeing pointer %a" Sptr_base.pp ptr];
    with_heap
      (Heap.wrap loc (Freeable_block_with_meta.wrap (Freeable_block.free ())))

  let zeros (ptr, _) size =
    let@ () = with_loc_err ~trace:"Memory store (0s)" () in
    let* () = log "zeroes" ptr in
    let@ ofs = with_ptr Write ptr in
    Block.with_block (Tree_block.zero_range ofs size)

  let store_str_global str ptr =
    let@ globals = with_globals_sym in
    let globals = GlobMap.add (String str) ptr globals in
    Rustsymex.Result.ok ((), globals)

  let store_global g ptr =
    let@ globals = with_globals_sym in
    let globals = GlobMap.add (Global g) ptr globals in
    Rustsymex.Result.ok ((), globals)

  let load_str_global str =
    let@ globals = with_globals_sym in
    let ptr = GlobMap.find_opt (String str) globals in
    Rustsymex.Result.ok (ptr, globals)

  let load_global g =
    let@ globals = with_globals_sym in
    let ptr = GlobMap.find_opt (Global g) globals in
    Rustsymex.Result.ok (ptr, globals)

  let borrow ?protect (((ptr : Sptr_base.t), _) as fptr) (ty : Types.ty) =
    let@ () = with_loc_err ~trace:"Borrow" () in
    match ptr.tag with
    | None -> Result.ok fptr
    | Some tag ->
        let@ ofs = with_ptr Ghost ptr in
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
          let@ ofs = with_ptr Ghost ptr in
          [%l.debug "Unprotecting pointer %a" Sptr_base.pp ptr];
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

  let leak_check () : (unit, Error.with_trace, syn list) Result.t =
    with_heap
      (let open Heap.SM in
       let open Heap.SM.Syntax in
       let* heap = get_state () in
       let leaks =
         Heap.syntactic_bindings (Heap.of_opt heap)
         |> Seq.filter_map (fun (k, (v : Freeable_block_with_meta.t)) ->
             (* FIXME: This only works because our addresses are concrete *)
             let open DecayMap.SM in
             match (v.node, v.info) with
             | Alive _, Some { kind = Heap; trace; _ } -> Some (k, trace)
             | _ -> None)
       in
       if Seq.is_empty leaks then Result.ok ()
       else
         let pp_leak ft (k, trace) =
           Fmt.pf ft "%a (allocated at %a)" Typed.ppa k Trace.pp trace
         in
         [%l.info "Found leaks: %a" Fmt.(seq ~sep:(any ", ") pp_leak) leaks];
         let* leak_trace =
           Seq.map (fun (_, t) () -> return t) leaks |> List.of_seq |> branches
         in
         let leak_trace =
           Trace.rename ~rev:true 0 "Leaking function" leak_trace
         in
         Result.error (Error.decorate leak_trace `MemoryLeak))

  let add_error e =
    let@ errors = with_errors_sym in
    Rustsymex.Result.ok ((), e :: errors)

  let pop_error () =
    let** error =
      let@ errors = with_errors_sym in
      match errors with
      | e :: rest -> Rustsymex.Result.ok (e, rest)
      | _ -> failwith "pop_error with no errors?"
    in
    Result.error error

  let declare_fn fn_def =
    let align = Usize.(16s) in
    let** result =
      with_functions_sym (fun fns ->
          Rustsymex.Result.ok (FunBiMap.get_loc fn_def fns, fns))
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
          with_alloc_kind (Function fn_def) @@ fun () ->
          alloc_untyped ?span ~zeroed:false ~size:Usize.(0s) ~align
        in
        let ptr = { ptr with tag = None } in
        let loc = Typed.Ptr.loc ptr.ptr in
        with_functions_sym (fun fns ->
            Rustsymex.Result.ok ((ptr, meta), FunBiMap.add loc fn_def fns))

  let lookup_fn (({ ptr; _ } : Sptr_base.t), _) =
    let open Rustsymex in
    let open Syntax in
    let@ () = with_loc_err ~trace:"Accessing function pointer" () in
    let@ functions = with_functions_sym in
    let** () =
      assert_or_error (Typed.Ptr.ofs ptr ==@ Usize.(0s)) `MisalignedFnPointer
    in
    let loc = Typed.Ptr.loc ptr in
    match FunBiMap.get_fn loc functions with
    | Some fn -> Result.ok (fn, functions)
    | None -> Result.error `NotAFnPointer

  let lookup_const_generic id ty =
    let open Rustsymex in
    let open Syntax in
    let@ () = with_loc_err ~trace:"Accessing const generic" () in
    let@ const_generics = with_const_generics_sym in
    match Types.ConstGenericVarId.Map.find_opt id const_generics with
    | Some v -> Result.ok (v, const_generics)
    | None ->
        let++ v = Encoder.nondet_valid ty in
        (v, Types.ConstGenericVarId.Map.add id v const_generics)

  let register_thread_exit callback =
    (* HACK: we cannot expect thread exit callbacks to miss with syn, because
       when we define the callback type the syn type has not yet been defined.
       Instead we expect it to return unit; for now we fail, while we figure out
       a solution. *)
    let@ thread_destructor = with_thread_destructor_sym in
    let callback () =
      SM.Result.map_missing
        (fun _ -> failwith "TODO: Miss in thread exit")
        (callback ())
    in
    let destructor =
      match thread_destructor with
      | None -> callback
      | Some destructor -> fun () -> Result.bind callback (destructor ())
    in
    Rustsymex.Result.ok ((), Some destructor)

  let run_thread_exits () =
    let* st_opt = SM.get_state () in
    let st = of_opt st_opt in
    match st.thread_destructor with
    | None -> Result.ok ()
    | Some destructor -> SM.Result.map_missing (fun () -> []) (destructor ())
end
