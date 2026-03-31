open Rustsymex
open Rustsymex.Syntax
open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open T

(** A map to store information on "decayed" pointers, namely mapping from
    locations to integers (their decayed) value.

    It exposes two functions: [decay], to convert a location to an integer
    (given the size and alignment of its allocation), and [from_exposed] which
    does the reverse operation. Importantly, while [decay] is total,
    [from_exposed] is partial: if a provenance cannot be guessed from the
    integer, [None] is returned. *)
module type DecayMapS = sig
  type t

  val empty : t
  val pp : Format.formatter -> t -> unit

  (** Decays the given location into an integer, updating the decay map
      accordingly. If [expose] is true, the provenance is marked as exposed, and
      can be retrieved later with [from_exposed]. Returns the decayed integer,
      along with the updated decay map. *)
  val decay :
    expose:bool ->
    size:[< sint ] Typed.t ->
    align:[< nonzero ] Typed.t ->
    [< sloc ] Typed.t ->
    t ->
    (sint Typed.t * t) Rustsymex.t

  (** Tries finding, for the given integer, the matching provenance in the decay
      map. If found, it returns that provenance, along with the exposed address
      for that allocation at offset 0. Otherwise returns [None]. *)
  val from_exposed :
    [< sint ] Typed.t ->
    t ->
    ((sloc Typed.t * sint Typed.t) option * t) Rustsymex.t
end

module DecayMap : DecayMapS = struct
  module MapKey = struct
    include Typed

    type t = sloc Typed.t
    type syn = Expr.t [@@deriving show { with_path = false }]

    let to_int = unique_tag
    let pp = ppa
    let show = Fmt.to_to_string pp
    let simplify = Rustsymex.simplify
  end

  type entry = { address : sint Typed.t; exposed : bool }
  [@@deriving show { with_path = false }]

  module Map =
    Soteria.Data.S_map.Direct_access_patricia_tree (Rustsymex) (MapKey)

  type t = entry Map.t

  let pp = Map.pp pp_entry
  let empty = Map.empty

  let decay ~expose ~size ~align (loc : [< sloc ] Typed.t) (map : t) :
      (T.sint Typed.t * t) Rustsymex.t =
    if%sat Typed.Ptr.is_null_loc loc then return (Usize.(0s), map)
    else
      let* key, entry = Map.find_opt (cast loc) map in
      match entry with
      | Some { address; exposed } when Stdlib.not exposed && expose ->
          let map = Map.syntactic_add key { address; exposed = true } map in
          return (address, map)
      | Some { address; exposed = _ } -> return (address, map)
      | None ->
          let* address = nondet (Typed.t_usize ()) in
          let isize_max = Layout.max_value_z (TInt Isize) in
          let+ () =
            assume
              [
                (address %@ align ==@ Usize.(0s));
                align <=@ address;
                address <@ Typed.BitVec.usize isize_max -!@ size;
              ]
          in
          let map = Map.syntactic_add key { address; exposed = expose } map in
          (address, map)

  let from_exposed (loc_int : [< sint ] Typed.t) (map : t) =
    (* UX: we only consider the first one; this is more or less correct, as per
       the documentation of [with_exposed_provenance]: "The provenance of the
       returned pointer is that of some pointer that was previously exposed"

       See
       https://doc.rust-lang.org/nightly/std/ptr/fn.with_exposed_provenance.html *)
    let usize_ty = Typed.t_usize () in
    let usize_ty_syn = Typed.untype_type usize_ty in
    let bindings = Map.syntactic_bindings map in
    let binding =
      Svalue.iter_vars (Expr.of_value loc_int)
      |> Iter.filter (fun (_, ty) -> Svalue.equal_ty usize_ty_syn ty)
      |> Iter.filter_map (fun (var, _) ->
          let v = Typed.mk_var var usize_ty in
          Seq.find
            (fun (_, { address; exposed }) -> exposed && Typed.equal v address)
            bindings)
      |> Iter.map (fun (loc, { address; _ }) -> (loc, address))
      |> Iter.to_opt
    in
    return (binding, map)
end

module DecayMapMonad = struct
  include
    Soteria.Sym_states.State_monad.Make
      (Rustsymex)
      (struct
        type t = DecayMap.t
      end)

  let not_impl msg = lift @@ not_impl msg
  let of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x
  let match_on xs ~constr = lift @@ match_on xs ~constr
  let get_where () = lift @@ get_trace ()
end

module D_abstr = Soteria.Data.Abstr.M (DecayMapMonad)

module type S = sig
  (** pointer type *)
  include D_abstr.S_with_syn

  include D_abstr.Sem_eq with type t := t

  val null_ptr : unit -> t
  val null_ptr_of : [< sint ] Typed.t -> t

  (** Pointer equality. This comparison is structural, i.e. it is aware of
      provenance.

      In other words, given [addr = decay ptr] and [ptr' = of_exposed addr],
      [sem_eq ptr ptr'] will be false, even though they would have the same
      address, as [ptr] has provenance and [ptr'] does not. *)
  val sem_eq : t -> t -> sbool Typed.t

  (** If this pointer is at a null location, i.e. has no provenance *)
  val is_at_null_loc : t -> sbool Typed.t

  (** If these two pointers are at the same location (ie. same allocation) *)
  val is_same_loc : t -> t -> sbool Typed.t

  (** The distance, in bytes, between two pointers; if they point to different
      allocations, they are decayed and substracted. *)
  val distance : t -> t -> sint Typed.t DecayMapMonad.t

  (** The symbolic constraints needed for the pointer to be valid. *)
  val constraints : t -> sbool Typed.t

  (** Generates a nondeterministic pointer, that is valid for accesses to the
      given type. The location of the pointer is nondeterministic; it could
      point to any allocation. *)
  val nondet : Types.ty -> (t, [> Error.t ], 'a) Result.t

  (** [offset ?check ?ty ~signed ptr off] Offsets [ptr] by the size of [ty] *
      [off], interpreting [off] as a [signed] integer. [ty] defaults to u8. May
      result in a dangling pointer error if the pointer goes over the allocation
      limit. This check can be disabled with [~check:false]. *)
  val offset :
    ?check:bool ->
    ?ty:Charon.Types.ty ->
    signed:bool ->
    t ->
    [< sint ] Typed.t ->
    (t, [> Error.t ], 'a) Result.t

  (** Project a pointer to a field of the given type. *)
  val project :
    Types.ty ->
    Expressions.field_proj_kind ->
    Types.field_id ->
    t ->
    (t, [> Error.t ], 'a) Result.t

  (** Decay a pointer into an integer value, losing provenance.
      {b This does not expose the address of the allocation; for that, use
         [expose]} *)
  val decay : t -> [> sint ] Typed.t DecayMapMonad.t

  (** Decay a pointer into an integer value, exposing the address of the
      allocation, allowing it to be retrieved with [DecayMapS.from_exposed]
      later. *)
  val expose : t -> [> sint ] Typed.t DecayMapMonad.t

  (** For Miri: the allocation ID of this location, as a u64 *)
  val as_id : t -> [> sint ] Typed.t

  (** Returns a symbolic boolean to check whether this pointer has the given
      alignment, along with the error to raise otherwise. *)
  val is_aligned : nonzero Typed.t -> t -> [> sbool ] Typed.t * Error.t

  (** Get the allocation info for this pointer: its size and alignment *)
  val allocation_info : t -> [> sint ] Typed.t * [> nonzero ] Typed.t
end

type ('sptr, 'snonzero, 'sint) arithptr = {
  ptr : 'sptr;
  tag : Tree_borrow.tag option;
  align : 'snonzero;
  size : 'sint;
}

(** A pointer that can perform pointer arithmetics -- all pointers are a pair of
    location and offset, along with an optional metadata. *)
module ArithPtr :
  S
    with type t = (T.sptr Typed.t, T.nonzero Typed.t, T.sint Typed.t) arithptr
     and type syn = (Expr.t, Expr.t, Expr.t) arithptr = struct
  type t = (T.sptr Typed.t, T.nonzero Typed.t, T.sint Typed.t) arithptr
  type syn = (Expr.t, Expr.t, Expr.t) arithptr

  let pp' pp_v fmt { ptr; tag; _ } =
    Fmt.pf fmt "%a[%a]" pp_v ptr
      Fmt.(option ~none:(any "*") Tree_borrow.pp_tag)
      tag

  let pp : t Fmt.t = pp' Typed.ppa
  let show = Fmt.to_to_string pp
  let pp_syn : syn Fmt.t = pp' Expr.pp
  let show_syn = Fmt.to_to_string pp_syn

  let to_syn { ptr; tag; align; size } =
    {
      ptr = Expr.of_value ptr;
      align = Expr.of_value align;
      size = Expr.of_value size;
      tag;
    }

  let learn_eq syn t =
    let open DecayMapMonad.Consumer in
    let open Syntax in
    let* () = if syn.tag = t.tag then ok () else lfail Typed.v_false in
    let* () = learn_eq syn.ptr t.ptr in
    let* () = learn_eq syn.align t.align in
    learn_eq syn.size t.size

  let exprs_syn { ptr; align; size; _ } = [ ptr; align; size ]
  let fresh () = failwith "Fresh unimplemented for sptr (for now)"

  let null_ptr () =
    {
      ptr = Typed.Ptr.null ();
      tag = None;
      align = Usize.(1s);
      size = Usize.(0s);
    }

  let null_ptr_of ofs =
    let null_ptr = null_ptr () in
    let ptr = Typed.Ptr.add_ofs null_ptr.ptr ofs in
    { null_ptr with ptr }

  let sem_eq { ptr = ptr1; _ } { ptr = ptr2; _ } = ptr1 ==@ ptr2
  let is_at_null_loc { ptr; _ } = Typed.Ptr.is_at_null_loc ptr

  let is_same_loc { ptr = ptr1; _ } { ptr = ptr2; _ } =
    Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2

  let constraints { ptr; size; _ } =
    let ofs = Typed.Ptr.ofs ptr in
    Typed.conj [ Usize.(0s) <=$@ ofs; ofs <=$@ size ]

  let offset ?(check = true) ?(ty = Types.TLiteral (TUInt U8)) ~signed
      ({ ptr; _ } as fptr) off_by =
    let** size = Layout.size_of ty in
    let loc, off = Typed.Ptr.decompose ptr in
    let ( *? ), ( +? ) =
      if signed then (( *$?@ ), ( +$?@ )) else (( *?@ ), ( +?@ ))
    in
    let off_by, off_by_ovf = size *? off_by in
    let off, off_ovf = off +? off_by in
    let ptr = Typed.Ptr.mk loc off in
    let ptr = { fptr with ptr } in
    if check then
      let++ () =
        assert_or_error
          (off_by
          ==@ Usize.(0s)
          ||@ ((not off_by_ovf) &&@ not off_ovf &&@ constraints ptr))
          `UBDanglingPointer
      in
      ptr
    else Result.ok ptr

  let project ty kind field ptr =
    let field = Types.FieldId.to_int field in
    let** layout = Layout.layout_of ty in
    let fields =
      match kind with
      | Expressions.ProjAdt (_, Some variant) ->
          Layout.Fields_shape.shape_for_variant variant layout.fields
      | ProjAdt (_, None) | ProjTuple _ -> layout.fields
    in
    let off = Layout.Fields_shape.offset_of field fields in
    offset ~signed:false ptr off

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
    if%sat Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2 then
      DecayMapMonad.return (Typed.Ptr.ofs ptr1 -!@ Typed.Ptr.ofs ptr2)
    else
      let* ptr1 = decay p1 in
      let+ ptr2 = decay p2 in
      ptr1 -!@ ptr2

  let as_id { ptr; _ } = Typed.cast @@ Typed.Ptr.loc ptr
  let allocation_info { size; align; _ } = (Typed.cast size, Typed.cast align)

  let is_aligned exp_align { ptr; align; _ } =
    let loc, ofs = Typed.Ptr.decompose ptr in
    (* A pointer with no provenance is alignd to it's offset *)
    let align =
      Typed.ite (Typed.Ptr.is_null_loc loc) exp_align (Typed.cast align)
    in
    let is_aligned =
      ofs %@ exp_align ==@ Usize.(0s) &&@ (align %@ exp_align ==@ Usize.(0s))
    in
    (is_aligned, `MisalignedPointer (exp_align, align, ofs))

  let nondet ty =
    let** layout = Layout.layout_of ty in
    let* loc = nondet (Typed.t_loc ()) in
    let* ofs = nondet (Typed.t_usize ()) in
    let ptr = Typed.Ptr.mk loc ofs in
    let ptr = { ptr; tag = None; align = layout.align; size = layout.size } in
    let aligned, _ = is_aligned layout.align ptr in
    let* () = assume [ aligned ] in
    Result.ok ptr

  let subst subst_val p =
    let se = Expr.subst subst_val in
    let ptr = se p.ptr in
    let align = se p.align in
    let size = se p.size in
    { p with ptr; align; size }

  (* let subst subst_var p = let ptr = Typed.subst subst_var p.ptr in let align
     = Typed.subst subst_var p.align in let size = Typed.subst subst_var p.size
     in { p with ptr; align; size } *)
end
