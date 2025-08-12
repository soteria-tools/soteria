open Rustsymex
open Rustsymex.Syntax
open Charon
open Typed
open T
open Typed.Infix
open Typed.Syntax

module type S = sig
  (** pointer type *)
  type t

  val pp : t Fmt.t
  val null_ptr : t
  val null_ptr_of : sint Typed.t -> t

  (** Pointer equality *)
  val sem_eq : t -> t -> sbool Typed.t

  (** If this pointer is at a null location, i.e. has no provenance *)
  val is_at_null_loc : t -> sbool Typed.t

  (** If these two pointers are at the same location (ie. same allocation) *)
  val is_same_loc : t -> t -> sbool Typed.t

  (** The distance, in bytes, between two pointers; if they point to different
      allocations, they are decayed and substracted. *)
  val distance : t -> t -> sint Typed.t Rustsymex.t

  (** The symbolic constraints needed for the pointer to be valid. *)
  val constraints : t -> sbool Typed.t

  (** [offset ?check ?ty ptr off] Offsets [ptr] by the size of [ty] * [off].
      [ty] defaults to u8. May result in a dangling pointer error if the pointer
      goes over the allocation limit. This check can be disabled with
      [~check:false]. *)
  val offset :
    ?check:bool ->
    ?ty:Charon.Types.ty ->
    t ->
    sint Typed.t ->
    (t, [> `UBDanglingPointer ], 'a) Result.t

  (** Project a pointer to a field of the given type. *)
  val project :
    Types.ty ->
    Expressions.field_proj_kind ->
    Types.field_id ->
    t ->
    (t, [> `UBDanglingPointer ], 'a) Result.t

  (** Decay a pointer into an integer value, losing provenance. *)
  val decay : t -> sint Typed.t Rustsymex.t

  (** For Miri: the allocation ID of this location, as a u64 *)
  val as_id : t -> sint Typed.t

  (** Get the allocation info for this pointer: its size and alignment *)
  val allocation_info : t -> T.sint Typed.t * T.nonzero Typed.t

  val iter_vars : t -> (Svalue.Var.t * 'b ty -> unit) -> unit
  val subst : (Svalue.Var.t -> Svalue.Var.t) -> t -> t
end

type arithptr_t = {
  ptr : T.sptr Typed.t;
  tag : Tree_borrow.tag;
  align : T.nonzero Typed.t;
  size : T.sint Typed.t;
}

(** A pointer that can perform pointer arithmetics -- all pointers are a pair of
    location and offset, along with an optional metadata. *)
module ArithPtr : S with type t = arithptr_t = struct
  type t = arithptr_t = {
    ptr : T.sptr Typed.t;
    tag : Tree_borrow.tag;
    align : T.nonzero Typed.t;
    size : T.sint Typed.t;
  }

  let pp fmt { ptr; tag; _ } =
    Fmt.pf fmt "%a[%a]" Typed.ppa ptr Tree_borrow.pp_tag tag

  let null_ptr =
    {
      ptr = Typed.Ptr.null;
      tag = Tree_borrow.zero;
      align = Typed.cast 1s;
      size = 0s;
    }

  let null_ptr_of ofs =
    let ptr = Typed.Ptr.add_ofs Typed.Ptr.null ofs in
    { null_ptr with ptr }

  let sem_eq { ptr = ptr1; _ } { ptr = ptr2; _ } = ptr1 ==@ ptr2
  let is_at_null_loc { ptr; _ } = Typed.Ptr.is_at_null_loc ptr

  let is_same_loc { ptr = ptr1; _ } { ptr = ptr2; _ } =
    Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2

  let constraints { ptr; size; _ } =
    let offset_constrs = Layout.int_constraints (TInt Isize) in
    let ofs = Typed.Ptr.ofs ptr in
    Typed.conj ((ofs <=@ size) :: offset_constrs ofs)

  let offset ?(check = true) ?(ty = Types.TLiteral (TUInt U8))
      ({ ptr; _ } as fptr) off =
    let* size = Layout.size_of_s ty in
    let off = size *@ off in
    let ptr = Typed.Ptr.add_ofs ptr off in
    let ptr = { fptr with ptr } in
    if check then
      if%sat [@lname "Ptr ok"] [@rname "Ptr dangling"]
        off ==@ 0s ||@ constraints ptr
      then Result.ok ptr
      else Result.error `UBDanglingPointer
    else Result.ok ptr

  let project ty kind field ptr =
    let field = Types.FieldId.to_int field in
    let layout, field =
      match kind with
      | Expressions.ProjAdt (adt_id, Some variant) ->
          (* Skip discriminator, so field + 1 *)
          (Layout.of_enum_variant adt_id variant, field + 1)
      | ProjAdt (_, None) | ProjTuple _ -> (Layout.layout_of ty, field)
    in
    let off = Layout.Fields_shape.offset_of field layout.fields in
    offset ptr (Typed.int off)

  module ValMap = Map.Make (struct
    type t = T.sloc Typed.t

    let compare = Typed.compare
  end)

  (* Create a map with the null-ptr preset to 0 *)
  let decayed_vars = ref (ValMap.add Typed.Ptr.null_loc 0s ValMap.empty)

  let decay { ptr; align; size; _ } =
    let open Rustsymex in
    let open Rustsymex.Syntax in
    let open Typed.Syntax in
    (* FIXME: if we want to be less unsound, we would also need to assert that this pointer's
       base is distinct from all other decayed pointers' bases... *)
    let loc, ofs = Typed.Ptr.decompose ptr in
    match ValMap.find_opt loc !decayed_vars with
    | Some loc_int -> return (loc_int +@ ofs)
    | None ->
        let+ loc_int =
          nondet Typed.t_int ~constrs:(fun x ->
              let isize_max = Layout.max_value (TInt Isize) in
              [ x %@ align ==@ 0s; 0s <@ x; x +@ size <=@ isize_max ])
        in
        decayed_vars := ValMap.add loc loc_int !decayed_vars;
        loc_int +@ ofs

  let distance ({ ptr = ptr1; _ } as p1) ({ ptr = ptr2; _ } as p2) =
    if%sat Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2 then
      return (Typed.Ptr.ofs ptr1 -@ Typed.Ptr.ofs ptr2)
    else
      let* ptr1 = decay p1 in
      let+ ptr2 = decay p2 in
      ptr1 -@ ptr2

  let as_id { ptr; _ } = Typed.cast @@ Typed.Ptr.loc ptr
  let allocation_info { size; align; _ } = (size, align)

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
