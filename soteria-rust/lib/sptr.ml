open Rustsymex
open Rustsymex.Syntax
open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open T

module type S = sig
  (** pointer type *)
  type t

  (** decay state type *)
  type decay_st

  val pp_decay_st : decay_st Fmt.t
  val empty_decay_st : decay_st
  val pp : t Fmt.t
  val null_ptr : unit -> t
  val null_ptr_of : sint Typed.t -> t

  (** Pointer equality *)
  val sem_eq : t -> t -> sbool Typed.t

  (** If this pointer is at a null location, i.e. has no provenance *)
  val is_at_null_loc : t -> sbool Typed.t

  (** If these two pointers are at the same location (ie. same allocation) *)
  val is_same_loc : t -> t -> sbool Typed.t

  (** The distance, in bytes, between two pointers; if they point to different
      allocations, they are decayed and substracted. *)
  val distance :
    t -> t -> decay_st -> ([> sint ] Typed.t * decay_st) Rustsymex.t

  (** The symbolic constraints needed for the pointer to be valid. *)
  val constraints : t -> sbool Typed.t

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
    (t, [> `UBDanglingPointer ], 'a) Result.t

  (** Project a pointer to a field of the given type. *)
  val project :
    Types.ty ->
    Expressions.field_proj_kind ->
    Types.field_id ->
    t ->
    (t, [> `UBDanglingPointer ], 'a) Result.t

  (** Decay a pointer into an integer value, losing provenance. *)
  val decay : t -> decay_st -> ([> sint ] Typed.t * decay_st) Rustsymex.t

  (** Decay a pointer into an integer value, using an effect handler;
      {b this could be unsound} but should be ok in most cases. *)
  val decay_eft : t -> [> sint ] Typed.t Rustsymex.t

  (** To be used before [decay_eft] to wrap a function in a handler for the
      decay state. {b This could be unsound}, and should be avoided. *)
  val with_decay :
    decay_st -> (unit -> 'a Rustsymex.t) -> ('a * decay_st) Rustsymex.t

  (** For Miri: the allocation ID of this location, as a u64 *)
  val as_id : t -> [> sint ] Typed.t

  (** Get the allocation info for this pointer: its size and alignment *)
  val allocation_info : t -> [> sint ] Typed.t * [> nonzero ] Typed.t

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
  module LocMap = Map.MakePp (struct
    type t = T.sloc Typed.t

    let compare = Typed.compare
    let pp = Typed.ppa
  end)

  type t = arithptr_t = {
    ptr : T.sptr Typed.t;
    tag : Tree_borrow.tag;
    align : T.nonzero Typed.t;
    size : T.sint Typed.t;
  }

  type decay_st = T.sint Typed.t LocMap.t

  let empty_decay_st = LocMap.empty
  let pp_decay_st = LocMap.pp Typed.ppa

  let pp fmt { ptr; tag; _ } =
    Fmt.pf fmt "%a[%a]" Typed.ppa ptr Tree_borrow.pp_tag tag

  let null_ptr () =
    {
      ptr = Typed.Ptr.null ();
      tag = Tree_borrow.zero;
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
    let* size = Layout.size_of_s ty in
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
    let layout = Layout.layout_of ty in
    let fields =
      match kind with
      | Expressions.ProjAdt (_, Some variant) ->
          Layout.Fields_shape.shape_for_variant variant layout.fields
      | ProjAdt (_, None) | ProjTuple _ -> layout.fields
    in
    let off = Layout.Fields_shape.offset_of field fields in
    offset ~signed:false ptr (Typed.BitVec.usizei off)

  let decay { ptr; align; size; _ } decay_st =
    let open Rustsymex in
    let open Rustsymex.Syntax in
    (* FIXME: if we want to be less unsound, we would also need to assert that this pointer's
       base is distinct from all other decayed pointers' bases... *)
    let loc, ofs = Typed.Ptr.decompose ptr in
    match LocMap.find_opt loc decay_st with
    | Some loc_int -> return (loc_int +!@ ofs, decay_st)
    | None ->
        let+ loc_int =
          if%sat Typed.Ptr.is_null_loc loc then return Usize.(0s)
          else
            let* loc_int = nondet (Typed.t_usize ()) in
            let isize_max = Layout.max_value_z (TInt Isize) in
            let constrs =
              [
                (loc_int %@ align ==@ Usize.(0s));
                Usize.(0s) <@ loc_int;
                loc_int <@ Typed.BitVec.usize isize_max -!@ size;
              ]
            in
            let+ () = Rustsymex.assume constrs in
            loc_int
        in
        L.debug (fun m -> m "Decayed %a to %a" Typed.ppa loc Typed.ppa loc_int);
        let decay_st = LocMap.add loc loc_int decay_st in
        (loc_int +!@ ofs, decay_st)

  type _ Effect.t += Decay : t -> [> T.sint ] Typed.t Rustsymex.t Effect.t

  let with_decay decay_st f =
    let decay_st = ref decay_st in
    try
      let+ res = f () in
      (res, !decay_st)
    with effect Decay ptr, k ->
      let* decayed, decay_st' = decay ptr !decay_st in
      decay_st := decay_st';
      Effect.Deep.continue k (return decayed)

  let decay_eft ptr = Effect.perform (Decay ptr)

  let distance ({ ptr = ptr1; _ } as p1) ({ ptr = ptr2; _ } as p2) decay_st =
    if%sat Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2 then
      return (Typed.Ptr.ofs ptr1 -!@ Typed.Ptr.ofs ptr2, decay_st)
    else
      let* ptr1, decay_st = decay p1 decay_st in
      let+ ptr2, decay_st = decay p2 decay_st in
      (ptr1 -!@ ptr2, decay_st)

  let as_id { ptr; _ } = Typed.cast @@ Typed.Ptr.loc ptr
  let allocation_info { size; align; _ } = (Typed.cast size, Typed.cast align)

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
