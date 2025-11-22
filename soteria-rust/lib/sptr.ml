open Rustsymex
open Rustsymex.Syntax
open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open T

module type DecayMapS = sig
  type t

  val pp : Format.formatter -> t -> unit

  val decay :
    size:[< sint ] Typed.t ->
    align:[< nonzero ] Typed.t ->
    [< T.sloc ] Typed.t ->
    t option ->
    (T.sint Typed.t * t option) Rustsymex.t
end

module DecayMap : DecayMapS = struct
  module StateKey = struct
    module Symex = Rustsymex
    include Typed

    type t = T.sloc Typed.t

    let pp = ppa
    let simplify = Rustsymex.simplify
    let fresh () = failwith "Allocation is not valid for the decay map!"
  end

  module SPmap = Soteria.Sym_states.Pmap.Make (Rustsymex) (StateKey)

  type t = T.sint Typed.t SPmap.t

  let pp = SPmap.pp Typed.ppa

  let decay ~size ~align (loc : [< T.sloc ] Typed.t) st =
    if%sat Typed.Ptr.is_null_loc loc then return (Usize.(0s), st)
    else
      let+ res =
        SPmap.wrap
          (function
            | Some decayed -> Result.ok (decayed, Some decayed)
            | None ->
                let* loc_int = nondet (Typed.t_usize ()) in
                let isize_max = Layout.max_value_z (TInt Isize) in
                let* () =
                  Rustsymex.assume
                    [
                      (loc_int %@ align ==@ Usize.(0s));
                      Usize.(0s) <@ loc_int;
                      loc_int <@ Typed.BitVec.usize isize_max -!@ size;
                    ]
                in
                Result.ok (loc_int, Some loc_int))
          (loc :> T.sloc Typed.t)
          st
      in
      Soteria.Symex.Compo_res.get_ok res
end

module DecayMapMonad = struct
  include
    Soteria.Sym_states.State_monad.Make
      (Rustsymex)
      (struct
        type t = DecayMap.t option
      end)

  let not_impl msg = lift @@ not_impl msg
  let of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x
  let match_on xs ~constr = lift @@ match_on xs ~constr
end

module type S = sig
  (** pointer type *)
  type t

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
  val distance : t -> t -> sint Typed.t DecayMapMonad.t

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
  val decay : t -> [> sint ] Typed.t DecayMapMonad.t

  (** For Miri: the allocation ID of this location, as a u64 *)
  val as_id : t -> [> sint ] Typed.t

  (** Returns a symbolic boolean to check whether this pointer has the given
      alignment *)
  val is_aligned : [< T.nonzero ] Typed.t -> t -> [> sbool ] Typed.t

  (** Get the allocation info for this pointer: its size and alignment *)
  val allocation_info : t -> [> sint ] Typed.t * [> nonzero ] Typed.t

  val iter_vars : t -> (Svalue.Var.t * 'b ty -> unit) -> unit
  val subst : (Svalue.Var.t -> Svalue.Var.t) -> t -> t
end

type arithptr_t = {
  ptr : T.sptr Typed.t;
  tag : Tree_borrow.tag option;
  align : T.nonzero Typed.t;
  size : T.sint Typed.t;
}

(** A pointer that can perform pointer arithmetics -- all pointers are a pair of
    location and offset, along with an optional metadata. *)
module ArithPtr : S with type t = arithptr_t = struct
  type t = arithptr_t = {
    ptr : T.sptr Typed.t;
    tag : Tree_borrow.tag option;
    align : T.nonzero Typed.t;
    size : T.sint Typed.t;
  }

  let pp fmt { ptr; tag; _ } =
    Fmt.pf fmt "%a[%a]" Typed.ppa ptr
      Fmt.(option ~none:(any "*") Tree_borrow.pp_tag)
      tag

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

  let decay { ptr; align; size; _ } decay_map =
    let loc, ofs = Typed.Ptr.decompose ptr in
    let+ loc_int, decay_map = DecayMap.decay ~size ~align loc decay_map in
    (loc_int +!!@ ofs, decay_map)

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
    ofs %@ exp_align ==@ Usize.(0s) &&@ (align %@ exp_align ==@ Usize.(0s))

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
