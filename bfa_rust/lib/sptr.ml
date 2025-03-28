open Charon
open Typed
open T
open Typed.Infix

module type S = sig
  (** pointer type *)
  type t

  val pp : t Fmt.t
  val null_ptr : t

  (** pointer equality, irrespective of metadata *)
  val eq : t -> t -> sbool Typed.t
  val is_null : t -> sbool Typed.t
  val is_same_loc : t -> t -> sbool Typed.t
  val is_before : t -> t -> sbool Typed.t
  val distance : t -> t -> sint Typed.t
  val constraints : t -> sbool Typed.t

  (** [offset ~ty ptr off] Offsets [ptr] by the size of [ty] * [off]. [ty]
      defaults to u8. *)
  val offset : ?ty:Charon.Types.ty -> t -> sint Typed.t -> t

  (** The metadata of the pointer, if it's a fat pointer *)
  val meta : t -> T.cval Typed.t option

  (** Sets the metadata of the pointer *)
  val with_meta : ?meta:(T.cval Typed.t) -> t -> t
end

module T : S with type t = T.sptr Typed.t * T.cval Typed.t option = struct
  type t = T.sptr Typed.t * T.cval Typed.t option

  let pp fmt (ptr, meta) =
    Format.fprintf fmt "(%a, %a)" Typed.ppa ptr
      Fmt.(option ~none:(Fmt.any "-") Typed.ppa)
      meta

  let null_ptr = (Typed.Ptr.null, None)
  let eq (ptr1, _) (ptr2,_) = (ptr1 ==@ ptr2)

  let is_null (ptr, _) = Typed.Ptr.is_null ptr

  let is_same_loc (ptr1, _) (ptr2, _) =
    Typed.Ptr.loc ptr1 ==@ Typed.Ptr.loc ptr2

  let is_before (ptr1, _) (ptr2, _) = Typed.Ptr.ofs ptr1 <@ Typed.Ptr.ofs ptr2

  let distance (ptr1, _) (ptr2, _) = Typed.Ptr.ofs ptr1 -@ Typed.Ptr.ofs ptr2

  let constraints =
    let offset_constrs = Layout.int_constraints Values.Isize in
    fun (ptr, _) ->
      let ofs = Typed.Ptr.ofs ptr in
      Typed.conj (offset_constrs ofs)

  let offset ?(ty = Types.TLiteral (TInteger U8)) (ptr, meta) off =
    let { size; _ } : Layout.layout = Layout.layout_of ty in
    let ptr' = Typed.Ptr.add_ofs ptr (Typed.int size *@ off) in
    (ptr', meta)

  let meta = snd
  let with_meta ?meta (ptr, _) = (ptr, meta)
end
