open Charon
open Typed
open T
open Typed.Infix

module type S = sig
  (** pointer type *)
  type t

  val pp : t Fmt.t
  val null_ptr : t
  val is_null : t -> sbool Typed.t

  (** [offset ~ty ptr off] Offsets [ptr] by the size of [ty] * [off]. [ty]
      defaults to u8. *)
  val offset : ?ty:Charon.Types.ty -> t -> sint Typed.t -> t

  (** The metadata of the pointer, if it's a fat pointer *)
  val meta : t -> T.cval Typed.t option

  (** Sets the metadata of the pointer *)
  val with_meta : t -> T.cval Typed.t option -> t
end

module T : S with type t = T.sptr Typed.t * T.cval Typed.t option = struct
  type t = T.sptr Typed.t * T.cval Typed.t option

  let pp fmt (ptr, meta) =
    Format.fprintf fmt "(%a, %a)" Typed.ppa ptr
      Fmt.(option ~none:(Fmt.any "-") Typed.ppa)
      meta

  let null_ptr = (Typed.Ptr.null, None)
  let is_null (ptr, _) = Typed.Ptr.is_null ptr

  let offset ?(ty = Types.TLiteral (TInteger U8)) (ptr, meta) off =
    let { size; _ } : Layout.layout = Layout.layout_of ty in
    let ptr' = Typed.Ptr.add_ofs ptr (Typed.int size *@ off) in
    (ptr', meta)

  let meta = snd
  let with_meta (ptr, _) meta = (ptr, meta)
end
