open Hc
include Svalue

module T = struct
  type sint = [ `NonZero | `Zero ]
  type nonzero = [ `NonZero ]
  type sbool = [ `Bool ]
  type any = [ `Bool | `NonZero | `Zero ]

  let pp_sint _ _ = ()
  let pp_nonzero _ _ = ()
  let pp_sbool _ _ = ()
  let pp_any _ _ = ()
end

type nonrec +'a t = t
type nonrec +'a ty = ty
type sbool = T.sbool

let[@inline] get_ty x = x.node.ty
let[@inline] untype_type x = x
let ppa = pp
let pp _ = pp
let ppa_ty = pp_ty
let pp_ty _ = pp_ty
let[@inline] cast x = x
let[@inline] untyped x = x
let[@inline] untyped_list l = l
let[@inline] type_ x = x
let[@inline] type_type x = x
let type_checked x ty = if equal_ty x.node.ty ty then Some x else None
let cast_checked = type_checked

let cast_checked2 x y =
  if equal_ty x.node.ty y.node.ty then Some (x, y, x.node.ty) else None

(* A phantom alias to the typed value, so {!Expr} can existentially wrap it (the
   index is purely phantom: [_ value = Svalue.t]). *)
type +'a value = 'a t

module Raw_expr = Expr

module Expr = struct
  type t = Svalue.t

  let pp = Raw_expr.pp
  let show = Raw_expr.show

  type packed_v = Packed : 'a value -> packed_v
  type packed_ty = PackedTy : 'a ty -> packed_ty

  let[@inline] of_value (v : 'a value) : t = untyped v
  let ty (s : t) : packed_ty = PackedTy (type_type s.node.ty)
  let subst (f : t -> packed_v) (v : t) : packed_v = f v

  module Subst = struct
    type nonrec t = Raw_expr.Subst.t

    let pp = Raw_expr.Subst.pp
    let empty = Raw_expr.Subst.empty

    type missing = { missing : 'a. Svalue.Var.t -> 'a ty -> 'a value }

    let apply ~missing_var s (e : Svalue.t) : packed_v * t =
      let missing_var var ty =
        untyped (missing_var.missing var (type_type ty))
      in
      let v, s = Raw_expr.Subst.apply ~missing_var s e in
      (Packed (type_ v), s)

    let learn s (e : Svalue.t) (v : 'a value) : t option =
      Raw_expr.Subst.learn s e (untyped v)
  end
end

let cast_value : type a. a ty -> Expr.packed_v -> a t option =
 fun ty (Expr.Packed v) -> type_checked (untyped v) ty

let as_bool : Expr.packed_v -> sbool t =
 fun (Expr.Packed v) -> type_ (untyped v)

(* Recover a substituted (existential) value at any phantom kind. Sound here
   because the index is purely phantom. *)
let of_packed : Expr.packed_v -> 'a t = fun (Expr.Packed v) -> type_ (untyped v)
