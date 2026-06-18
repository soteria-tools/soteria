open Logs.Import
include Svalue

(* Index aliases. The [NonZero]/[Zero]/[Overflowed] distinction is gone: all
   integer-like indices collapse to [sbv]. The names are kept so existing
   annotations keep resolving. *)
module T = struct
  (* The [NonZero]/[Zero]/[Overflowed] distinction is gone: all integer-like
     indices collapse to the single bitvector index [`Bv]. The names are kept so
     existing annotations keep resolving. *)
  type sbv = [ `Bv ]
  type sint = [ `Bv ]
  type sint_ovf = [ `Bv ]
  type nonzero = [ `Bv ]
  type zero = [ `Bv ]
  type sfloat = [ `Float ]
  type sbool = [ `Bool ]
  type sptr = [ `Ptr ]
  type sloc = [ `Loc ]
  type 'a sseq = [ `List of 'a ]
  type cval = [ sint | sptr | sfloat ]
  type any = [ sint_ovf | sfloat | sbool | sptr | sloc | any sseq ]

  let pp_sbv _ _ = ()
  let pp_sint _ _ = ()
  let pp_sint_ovf _ _ = ()
  let pp_nonzero _ _ = ()
  let pp_zero _ _ = ()
  let pp_sfloat _ _ = ()
  let pp_sbool _ _ = ()
  let pp_sptr _ _ = ()
  let pp_sloc _ _ = ()
  let pp_sseq _ _ _ = ()
  let pp_any _ _ = ()
  let pp_cval _ _ = ()
  let hash_sbv _ = 0
  let hash_sint _ = 0
  let hash_sint_ovf _ = 0
  let hash_nonzero _ = 0
  let hash_zero _ = 0
  let hash_sfloat _ = 0
  let hash_sbool _ = 0
  let hash_sptr _ = 0
  let hash_sloc _ = 0
  let hash_sseq _ = 0
  let hash_any _ = 0
  let hash_cval _ = 0
end

type sbool = T.sbool

let t_int = t_bv

include Bool

let[@inline] get_ty x = x.node.ty
let[@inline] type_type x = x
let[@inline] untype_type x = x
let ppa = pp
let pp _ = pp
let ppa_ty = pp_ty
let pp_ty _ = pp_ty
let hasha = hash
let hash _ = hash
let[@inline] untyped x = x
let[@inline] untyped_list l = l
let[@inline] type_ x = x

let type_checked : type a b. a t -> b ty -> b t option =
 fun x ty -> match eq_ty x.node.ty ty with Some Equal -> Some x | None -> None

let cast_checked = type_checked

let cast_float : type a. a t -> sfloat t option =
 fun x -> match x.node.ty with TFloat _ -> Some x | _ -> None

let cast_int : type a. a t -> (sbv t * int) option =
 fun x -> match x.node.ty with TBitVector n -> Some (x, n) | _ -> None

let size_of_int x = size_of x.node.ty

let cast_checked2 : type a b. a t -> b t -> (a t * a t * a ty) option =
 fun x y ->
  match eq_ty x.node.ty y.node.ty with
  | Some Equal -> Some (x, y, x.node.ty)
  | None -> None

(* {!Symex.Value.S} requirements that the syntactic module provides. *)
module Expr = Expr

let cast_value : type a. a ty -> Expr.packed_v -> a t option =
 fun ty (Packed v) ->
  match eq_ty v.node.ty ty with Some Equal -> Some v | None -> None

let as_bool pv =
  match cast_value TBool pv with
  | Some v -> v
  | None -> L.failwith "as_bool: not a boolean value"

(* Recover an existentially-wrapped value at a known kind, using the value's own
   type. Raise if the kind does not match (only sound on well-typed values). *)
let as_int (Packed v : Expr.packed_v) : sbv t =
  match v.node.ty with TBitVector _ -> v | _ -> L.failwith "as_int: not a bv"

let as_loc (Packed v : Expr.packed_v) : sloc t =
  match v.node.ty with TLoc _ -> v | _ -> L.failwith "as_loc: not a location"

let as_ptr (Packed v : Expr.packed_v) : sptr t =
  match v.node.ty with
  | TPointer _ -> v
  | _ -> L.failwith "as_ptr: not a pointer"

let as_float (Packed v : Expr.packed_v) : sfloat t =
  match v.node.ty with TFloat _ -> v | _ -> L.failwith "as_float: not a float"

module Bool = struct
  include Bool

  type t = sbool
end

module BitVec = struct
  include BitVec

  (* The nonzero variants no longer carry a static guarantee; they are kept as
     thin aliases (the non-zero obligation is now discharged at the symex
     layer). *)
  let mk_nz = mk
  let mki_masked n i = mk_masked n (Z.of_int i)
  let mki_nz = mki_masked
  let no_ovf_unsafe x = x

  let add_checked ~signed l r =
    (add ~checked:(checked_of_signed signed) l r, add_overflows ~signed l r)

  let sub_checked ~signed l r =
    (sub ~checked:(checked_of_signed signed) l r, sub_overflows ~signed l r)

  let mul_checked ~signed l r =
    (mul ~checked:(checked_of_signed signed) l r, mul_overflows ~signed l r)

  let neg_checked x = (neg ~checked:true x, neg_overflows x)
end

module Infix = struct
  include Infix

  let ( +!@ ) = ( +@ )
  let ( +!!@ ) = BitVec.add ~checked:checked_both
  let ( -!@ ) = ( -@ )
  let ( -!!@ ) = BitVec.sub ~checked:checked_both
  let ( *!@ ) = ( *@ )
  let ( *!!@ ) = BitVec.mul ~checked:checked_both
  let ( ~-! ) = ( ~- )
  let ( ~-!! ) = BitVec.neg ~checked:true
  let ( +?@ ) = BitVec.add_checked ~signed:false
  let ( +$?@ ) = BitVec.add_checked ~signed:true
  let ( -?@ ) = BitVec.sub_checked ~signed:false
  let ( -$?@ ) = BitVec.sub_checked ~signed:true
  let ( *?@ ) = BitVec.mul_checked ~signed:false
  let ( *$?@ ) = BitVec.mul_checked ~signed:true
  let ( ~-? ) = BitVec.neg_checked
end
