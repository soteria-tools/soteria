open Hc
include Svalue

module T = struct
  type sint = [ `NonZero | `Zero ]
  type sint_ovf = [ `NonZero | `Zero | `Overflowed ]
  type nonzero = [ `NonZero ]
  type zero = [ `Zero ]
  type sfloat = [ `Float ]
  type sbool = [ `Bool ]
  type sptr = [ `Ptr ]
  type sloc = [ `Loc ]
  type 'a sseq = [ `List of 'a ]
  type cval = [ sint | sptr | sfloat ]
  type any = [ sint_ovf | sfloat | sbool | sptr | sloc | any sseq ]

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

type nonrec +'a t = t
type nonrec +'a ty = ty
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
let[@inline] cast x = x
let[@inline] untyped x = x
let[@inline] untyped_list l = l
let[@inline] type_ x = x
let type_checked x ty = if equal_ty x.node.ty ty then Some x else None
let cast_checked = type_checked
let cast_float x = if is_float x.node.ty then Some x else None
let cast_int x = if is_bv x.node.ty then Some (x, size_of x.node.ty) else None
let size_of_int x = size_of x.node.ty

let cast_checked2 x y =
  if equal_ty x.node.ty y.node.ty then Some (x, y, x.node.ty) else None

module BitVec = struct
  include BitVec

  let mk_nz n z =
    if Z.equal z Z.zero then failwith "Zero value in mk_nonzero" else mk n z

  let mki_masked n i = mk_masked n (Z.of_int i)

  let mki_nz n i =
    if i = 0 then failwith "Zero value in mki_nonzero" else mki_masked n i
end

module Infix = struct
  include Infix

  let ( +!@ ) = ( +@ )
  let ( +!!@ ) = BitVec.add ~checked:true
  let ( -!@ ) = ( -@ )
  let ( -!!@ ) = BitVec.sub ~checked:true
  let ( *!@ ) = ( *@ )
  let ( *!!@ ) = BitVec.mul ~checked:true
  let ( ~-! ) = ( ~- )
  let mk_checked_op op ovf = fun l r -> (op ?checked:(Some true) l r, ovf l r)
  let ( +?@ ) = mk_checked_op BitVec.add (BitVec.add_overflows ~signed:false)
  let ( +$?@ ) = mk_checked_op BitVec.add (BitVec.add_overflows ~signed:true)
  let ( -?@ ) = mk_checked_op BitVec.sub (BitVec.sub_overflows ~signed:false)
  let ( -$?@ ) = mk_checked_op BitVec.sub (BitVec.sub_overflows ~signed:true)
  let ( *?@ ) = mk_checked_op BitVec.mul (BitVec.mul_overflows ~signed:false)
  let ( *$?@ ) = mk_checked_op BitVec.mul (BitVec.mul_overflows ~signed:true)
  let ( ~-? ) x = (~-x, BitVec.neg_overflows x)
end
