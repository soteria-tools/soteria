(* A minimal stand-in for the real [Svalue]/[Typed] modules, just enough to
   type-check the output of [match%ty]. *)

type ext = Adt of int | ThinPtr | FullPtr

type ty =
  | TBool
  | TFloat of int
  | TLoc of int
  | TPointer of int
  | TSeq of ty
  | TBitVector of int
  | TExtension of ext

module Typed = struct
  type +'a t

  module T = struct
    type sint = [ `NonZero | `Zero ]
    type sfloat = [ `Float ]
    type sbool = [ `Bool ]
    type sloc = [ `Loc ]
    type sptr = [ `Ptr ]
    type 'a sseq = [ `List of 'a ]
    type sptr_f = [ `FullPtr ]
    type sptr_t = [ `ThinPtr ]
    type adt = [ `Adt ]

    type any =
      [ sint | sfloat | sbool | sloc | sptr | any sseq | sptr_f | sptr_t | adt ]
  end

  let get_ty : 'a t -> ty = fun _ -> assert false
  let cast : 'a t -> 'b t = fun _ -> assert false
end
