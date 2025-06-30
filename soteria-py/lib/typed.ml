include Svalue

module T = struct
  type sbool = [ `Bool ]

  let pp_sbool _ _ = ()
end

type nonrec +'a t = t
type nonrec +'a ty = ty
type sbool = T.sbool
