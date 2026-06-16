include Z

let pp = pp_print
let show = to_string
let to_int_opt z = if Z.fits_int z then Some (Z.to_int z) else None
