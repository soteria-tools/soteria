open Simple_smt

let int_to_bv size i =
  let bv_fam = ifam "int_to_bv" [ size ] in
  app bv_fam [ i ]

let bv_to_int signed bv =
  if signed then app_ "sbv_to_int" [ bv ] else app_ "ubv_to_int" [ bv ]

let bv_and l r = app_ "bvand" [ l; r ]
let bv_or l r = app_ "bvor" [ l; r ]
let bv_xor l r = app_ "bvxor" [ l; r ]

let lift_binary fn size signed l r =
  bv_to_int signed (fn (int_to_bv size l) (int_to_bv size r))

let int_band = lift_binary bv_and
let int_bor = lift_binary bv_or
let int_bxor = lift_binary bv_xor
