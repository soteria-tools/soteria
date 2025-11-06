include Soteria.Bv_values.Typed

let ptr_bits =
  Option.get Cerb_frontend.Ocaml_implementation.DefaultImpl.impl.sizeof_pointer
  * 8

let t_loc = t_loc ptr_bits
let t_ptr = t_ptr ptr_bits
let t_usize = t_int ptr_bits

module BitVec = struct
  include BitVec

  let of_bool x =
    of_bool
      (Option.get
         (Cerb_frontend.Ocaml_implementation.DefaultImpl.impl.sizeof_ity Bool))
      x

  let usize z = mk ptr_bits z
  let usizenz z = mk_nz ptr_bits z
  let usizei i = mki ptr_bits i
  let usizeinz i = mki_nz ptr_bits i

  let fit_to ?(signed = false) size (bv : [< T.sint ] t) : [> T.sint ] t =
    let cur = size_of_int bv in
    if cur = size then bv
    else if cur < size then extend ~signed (size - cur) bv
    else extract 0 (size - 1) bv
end

module Ptr = struct
  include Ptr

  let null = null ptr_bits
  let null_loc = null_loc ptr_bits
  let loc_of_z = loc_of_z ptr_bits
end

module Syntax = struct
  module U8 = struct
    module Sym_int_syntax = struct
      let mk_nonzero = BitVec.mki_nz 8
      let zero () = BitVec.zero 8
      let one () = BitVec.mki_nz 8 1
    end
  end

  module Usize = struct
    module Sym_int_syntax = struct
      let mk_nonzero = BitVec.mki_nz ptr_bits
      let zero () = BitVec.zero ptr_bits
      let one () = BitVec.mki_nz ptr_bits
    end
  end
end
