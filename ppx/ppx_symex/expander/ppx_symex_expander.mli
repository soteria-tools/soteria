open Ppxlib

module If_sat : sig
  module Extension_name : sig
    type t = Sat | Sat1

    val to_string : t -> string
  end

  val expand : ext:Extension_name.t -> expression -> expression
end

module Sym_int_constants : sig
  val rewriter : Location.t -> string -> expression
  val suffix : char
end

module Sym_float_constants : sig
  val rewriter : Location.t -> string -> expression
  val suffix : char
end
