include Stdlib.String

type t = string

let pp ft s = Format.pp_print_string ft s

let index_of ~sub_str s =
  let re = Str.regexp_string sub_str in
  try
    let pos = Str.search_forward re s 0 in
    Some pos
  with Not_found -> None

let contains ~sub_str s = Option.is_some (index_of ~sub_str s)

module Interned : sig
  type t

  val intern : string -> t
  val to_string : t -> string

  (** Unique tag of the interned string. *)
  val tag : t -> int
end = struct
  module HC = Hc.Make_strong_thread_safe (struct
    type t = string

    let hash = Stdlib.Hashtbl.hash
    let equal = Stdlib.String.equal
  end)

  type t = string Hc.hash_consed

  let intern s = HC.hashcons s
  let to_string hc = hc.Hc.node
  let tag hc = hc.Hc.tag
end
