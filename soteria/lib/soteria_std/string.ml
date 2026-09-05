include Stdlib.String

type t = string

let pp ft s = Format.pp_print_string ft s
let show s = s
let contains ~sub s = Option.is_some (find_first ~sub s)

module Interned : sig
  type t

  val intern : string -> t
  val to_string : t -> string

  (** Unique tag of the interned string. *)
  val tag : t -> int

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end = struct
  module HC = Hc.Make_strong_thread_safe (struct
    type t = string

    let hash = Stdlib.Hashtbl.hash
    let equal = Stdlib.String.equal
  end)

  type t = string Hc.hash_consed

  let[@inline] intern s = HC.hashcons s
  let[@inline] to_string hc = hc.Hc.node
  let[@inline] tag hc = hc.Hc.tag
  let[@inline] hash s = Int.hash @@ tag s
  let[@inline] equal s1 s2 = tag s1 = tag s2
  let[@inline] compare s1 s2 = Int.compare (tag s1) (tag s2)
  let[@inline] pp ft s = Fmt.string ft (to_string s)
end
