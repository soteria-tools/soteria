type 'a t = ('a, unit) Hashtbl.t

let create i = Hashtbl.create i
let add tbl x = Hashtbl.replace tbl x ()
let mem tbl x = Hashtbl.mem tbl x
let remove tbl x = Hashtbl.remove tbl x
let iter f tbl = Hashtbl.iter (fun x _ -> f x) tbl
let to_seq tbl = Hashtbl.to_seq_keys tbl
let pp pp_elt : 'a t Fmt.t = Fmt.braces @@ Fmt.iter ~sep:Fmt.comma iter pp_elt

module type PrintableHashedType = sig
  include Hashtbl.HashedType

  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type elt
  type t

  val create : int -> t
  val add : t -> elt -> unit
  val mem : t -> elt -> bool
  val remove : t -> elt -> unit
  val iter : (elt -> unit) -> t -> unit
  val to_seq : t -> elt Seq.t
  val pp : Format.formatter -> t -> unit
end

module Make (Elt : PrintableHashedType) : S with type elt = Elt.t = struct
  module Hashtbl = Hashtbl.Make (Elt)

  type t = unit Hashtbl.t
  type elt = Elt.t

  let create i = Hashtbl.create i
  let add tbl x = Hashtbl.replace tbl x ()
  let mem tbl x = Hashtbl.mem tbl x
  let remove tbl x = Hashtbl.remove tbl x
  let iter f tbl = Hashtbl.iter (fun x _ -> f x) tbl
  let to_seq tbl = Hashtbl.to_seq_keys tbl
  let pp = Fmt.braces @@ Fmt.iter ~sep:Fmt.comma iter Elt.pp
end
