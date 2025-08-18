type 'a t = ('a, unit) Hashtbl.t

let with_capacity i = Hashtbl.create i

let singleton x =
  let tbl = Hashtbl.create 1 in
  Hashtbl.add tbl x ();
  tbl

let add tbl x = Hashtbl.replace tbl x ()
let add_iter tbl iter = iter @@ add tbl
let mem tbl x = Hashtbl.mem tbl x
let remove tbl x = Hashtbl.remove tbl x
let iter f tbl = Hashtbl.iter (fun x _ -> f x) tbl
let to_seq tbl = Hashtbl.to_seq_keys tbl

let of_seq seq =
  let tbl = with_capacity 0 in
  Seq.iter (fun x -> add tbl x) seq;
  tbl

let cardinal tbl = Hashtbl.length tbl
let copy tbl = Hashtbl.copy tbl
let subseteq lset rset = to_seq lset |> Seq.for_all (fun x -> mem rset x)
let equal lset rset = cardinal lset = cardinal rset && subseteq lset rset

let pp pp_elt : Format.formatter -> 'a t -> unit =
  Fmt.braces @@ Fmt.iter ~sep:Fmt.comma iter pp_elt

module type PrintableHashedType = sig
  include Hashtbl.HashedType

  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type elt
  type t

  val with_capacity : int -> t
  val singleton : elt -> t
  val add : t -> elt -> unit
  val add_iter : t -> elt Iter.t -> unit
  val mem : t -> elt -> bool
  val remove : t -> elt -> unit
  val iter : (elt -> unit) -> t -> unit
  val to_seq : t -> elt Seq.t
  val of_seq : elt Seq.t -> t
  val cardinal : t -> int
  val copy : t -> t
  val subseteq : t -> t -> bool
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make (Elt : PrintableHashedType) : S with type elt = Elt.t = struct
  module Hashtbl = Hashtbl.Make (Elt)

  type t = unit Hashtbl.t
  type elt = Elt.t

  let singleton x =
    let tbl = Hashtbl.create 1 in
    Hashtbl.add tbl x ();
    tbl

  let with_capacity i = Hashtbl.create i
  let add tbl x = Hashtbl.replace tbl x ()
  let add_iter tbl iter = iter @@ add tbl
  let mem tbl x = Hashtbl.mem tbl x
  let remove tbl x = Hashtbl.remove tbl x
  let iter f tbl = Hashtbl.iter (fun x _ -> f x) tbl
  let to_seq tbl = Hashtbl.to_seq_keys tbl

  let of_seq seq =
    let tbl = with_capacity 0 in
    Seq.iter (fun x -> add tbl x) seq;
    tbl

  let cardinal tbl = Hashtbl.length tbl
  let copy tbl = Hashtbl.copy tbl
  let subseteq lset rset = to_seq lset |> Seq.for_all (fun x -> mem rset x)
  let equal lset rset = cardinal lset = cardinal rset && subseteq lset rset
  let pp = Fmt.braces @@ Fmt.iter ~sep:Fmt.comma iter Elt.pp
end

module Hstring = Make (struct
  include String

  let pp = Fmt.string
end)

module Hint = Make (struct
  include Int

  let pp = Fmt.int
end)
