(** String interning, probably going to be useful for qualified names etc. *)

type t = string

module Hcons = Ephemeron.K1.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

let tbl = Hcons.create 1023

let of_string (s : string) : t =
  match Hcons.find_opt tbl s with
  | Some interned -> interned
  | None ->
      Hcons.add tbl s s;
      s

let to_string (s : t) : string = s
let pp = Fmt.string
let equal (x : t) (y : t) = x == y
let hash = Hashtbl.hash
