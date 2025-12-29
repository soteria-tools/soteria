open Soteria_std

type t = int
type 'ty iter_vars = (t * 'ty -> unit) -> unit

let hash = Int.hash
let[@inline] of_int i = i
let[@inline] to_int i = i
let to_string i = "|" ^ string_of_int i ^ "|"
let of_string s = int_of_string (String.sub s 1 (String.length s - 2))
let pp = Fmt.of_to_string to_string
let equal = Int.equal
let compare = Int.compare

module Set = PatriciaTree.MakeSet (Int)
module Hashset = Hashset.Hint
module Map = PatriciaTree.MakeMap (Int)
module Hashtbl = Hashtbl.Make (Int)

module Counter (Start_at : sig
  val start_at : int
end) =
struct
  type nonrec t = t

  let default = Start_at.start_at
  let[@inline] next i = (i, i + 1)
end

module Incr_counter_mut (Start_at : sig
  val start_at : int
end) =
struct
  module Counter = Counter (Start_at)

  type var = int

  include Reversible.Make_mutable (Counter)

  let get_next = wrap Counter.next
end
