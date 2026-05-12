open Soteria_std

type t = int
type 'ty iter_vars = (t * 'ty -> unit) -> unit

let hash = Int.hash
let[@inline] of_int i = i
let[@inline] to_int i = i
let to_string i = "|" ^ string_of_int i ^ "|"
let of_string s = int_of_string (String.sub s 1 (String.length s - 2))

(* User-facing names and opaque metadata are stored out-of-band so that [t]
   remains a bare int (cheap to hash/compare, unchanged SMT encoding). *)
let names : (int, string) Hashtbl.t = Hashtbl.create 16
let metadatas : (int, string) Hashtbl.t = Hashtbl.create 16
let set_name = Hashtbl.replace names
let name = Hashtbl.find_opt names
let has_name = Hashtbl.mem names
let set_metadata i m = Hashtbl.replace metadatas i m
let metadata i = Hashtbl.find_opt metadatas i

let pp ft i =
  match Hashtbl.find_opt names i with
  | Some n -> Format.pp_print_string ft n
  | None -> Format.pp_print_string ft (to_string i)

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
