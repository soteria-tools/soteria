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

module Set = Set.Make (Int)

module Hashset = Hashset.Make (struct
  include Int

  let pp = pp
end)

module Counter = struct
  type nonrec t = t

  let default = 0
  let[@inline] next i = (i, i + 1)
end

module Incr_counter_mut = struct
  type var = int

  include Reversible.Make_mutable (Counter)

  let get_next = wrap Counter.next
end
