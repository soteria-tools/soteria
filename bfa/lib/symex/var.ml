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

module Incr_counter_mut = struct
  type var = t
  type t = int Dynarray.t

  let init () =
    let t = Dynarray.create () in
    Dynarray.add_last t 0;
    t

  let backtrack_n t n =
    let len = Dynarray.length t in
    Dynarray.truncate t (len - n)

  let save t = Dynarray.add_last t (Dynarray.get_last t)

  let reset t =
    Dynarray.truncate t 1;
    Dynarray.set t 0 0

  let get_next t =
    let var = Dynarray.pop_last t in
    Dynarray.add_last t (var + 1);
    var
end
