type gauge = Exhausted | Not_exhausted

type fuel = {
  steps : int;  (** The number of steps that can be taken per branch. *)
  branching : int;
      (** Number of times branching happens. TODO: this is kind of broken at the
          moment. *)
}

let default_fuel = { steps = 200; branching = 4 }

type t = fuel Dynarray.t

(** Default branching yield [2 ^ default_branching] branches at most *)

let init () =
  let d = Dynarray.create () in
  Dynarray.add_last d default_fuel;
  d

let backtrack_n d n =
  let len = Dynarray.length d in
  Dynarray.truncate d (len - n)

let save d = Dynarray.add_last d (Dynarray.get_last d)

let reset d =
  Dynarray.truncate d 1;
  Dynarray.set d 0 default_fuel

let consume_fuel_steps n d =
  let f = Dynarray.pop_last d in
  if f.steps >= n then (
    Dynarray.add_last d { f with steps = f.steps - n };
    Not_exhausted)
  else Exhausted

let consume_branching n d =
  let f = Dynarray.pop_last d in
  if f.branching >= n then (
    Dynarray.add_last d { f with branching = f.branching - n };
    Not_exhausted)
  else Exhausted

let branching_left () d =
  let f = Dynarray.get_last d in
  f.branching
