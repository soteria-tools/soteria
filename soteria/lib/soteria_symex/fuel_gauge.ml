open Soteria_std

type exhaust = Exhausted | Not_exhausted

module Fuel_value = struct
  type t = Infinite | Finite of int [@@deriving show { with_path = false }]

  let decrease fv i =
    match fv with
    | Infinite -> Infinite
    | Finite n -> Finite (Int.max (n - i) 0)

  let geq fv i = match fv with Infinite -> true | Finite n -> n >= i
end

type t = {
  steps : Fuel_value.t;
      (** The number of steps that can be taken per branch. *)
  branching : Fuel_value.t;
      (** Number of times branching happens. Careful, the total number of
          branches is potentially exponential in the number of branchings *)
      (* TODO:
          Realised that we could just have the symex monad do a
          [Iter.take branches] and we wouldn't ever have to keep track of
          branching inside execution since everything is lazily executed *)
}
[@@deriving show { with_path = false }]

(* Divided by two to avoid overflow in case we add some bits to these values. *)
let infinite = { steps = Infinite; branching = Infinite }

let consume_fuel_steps n gauge =
  if Fuel_value.geq gauge.steps n then
    (Not_exhausted, { gauge with steps = Fuel_value.decrease gauge.steps n })
  else (Exhausted, { gauge with steps = Finite 0 })

let consume_branching n gauge =
  if Fuel_value.geq gauge.branching n then
    ( Not_exhausted,
      { gauge with branching = Fuel_value.decrease gauge.branching n } )
  else (Exhausted, { gauge with branching = Finite 0 })

(** Receives a list and a fuel gauge and returns a list that contains at most as
    many elements as the fuel gauge allows. It also returns the updated fuel
    gauge. *)
let take_branches list fuel =
  match fuel.branching with
  | Infinite -> (list, fuel)
  | Finite n ->
      let list, taken = List.take_count (n + 1) list in
      let to_consume = Int.max (taken - 1) 0 in
      let fuel = { fuel with branching = Finite (n - to_consume) } in
      (list, fuel)
