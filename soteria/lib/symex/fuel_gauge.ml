open Soteria_std

type exhaust = Exhausted | Not_exhausted

module Fuel_value = struct
  type t = Infinite | Finite of int

  let pp ft = function
    | Infinite -> Fmt.string ft "infinite"
    | Finite i -> Fmt.int ft i

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

let is_infinite = function
  | { steps = Infinite; branching = Infinite } -> true
  | _ -> false

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

module Cli = struct
  let validate_or_exit (g : (t, string) result) : t =
    match g with
    | Ok g -> g
    | Error e ->
        Fmt.epr "%a" Terminal.Color.pp_fatal e;
        exit Cmdliner.Cmd.Exit.cli_error

  let fuel_value_conv =
    let parse v =
      match v with
      | "inf" | "infinite" -> Ok Fuel_value.Infinite
      | v -> (
          match int_of_string_opt v with
          | Some v -> Ok (Fuel_value.Finite v)
          | None -> Error ("Invalid fuel: " ^ v))
    in
    Cmdliner.Arg.conv' ~docv:"FUEL_VALUE" (parse, Fuel_value.pp)

  let process_args ~default steps branching infinite_fuel =
    match (infinite_fuel, steps, branching) with
    | true, (Some Fuel_value.Infinite | None), (Some Fuel_value.Infinite | None)
      ->
        Ok infinite
    | true, Some (Finite _), _ | true, _, Some (Finite _) ->
        Error "Cannot use --infinite-fuel with other finite fuel options"
    | false, steps, branching ->
        let res =
          Option.fold ~none:default
            ~some:(fun steps -> { default with steps })
            steps
        in
        let res =
          Option.fold ~none:res
            ~some:(fun branching -> { res with branching })
            branching
        in
        Ok res

  let term ~default () =
    let steps =
      Cmdliner.Arg.(
        value
        & opt (some fuel_value_conv) None
        & info [ "step-fuel" ]
            ~env:(Cmdliner.Cmd.Env.info "SOTERIA_STEP_FUEL")
            ~doc:
              (Fmt.str
                 "How many symbolic execution steps (~1 statement) are \
                  executed per branch, clashes with --infinite-fuel if finite. \
                  Default: %a"
                 Fuel_value.pp default.steps))
    in
    let branching =
      Cmdliner.Arg.(
        value
        & opt (some fuel_value_conv) None
        & info
            [ "branching-fuel"; "branch-fuel" ]
            ~docv:"N"
            ~env:(Cmdliner.Cmd.Env.info "SOTERIA_BRANCHING_FUEL")
            ~doc:
              (Fmt.str
                 "How many times symbolic execution may branch, may yield ~2^N \
                  branches!, clashes with --infinite-fuel if finite. Default: \
                  %a"
                 Fuel_value.pp default.branching))
    in
    let infinite_fuel =
      Cmdliner.Arg.(
        value
        & flag
        & info [ "infinite-fuel" ]
            ~env:(Cmdliner.Cmd.Env.info "SOTERIA_INFINITE_FUEL")
            ~doc:
              (Fmt.str "Use infinite fuel (may not terminate). Default: %b"
                 (is_infinite default)))
    in
    Cmdliner.Term.(
      const (process_args ~default) $ steps $ branching $ infinite_fuel)
end
