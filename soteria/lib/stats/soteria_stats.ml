module Hstring = Hashtbl.Hstring
module Hsset = Hashset.Hstring

type 'range stats = {
  mutable exec_time : float;  (** Execution time for the whole execution *)
  give_up_reasons : 'range Dynarray.t Hstring.t;
      (** For each "give up reason" (a string) we keep track of the list of code
          ranges at which we reached it *)
  missing_without_fixes : string Dynarray.t;
      (** A list of reasons why we couldn't get a fix for a given Missing error
      *)
  mutable branch_number : int;
      (** Number of explored branches. Note that this includes the number of
          attempted branches that ended up vanishing: it may be greated than the
          length of the list obtained at the end of execution. *)
  mutable steps_number : int;  (** Number of steps taken *)
  mutable unexplored_branch_number : int;
      (** Number of branches that were not explored because of fuel exhaustion.
          This includes branches not taking due to missing branch fuel, and
          branches that were cut due to step fuel. *)
  mutable sat_unknowns : int;
      (** Number of times the SAT solver returned [unknown] *)
}
[@@deriving yojson]

type ('a, 'range) with_stats = { res : 'a; stats : 'range stats }

let map_with_stats f { res; stats } = { res = f res; stats }

let create () =
  {
    exec_time = 0.0;
    give_up_reasons = Hstring.create 0;
    missing_without_fixes = Dynarray.create ();
    branch_number = 1;
    steps_number = 0;
    unexplored_branch_number = 0;
    sat_unknowns = 0;
  }

let with_empty_stats res = { res; stats = create () }

module type CodeRange = sig
  type t [@@deriving yojson]
end

module type S = sig
  module Range : CodeRange

  type t = Range.t stats [@@deriving yojson]
  type nonrec 'a with_stats = ('a, Range.t) with_stats

  (** Merges two stats records. Does not modify either. *)
  val merge : t -> t -> t

  (** Empty record of statistics *)
  val create : unit -> t

  (** Push a reason for giving up symex, with the location at which it was
      reached. *)
  val push_give_up_reason : loc:Range.t -> string -> t -> unit

  (** Push a reason for not being able to fix a Missing error *)
  val push_missing_without_fix : string -> t -> unit

  (** Returns true if the stats record contains any give up reasons *)
  val did_give_up : t -> bool

  (** Adds the given execution time to the stats record. *)
  val add_exec_time : float -> t -> unit

  (** Dumps the stats record to a file in JSON format *)
  val dump : t -> string -> unit

  module As_ctx : sig
    (** Module for manipulating statistics as a context using algebraic effects
        (that are hidden). All calls to any function in this module should
        happen only inside a function wrapped with {!with_stats}, ensuring that
        the statistics are properly passed around. *)

    (** [with_stats () f] runs function [f] and handles effects raised by the
        functions of this module such as {!add_exec_time}, and returns a record
        containing the result of executing [f] together with the obtained
        statistics. *)
    val with_stats : unit -> (unit -> 'a) -> 'a with_stats

    (** [with_stats_ignored () f] runs function [f] and handles effects raised
        by the functions of this module, but ignores their effect. This is to be
        used when the user does not wish to pay the (minor) performance cost of
        stats bookkeeping. *)
    val with_stats_ignored : unit -> (unit -> 'a) -> 'a

    (** Adds the given execution time to the stats record. Should be handled by
        Soteria itself. *)
    val add_exec_time : float -> unit

    (** Measures time taken by the given function and adds it to the statistics
        in the environment. *)
    val add_time_of : (unit -> 'a) -> 'a

    (** Adds branches to the count of statistics. Should be handled by Soteria
        itself *)
    val add_branches : int -> unit

    (** Adds steps to the count of statistics. Handled by Soteria when users
        call {!Soteria_symex.Symex.S.consume_fuel_steps} *)
    val add_steps : int -> unit

    (** Adds unexplored branches due to fuel exhaustion to the count of
        statistics. Handled by Soteria when users call
        {!Soteria_symex.Symex.S.consume_fuel_steps} or when branching *)
    val add_unexplored_branches : int -> unit

    (** Adds the number of times the SAT solver returned unknowns to the count
        of statistics. Handled by Soteria. *)
    val add_sat_unknowns : int -> unit

    (** Push a reason for not being able to fix a Missing error. Handled by
        Soteria when users call {!Soteria_symex.Symex.S.Result.miss_without_fix}
    *)
    val push_missing_without_fix : string -> unit

    (** Push a reason for giving up symex, with the location at which it was
        reached. Handled by Soteria when users call
        {!Soteria_symex.Symex.S.give_up} *)
    val push_give_up_reason : loc:Range.t -> string -> unit
  end
end

module Make (Range : CodeRange) : S with module Range = Range = struct
  module Range = Range

  type t = Range.t stats [@@deriving yojson]
  type nonrec 'a with_stats = ('a, Range.t) with_stats

  let create () = create ()

  let merge t1 t2 =
    let merge_hstrings h1 h2 =
      let h = Hstring.create (Hstring.length h1 + Hstring.length h2) in
      let add_pair key arr =
        match Hstring.find_opt h key with
        | Some existing_arr -> Dynarray.append existing_arr arr
        | None ->
            (* Avoid sharing memory... *)
            let empty = Dynarray.create () in
            Hstring.add h key empty;
            Dynarray.append empty arr
      in
      Hstring.iter add_pair h1;
      Hstring.iter add_pair h2;
      h
    in

    let give_up_reasons =
      merge_hstrings t1.give_up_reasons t2.give_up_reasons
    in
    let missing_without_fixes =
      let d = Dynarray.create () in
      Dynarray.append d t1.missing_without_fixes;
      Dynarray.append d t2.missing_without_fixes;
      d
    in
    {
      give_up_reasons;
      missing_without_fixes;
      exec_time = t1.exec_time +. t2.exec_time;
      branch_number = t1.branch_number + t2.branch_number;
      steps_number = t1.steps_number + t2.steps_number;
      unexplored_branch_number =
        t1.unexplored_branch_number + t2.unexplored_branch_number;
      sat_unknowns = t1.sat_unknowns + t2.sat_unknowns;
    }

  let push_give_up_reason ~loc reason t =
    match Hstring.find_opt t.give_up_reasons reason with
    | Some arr -> Dynarray.add_last arr loc
    | None ->
        let arr = Dynarray.create () in
        Dynarray.add_last arr loc;
        Hstring.add t.give_up_reasons reason arr

  let push_missing_without_fix reason t =
    Dynarray.add_last t.missing_without_fixes reason

  let did_give_up t = Hstring.length t.give_up_reasons > 0
  let add_exec_time time t = t.exec_time <- t.exec_time +. time

  let dump t file =
    let oc = open_out file in
    let json = to_yojson t in
    Yojson.Safe.to_channel oc json;
    close_out oc

  module As_ctx = struct
    type _ Effect.t += Apply : (t -> unit) -> unit Effect.t

    let with_stats () f =
      let stats = create () in
      let res =
        try f ()
        with effect Apply f, k ->
          f stats;
          Effect.Deep.continue k ()
      in
      { res; stats }

    let with_stats_ignored () f =
      try f () with effect Apply _, k -> Effect.Deep.continue k ()

    let[@inline] apply f = Effect.perform (Apply f)

    let push_give_up_reason ~loc reason =
      apply (push_give_up_reason ~loc reason)

    let push_missing_without_fix reason =
      apply (push_missing_without_fix reason)

    let add_exec_time time = apply (add_exec_time time)

    let add_time_of f =
      let start = Unix.gettimeofday () in
      let res = f () in
      add_exec_time (Unix.gettimeofday () -. start);
      res

    let add_branches n =
      apply (fun stats -> stats.branch_number <- stats.branch_number + n)

    let add_steps n =
      apply (fun stats -> stats.steps_number <- stats.steps_number + n)

    let add_unexplored_branches n =
      apply (fun stats ->
          stats.unexplored_branch_number <- stats.unexplored_branch_number + n)

    let add_sat_unknowns n =
      apply (fun stats -> stats.sat_unknowns <- stats.sat_unknowns + n)
  end
end
