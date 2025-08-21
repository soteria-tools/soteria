open Soteria_logs
open Syntaxes.FunctionWrap
module L = Logs.L
module List = ListLabels

module Meta = struct
  (** Management of meta-information about the execution. *)

  module type S = sig
    (** Management of meta-information about the execution. *)

    (** Used for various bookkeeping utilities. *)
    module Range : Soteria_stats.CodeRange
  end

  module Dummy : S with type Range.t = unit = struct
    (** Can be used by users as a default *)

    module Range = struct
      type t = unit

      let to_yojson () = `Null
      let of_yojson _ = Ok ()
    end
  end
end

module type Base = sig
  module Value : Value.S
  module Stats : Soteria_stats.S

  type 'a t

  module MONAD : Monad.Base with type 'a t = 'a t

  type 'a v := 'a Value.t
  type 'a vt := 'a Value.ty
  type sbool := Value.sbool

  val assume : sbool v list -> unit t
  val vanish : unit -> 'a t
  val assert_ : sbool v -> bool t
  val assert_ox : sbool v -> bool t
  val nondet : 'a vt -> 'a v t
  val fresh_var : 'a vt -> Var.t t

  val branch_on :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  (** Branches on value, and takes at most one branch, starting with the [then]
      branch. This means that if the [then_] branch is SAT, it is taken and the
      [else_] branch is ignored, otherwise the [else_] branch is taken. This is,
      of course, UX-sound, but not OX-sound. *)
  val branch_on_take_one :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a t) ->
    else_:(unit -> 'a t) ->
    'a t

  val branches : (unit -> 'a t) list -> 'a t

  (** {2 Fuel} *)

  val consume_fuel_steps : int -> unit t

  (** [run ~fuel  p] actually performs symbolic execution of the symbolic
      process [p] and returns a list of obtained branches which capture the
      outcome together with a path condition that is a list of boolean symbolic
      values.
      - statistics correponding to the execution.

      Users may optionally pass a {{!Fuel_gauge.t}fuel gauge} to limit execution
      depth and breadth. *)
  val run : ?fuel:Fuel_gauge.t -> 'a t -> ('a * sbool v list) list

  (** Same as {!run}, but returns additional information about execution, see
      {!Soteria_stats}. *)
  val run_with_stats :
    ?fuel:Fuel_gauge.t -> 'a t -> ('a * sbool v list) list Stats.with_stats

  (** Same as {!run} but has to be run within {!Stats.with_stats} or will throw
      an exception. This function is exposed should users wish to run several
      symbolic execution processes using a single [stats] record. *)
  val run_needs_stats : ?fuel:Fuel_gauge.t -> 'a t -> ('a * sbool v list) list
end

module type S = sig
  include Base
  include Monad.Base with type 'a t := 'a t

  (** Gives up on this path of execution for incompleteness reason. For
      instance, if a give feature is unsupported.

      Logs the result, and adds the reason to the execution statistics. *)
  val give_up : loc:Stats.Range.t -> string -> 'a t

  (** If the given option is None, gives up execution, otherwise continues,
      unwrapping the option. *)
  val some_or_give_up : loc:Stats.Range.t -> string -> 'a option -> 'a t

  (** If the given result is Error, gives up execution, otherwise continues,
      unwrapping the result. *)
  val ok_or_give_up : ('a, string * Stats.Range.t) result -> 'a t

  val all : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_list : 'a list -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val fold_iter : 'a Iter.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
  val fold_seq : 'a Seq.t -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t

  module rec Result : sig
    type nonrec ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) Compo_res.t t

    val ok : 'ok -> ('ok, 'err, 'fix) t
    val error : 'err -> ('ok, 'err, 'fix) t
    val miss : 'fix list -> ('ok, 'err, 'fix) t

    (** Missing without any fix. Will add to the statistics and log that
        information. *)
    val miss_no_fix : reason:string -> unit -> ('ok, 'err, 'fix) t

    val bind :
      ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

    val map : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t

    val bind_2 :
      ('ok, 'err, 'fix) t ->
      f:('ok -> ('a, 'b, 'fix) t) ->
      fe:('err -> ('a, 'b, 'fix) t) ->
      ('a, 'b, 'fix) t

    val map_error : ('ok, 'err, 'fix) t -> ('err -> 'a) -> ('ok, 'a, 'fix) t
    val map_missing : ('ok, 'err, 'fix) t -> ('fix -> 'a) -> ('ok, 'err, 'a) t

    val fold_list :
      'a list -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t

    val fold_iter :
      'a Iter.t -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t

    val fold_seq :
      'a Seq.t -> init:'b -> f:('b -> 'a -> ('b, 'c, 'd) t) -> ('b, 'c, 'd) t
  end

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let** ) :
      ('a, 'c, 'd) Result.t ->
      ('a -> ('b, 'c, 'd) Result.t) ->
      ('b, 'c, 'd) Result.t

    val ( let++ ) : ('a, 'c, 'd) Result.t -> ('a -> 'b) -> ('b, 'c, 'd) Result.t
    val ( let+- ) : ('a, 'b, 'd) Result.t -> ('b -> 'c) -> ('a, 'c, 'd) Result.t
    val ( let+? ) : ('a, 'b, 'c) Result.t -> ('c -> 'd) -> ('a, 'b, 'd) Result.t

    module Symex_syntax : sig
      type sbool_v := Value.sbool Value.t

      val branch_on :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        sbool_v ->
        then_:(unit -> 'a t) ->
        else_:(unit -> 'a t) ->
        'a t

      val branch_on_take_one :
        ?left_branch_name:string ->
        ?right_branch_name:string ->
        sbool_v ->
        then_:(unit -> 'a t) ->
        else_:(unit -> 'a t) ->
        'a t
    end
  end
end

module Extend (Base : Base) = struct
  include Base
  include MONAD

  let all fn xs =
    let rec aux acc rs =
      match rs with
      | [] -> return (List.rev acc)
      | r :: rs -> bind (fn r) @@ fun x -> aux (x :: acc) rs
    in
    aux [] xs

  let give_up ~loc reason =
    (* The bind ensures that the side effect will not be enacted before the whole process is ran. *)
    bind (return ()) @@ fun () ->
    L.info (fun m -> m "%s" reason);
    Stats.As_ctx.push_give_up_reason ~loc reason;
    vanish ()

  let some_or_give_up ~loc reason = function
    | Some x -> return x
    | None -> give_up ~loc reason

  let ok_or_give_up = function
    | Ok x -> return x
    | Error (msg, loc) -> give_up ~loc msg

  let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return ~fold x ~init ~f
  let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
  let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
  let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f

  module Result = struct
    include Compo_res.T (MONAD)

    let miss_no_fix ~reason () =
      bind (ok ()) @@ fun () ->
      Stats.As_ctx.push_missing_without_fix reason;
      L.debug (fun m -> m "Missing without fix: %s" reason);
      miss []

    let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return:ok ~fold x ~init ~f
    let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
    let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
    let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let** ) = Result.bind
    let ( let++ ) = Result.map
    let ( let+- ) = Result.map_error
    let ( let+? ) = Result.map_missing

    module Symex_syntax = struct
      let branch_on = branch_on
      let branch_on_take_one = branch_on_take_one
    end
  end
end

module Make (Meta : Meta.S) (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value and module Stats.Range = Meta.Range =
Extend (struct
  module Solver = Solver.Mutable_to_in_place (Sol)
  module Stats = Soteria_stats.Make (Meta.Range)

  module Fuel = struct
    include Reversible.Effectful (Fuel_gauge)

    let consume_branching n = wrap (Fuel_gauge.consume_branching n) ()
    let consume_fuel_steps n = wrap (Fuel_gauge.consume_fuel_steps n) ()
    let take_branches list = wrap (Fuel_gauge.take_branches list) ()
  end

  module Value = Solver.Value
  module MONAD = Monad.IterM

  type 'a t = 'a Iter.t

  module Symex_state : Reversible.In_place = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel.backtrack_n n

    let save () =
      Solver.save ();
      Fuel.save ()

    let reset () = Solver.reset ()
  end

  let consume_fuel_steps n f =
    match Fuel.consume_fuel_steps n with
    | Exhausted -> L.debug (fun m -> m "Exhausted step fuel")
    | Not_exhausted ->
        Stats.As_ctx.add_steps n;
        f ()

  let assume learned f =
    let rec aux acc learned =
      match learned with
      | [] ->
          Solver.add_constraints acc;
          f ()
      | l :: ls -> (
          let l = Solver.simplify l in
          match Value.as_bool l with
          | Some true -> aux acc ls
          | Some false ->
              L.trace (fun m -> m "Assuming false, stopping this branch")
          | None -> aux (l :: acc) ls)
    in
    aux [] learned

  (** Assert is [if%sat (not value) then error else ok]. In UX, assert only
      returns false if (not value) is *really* satisfiable. *)
  let assert_ value f =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> f true
    | Some false -> f false
    | None ->
        let result =
          let@ () =
            Logs.with_section
              (Fmt.str "Checking entailment for %a" Value.ppa value)
          in
          Symex_state.save ();
          Solver.add_constraints [ Value.(not value) ];
          let sat = Solver_result.is_sat (Solver.sat ()) in
          Symex_state.backtrack_n 1;
          not sat
        in
        f result

  let assert_ox value f =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> f true
    | Some false -> f false
    | None ->
        let result =
          let@ () =
            Logs.with_section
              (Fmt.str "Checking entailment for %a" Value.ppa value)
          in
          Symex_state.save ();
          Solver.add_constraints [ Value.(not value) ];
          let unsat = Solver_result.is_unsat (Solver.sat ()) in
          Symex_state.backtrack_n 1;
          unsat
        in
        f result

  let nondet ty f =
    let v = Solver.fresh_var ty in
    let v = Value.mk_var v ty in
    f v

  let fresh_var ty f = f (Solver.fresh_var ty)

  let branch_on ?(left_branch_name = "Left branch")
      ?(right_branch_name = "Right branch") guard ~(then_ : unit -> 'a Iter.t)
      ~(else_ : unit -> 'a Iter.t) : 'a Iter.t =
   fun f ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        let left_unsat = ref false in
        Symex_state.save ();
        Logs.with_section ~is_branch:true left_branch_name (fun () ->
            Solver.add_constraints ~simplified:true [ guard ];
            let sat_res = Solver.sat () in
            left_unsat := Solver_result.is_unsat sat_res;
            if Solver_result.is_sat sat_res then then_ () f
            else L.trace (fun m -> m "Branch is not feasible"));
        Symex_state.backtrack_n 1;
        Logs.with_section ~is_branch:true right_branch_name (fun () ->
            Solver.add_constraints [ Value.(not guard) ];
            if !left_unsat then
              (* Right must be sat since left was not! We didn't branch so we don't consume the counter *)
              else_ () f
            else
              match Fuel.consume_branching 1 with
              | Exhausted ->
                  L.debug (fun m ->
                      m "Exhausted branching fuel, not continuing")
              | Not_exhausted ->
                  Stats.As_ctx.add_branches 1;
                  if Solver_result.is_sat (Solver.sat ()) then else_ () f
                  else L.trace (fun m -> m "Branch is not feasible"))

  let branch_on_take_one ?left_branch_name:_ ?right_branch_name:_ guard ~then_
      ~else_ : 'a Iter.t =
   fun f ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        let left_sat = ref true in
        if Solver_result.is_sat (Solver.sat ()) then then_ () f
        else left_sat := false;
        Symex_state.backtrack_n 1;
        if not !left_sat then (
          Solver.add_constraints [ Value.(not guard) ];
          else_ () f)

  let branches (brs : (unit -> 'a Iter.t) list) : 'a Iter.t =
   fun f ->
    let brs = Fuel.take_branches brs in
    (* If there are 0 or 1 branches, we don't do anything,
       else we add how many *new* branches we take. *)
    Stats.As_ctx.add_branches (max (List.length brs - 1) 0);
    match brs with
    | [] -> ()
    | [ a ] -> a () f
    | a :: r ->
        (* First branch should not backtrack and last branch should not save *)
        let with_section =
          let branch_counter = ref 0 in
          fun k ->
            Logs.start_section ~is_branch:true
              ("Branch number " ^ string_of_int !branch_counter);
            k ();
            Logs.end_section ();
            incr branch_counter
        in
        let rec loop brs =
          match brs with
          | [ x ] ->
              Symex_state.backtrack_n 1;
              with_section @@ fun () -> x () f
          | x :: r ->
              Symex_state.backtrack_n 1;
              Symex_state.save ();
              (with_section @@ fun () -> x () f);
              loop r
          | [] -> failwith "unreachable"
        in
        Symex_state.save ();
        (with_section @@ fun () -> a () f);
        loop r

  let run_needs_stats ?(fuel = Fuel_gauge.infinite) iter =
    let@ () = Stats.As_ctx.add_time_of in
    Symex_state.reset ();
    let@ () = Fuel.run ~init:fuel in
    let l = ref [] in
    let () = iter @@ fun x -> l := (x, Solver.as_values ()) :: !l in
    List.rev !l

  let run ?fuel iter =
    let@ () = Stats.As_ctx.with_stats_ignored () in
    run_needs_stats ?fuel iter

  let run_with_stats ?fuel iter =
    let@ () = Stats.As_ctx.with_stats () in
    run_needs_stats ?fuel iter

  let vanish () _f = ()
end)
