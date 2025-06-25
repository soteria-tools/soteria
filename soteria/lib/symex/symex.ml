open Soteria_logs
open Syntaxes.FunctionWrap
module L = Logs.L
module List = ListLabels

let is_sat (result : Solver.result) =
  match result with Sat -> true | Unsat | Unknown -> false

let is_unsat (result : Solver.result) =
  match result with Unsat -> true | Sat | Unknown -> false

module type Config = sig
  val fuel : Fuel_gauge.t
end

module type Base = sig
  module Value : Value.S
  module MONAD : Monad.Base

  type 'a v := 'a Value.t
  type 'a vt := 'a Value.ty
  type sbool := Value.sbool

  val assume : sbool v list -> unit MONAD.t
  val vanish : unit -> 'a MONAD.t
  val assert_ : sbool v -> bool MONAD.t
  val assert_ox : sbool v -> bool MONAD.t
  val nondet : ?constrs:('a v -> sbool v list) -> 'a vt -> 'a v MONAD.t
  val fresh_var : 'a vt -> Var.t MONAD.t

  val branch_on :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a MONAD.t) ->
    else_:(unit -> 'a MONAD.t) ->
    'a MONAD.t

  (** Branches on value, and takes at most one branch, starting with the [then]
      branch. This means that if the [then_] branch is SAT, it is taken and the
      [else_] branch is ignored, otherwise the [else_] branch is taken. This is,
      of course, UX-sound, but not OX-sound. *)
  val branch_on_take_one :
    ?left_branch_name:string ->
    ?right_branch_name:string ->
    sbool v ->
    then_:(unit -> 'a MONAD.t) ->
    else_:(unit -> 'a MONAD.t) ->
    'a MONAD.t

  val branches : (unit -> 'a MONAD.t) list -> 'a MONAD.t

  (** {2 Fuel} *)

  val consume_fuel_steps : int -> unit MONAD.t
  val set_default_fuel : Fuel_gauge.t -> unit

  (** [run] p actually performs symbolic execution and returns a list of
      obtained branches which capture the outcome together with a path condition
      that is a list of boolean symbolic values *)
  val run : 'a MONAD.t -> ('a * sbool v list) list
end

module type S = sig
  include Base
  include Monad.Base with type 'a t = 'a MONAD.t

  val all : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_list : ('a, 'b) Monad.FoldM(MONAD)(Foldable.List).folder
  val fold_iter : ('a, 'b) Monad.FoldM(MONAD)(Foldable.Iter).folder
  val fold_seq : ('a, 'b) Monad.FoldM(MONAD)(Foldable.Seq).folder

  module rec Result : sig
    type ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) Compo_res.t MONAD.t

    val ok : 'ok -> ('ok, 'err, 'fix) t
    val error : 'err -> ('ok, 'err, 'fix) t
    val miss : 'fix -> ('ok, 'err, 'fix) t

    val bind :
      ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

    val map : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t

    val bind_2 :
      ('ok, 'err, 'fix) t ->
      f:('ok -> ('a, 'b, 'fix) t) ->
      fe:('err -> ('a, 'b, 'fix) t) ->
      ('a, 'b, 'fix) t

    val map_error : ('ok, 'err, 'fix) t -> ('err -> 'a) -> ('ok, 'a, 'fix) t

    val bind_missing :
      ('ok, 'err, 'fix) t -> ('fix -> ('ok, 'err, 'a) t) -> ('ok, 'err, 'a) t

    val map_missing : ('ok, 'err, 'fix) t -> ('fix -> 'a) -> ('ok, 'err, 'a) t

    val fold_list :
      ('elem, 'a, 'b, 'c) Monad.FoldM3(Result)(Foldable.List).folder

    val fold_iter :
      ('elem, 'a, 'b, 'c) Monad.FoldM3(Result)(Foldable.Iter).folder

    val fold_seq : ('elem, 'a, 'b, 'c) Monad.FoldM3(Result)(Foldable.Seq).folder
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

    val ( let*? ) :
      ('a, 'b, 'c) Result.t ->
      ('c -> ('a, 'b, 'd) Result.t) ->
      ('a, 'b, 'd) Result.t

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

  let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return ~fold x ~init ~f
  let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
  let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
  let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f

  module Result = struct
    include Compo_res.T (MONAD)

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
    let ( let*? ) = Result.bind_missing
    let ( let+? ) = Result.map_missing

    module Symex_syntax = struct
      let branch_on = branch_on
      let branch_on_take_one = branch_on_take_one
    end
  end
end

module Make_seq (C : Config) (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value = Extend (struct
  module Solver = Solver.Mutable_to_in_place (Sol)

  module Fuel = struct
    include Reversible.Make_in_place (struct
      include Fuel_gauge

      let default = C.fuel
    end)

    let consume_branching n = wrap (Fuel_gauge.consume_branching n) ()
    let consume_fuel_steps n = wrap (Fuel_gauge.consume_fuel_steps n) ()
    let branching_left = wrap_read Fuel_gauge.branching_left
  end

  module Value = Solver.Value
  module MONAD = Monad.SeqM

  module Symex_state : Reversible.In_place = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel.backtrack_n n

    let save () =
      Solver.save ();
      Fuel.save ()

    let reset () =
      Solver.reset ();
      Fuel.reset ()
  end

  let consume_fuel_steps n () =
    match Fuel.consume_fuel_steps n with
    | Exhausted -> Seq.Nil
    | Not_exhausted -> Seq.Cons ((), Seq.empty)

  let set_default_fuel = Fuel.set_default

  let assume learned () =
    let rec aux acc learned =
      match learned with
      | [] ->
          Solver.add_constraints acc;
          Seq.Cons ((), Seq.empty)
      | l :: ls -> (
          let l = Solver.simplify l in
          match Value.as_bool l with
          | Some true -> aux acc ls
          | Some false -> Nil
          | None -> aux (l :: acc) ls)
    in
    aux [] learned

  (** Assert is [if%sat (not value) then error else ok]. In UX, assert only
      returns false if (not value) is *really* satisfiable. *)
  let assert_ value () =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> Seq.Cons (true, Seq.empty)
    | Some false -> Seq.Cons (false, Seq.empty)
    | None ->
        Symex_state.save ();
        Solver.add_constraints [ Value.(not value) ];
        let sat = is_sat (Solver.sat ()) in
        Symex_state.backtrack_n 1;
        Seq.Cons (not sat, Seq.empty)

  (** TODO: is this correct? *)
  let assert_ox value () =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> Seq.Cons (true, Seq.empty)
    | Some false -> Seq.Cons (false, Seq.empty)
    | None ->
        Symex_state.save ();
        Solver.add_constraints [ Value.(not value) ];
        let unsat = is_unsat (Solver.sat ()) in
        Symex_state.backtrack_n 1;
        Seq.Cons (unsat, Seq.empty)

  let nondet ?constrs ty () =
    let v = Solver.fresh_var ty in
    let v = Value.mk_var v ty in
    let () =
      match constrs with
      | Some constrs -> Solver.add_constraints (constrs v)
      | None -> ()
    in
    Seq.Cons (v, Seq.empty)

  let fresh_var ty = Seq.return (Solver.fresh_var ty)

  let branch_on ?left_branch_name:_ ?right_branch_name:_ guard ~then_ ~else_ :
      'a Seq.t =
   fun () ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ () ()
    | Some false -> else_ () ()
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        let left_sat = Solver.sat () in
        Seq.append
          (fun () -> if is_sat left_sat then then_ () () else Seq.Nil)
          (fun () ->
            Symex_state.backtrack_n 1;
            Solver.add_constraints [ Value.(not guard) ];
            if is_unsat left_sat then
              (* Right must be sat since left was not! We didn't branch so we don't consume the counter. *)
              else_ () ()
            else
              match Fuel.consume_branching 1 with
              | Exhausted -> Seq.Nil
              | Not_exhausted ->
                  if is_sat (Solver.sat ()) then else_ () () else Seq.Nil)
          ()

  let branch_on_take_one ?left_branch_name:_ ?right_branch_name:_ guard ~then_
      ~else_ : 'a Seq.t =
   fun () ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    | Some true -> then_ () ()
    | Some false -> else_ () ()
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        let left_sat = is_sat (Solver.sat ()) in
        Seq.append
          (fun () -> if left_sat then then_ () () else Seq.Nil)
          (fun () ->
            Symex_state.backtrack_n 1;
            if left_sat then Seq.Nil
            else (
              Solver.add_constraints [ Value.(not guard) ];
              else_ () ()))
          ()

  let branches (brs : (unit -> 'a Seq.t) list) : 'a Seq.t =
   fun () ->
    let brs, count =
      Soteria_std.List.take_count (Fuel.branching_left () + 1) brs
    in
    let () =
      match Fuel.consume_branching (max (count - 1) 0) with
      | Not_exhausted -> ()
      | Exhausted -> failwith "Exhausted fuel? Unreachable"
    in
    match brs with
    | [] -> Seq.Nil
    | [ a ] -> a () ()
    | a :: (_ :: _ as r) ->
        (* First branch should not backtrack and last branch should not save *)
        let rec loop brs =
          match brs with
          | [ x ] ->
              fun () ->
                Symex_state.backtrack_n 1;
                x () ()
          | x :: r ->
              Seq.append
                (fun () ->
                  Symex_state.backtrack_n 1;
                  Symex_state.save ();
                  x () ())
                (loop r)
          | [] -> failwith "unreachable"
        in
        Symex_state.save ();
        Seq.append (fun () -> a () ()) (loop r) ()

  let vanish () = Seq.empty

  let[@tail_mod_cons] rec run seq =
    match seq () with
    | Seq.Nil -> []
    | Seq.Cons (x1, seq) -> (
        let pc1 = Solver.as_values () in
        match seq () with
        | Seq.Nil -> [ (x1, pc1) ]
        | Seq.Cons (x2, seq) ->
            let pc2 = Solver.as_values () in
            (x1, pc1) :: (x2, pc2) :: run seq)

  let run s =
    Symex_state.reset ();
    run s
end)

module Make_iter (C : Config) (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value = Extend (struct
  module Solver = Solver.Mutable_to_in_place (Sol)

  module Fuel = struct
    include Reversible.Make_in_place (struct
      include Fuel_gauge

      let default = C.fuel
    end)

    let consume_branching n = wrap (Fuel_gauge.consume_branching n) ()
    let consume_fuel_steps n = wrap (Fuel_gauge.consume_fuel_steps n) ()
    let branching_left = wrap_read Fuel_gauge.branching_left
  end

  module Value = Solver.Value
  module MONAD = Monad.IterM

  module Symex_state : Reversible.In_place = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel.backtrack_n n

    let save () =
      Solver.save ();
      Fuel.save ()

    let reset () =
      Solver.reset ();
      Fuel.reset ()
  end

  let consume_fuel_steps n f =
    match Fuel.consume_fuel_steps n with
    | Exhausted -> L.debug (fun m -> m "Exhausted step fuel")
    | Not_exhausted -> f ()

  let set_default_fuel = Fuel.set_default

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
          let sat = is_sat (Solver.sat ()) in
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
          let unsat = is_unsat (Solver.sat ()) in
          Symex_state.backtrack_n 1;
          unsat
        in
        f result

  let nondet ?(constrs = fun _ -> []) ty f =
    let v = Solver.fresh_var ty in
    let v = Value.mk_var v ty in
    Solver.add_constraints (constrs v);
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
            left_unsat := is_unsat sat_res;
            if is_sat sat_res then then_ () f
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
                  if is_sat (Solver.sat ()) then else_ () f
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
        if is_sat (Solver.sat ()) then then_ () f else left_sat := false;
        Symex_state.backtrack_n 1;
        if not !left_sat then (
          Solver.add_constraints [ Value.(not guard) ];
          else_ () f)

  let branches (brs : (unit -> 'a Iter.t) list) : 'a Iter.t =
   fun f ->
    let brs, count =
      Soteria_std.List.take_count (Fuel.branching_left () + 1) brs
    in
    let () =
      match Fuel.consume_branching (max (count - 1) 0) with
      | Not_exhausted -> ()
      | Exhausted -> failwith "Exhausted fuel? Unreachable"
    in
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

  let run iter =
    Symex_state.reset ();
    let l = ref [] in
    let () = iter @@ fun x -> l := (x, Solver.as_values ()) :: !l in
    List.rev !l

  let vanish () _f = ()
end)
