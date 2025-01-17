module List = ListLabels

module type Base = sig
  module Value : Value.S
  module MONAD : Monad.Base

  val assume : Value.t list -> unit MONAD.t
  val vanish : unit -> 'a MONAD.t
  val assert_ : Value.t -> bool MONAD.t
  val nondet : ?constrs:(Value.t -> Value.t list) -> Value.ty -> Value.t MONAD.t
  val fresh_var : Value.ty -> Var.t MONAD.t
  val batched : (unit -> 'a MONAD.t) -> 'a MONAD.t

  val branch_on :
    Value.t ->
    then_:(unit -> 'a MONAD.t) ->
    else_:(unit -> 'a MONAD.t) ->
    'a MONAD.t

  (** Branches on value, and takes at most one branch, starting with the [then]
      branch. This means that if the [then_] branch is SAT, it is taken and the
      [else_] branch is ignored, otherwise the [else_] branch is taken. This is,
      of course, UX-sound, but not OX-sound. *)
  val branch_on_take_one :
    Value.t ->
    then_:(unit -> 'a MONAD.t) ->
    else_:(unit -> 'a MONAD.t) ->
    'a MONAD.t

  val branches : (unit -> 'a MONAD.t) list -> 'a MONAD.t

  (** {2 Fuel} *)

  val consume_fuel_steps : int -> unit MONAD.t

  (** [run] p actually performs symbolic execution and returns a list of
      obtained branches which capture the outcome together with a path condition
      that is a list of boolean symbolic values *)
  val run : 'a MONAD.t -> ('a * Value.t list) list
end

module type S = sig
  include Base
  include Monad.Base with type 'a t = 'a MONAD.t

  val all : 'a t list -> 'a list t
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

    val bind_error :
      ('ok, 'err, 'fix) t -> ('err -> ('ok, 'a, 'fix) t) -> ('ok, 'a, 'fix) t

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
      val branch_on :
        Value.t -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t

      val branch_on_take_one :
        Value.t -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t
    end
  end
end

module Extend (Base : Base) = struct
  include Base
  include MONAD

  let all xs =
    let rec aux acc rs =
      match rs with
      | [] -> return (List.rev acc)
      | r :: rs -> bind r @@ fun x -> aux (x :: acc) rs
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

module Make_seq (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value = Extend (struct
  module Solver = Solver.Mutable_to_in_place (Sol)
  module Fuel_in_place = Incremental.Mutable_to_in_place (Fuel)
  module Value = Solver.Value
  module MONAD = Monad.SeqM

  module Symex_state : Incremental.In_place = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel_in_place.backtrack_n n

    let save () =
      Solver.save ();
      Fuel_in_place.save ()

    let reset () =
      Solver.reset ();
      Fuel_in_place.reset ()
  end

  let consume_fuel_steps n () =
    match Fuel_in_place.wrap (Fuel.consume_fuel_steps n) with
    | Fuel.Exhausted -> Seq.Nil
    | Fuel.Not_exhausted -> Seq.Cons ((), Seq.empty)

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

  let assert_ value () =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> Seq.Cons (true, Seq.empty)
    | Some false -> Seq.Cons (false, Seq.empty)
    | None ->
        Symex_state.save ();
        Solver.add_constraints [ Value.(not value) ];
        let sat = Solver.sat () in
        Symex_state.backtrack_n 1;
        Seq.Cons (not sat, Seq.empty)

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

  let branch_on guard ~then_ ~else_ : 'a Seq.t =
   fun () ->
    let guard = Solver.simplify guard in
    let left_sat = ref true in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ () ()
    | Some false -> else_ () ()
    | None ->
        Seq.append
          (fun () ->
            Symex_state.save ();
            Solver.add_constraints ~simplified:true [ guard ];
            if Solver.sat () then then_ () ()
            else (
              left_sat := false;
              Seq.empty ()))
          (fun () ->
            Symex_state.backtrack_n 1;
            Solver.add_constraints [ Value.(not guard) ];
            if !left_sat then
              (* We have to check right *)
              if Solver.sat () then
                match Fuel_in_place.wrap (Fuel.consume_branching 1) with
                | Exhausted -> Seq.empty ()
                | Not_exhausted -> else_ () ()
              else Seq.empty ()
            else (* Right must be sat since left was not! *)
              else_ () ())
          ()

  let branch_on_take_one guard ~then_ ~else_ : 'a Seq.t =
   fun () ->
    let guard = Solver.simplify guard in
    match Value.as_bool guard with
    | Some true -> then_ () ()
    | Some false -> else_ () ()
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        if Solver.sat () then then_ () ()
        else (
          Symex_state.backtrack_n 1;
          Solver.add_constraints [ Value.(not guard) ];
          else_ () ())

  let branches (brs : (unit -> 'a Seq.t) list) () =
    let brs = List.take (Fuel_in_place.wrap (Fuel.branching_left ()) + 1) brs in
    let () =
      match
        Fuel_in_place.wrap (Fuel.consume_branching (List.length brs - 1))
      with
      | Fuel.Not_exhausted -> ()
      | Fuel.Exhausted -> failwith "Exhausted fuel? Unreachable"
    in
    match brs with
    | [] -> Seq.Nil
    | [ a ] -> a () ()
    (* Optimised case *)
    | [ a; b ] ->
        Seq.append
          (fun () ->
            Solver.save ();
            a () ())
          (fun () ->
            Symex_state.backtrack_n 1;
            b () ())
          ()
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
        Seq.append
          (fun () ->
            Symex_state.save ();
            a () ())
          (loop r) ()

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
    Solver.reset ();
    run s

  let batched s =
    MONAD.bind (s ()) @@ fun x ->
    if Solver.sat () then MONAD.return x else vanish ()
end)

module Make_iter (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value = Extend (struct
  module Solver = Solver.Mutable_to_in_place (Sol)
  module Fuel_in_place = Incremental.Mutable_to_in_place (Fuel)
  module Value = Solver.Value
  module MONAD = Monad.IterM

  module Symex_state : Incremental.In_place = struct
    let backtrack_n n =
      Solver.backtrack_n n;
      Fuel_in_place.backtrack_n n

    let save () =
      Solver.save ();
      Fuel_in_place.save ()

    let reset () =
      Solver.reset ();
      Fuel_in_place.reset ()
  end

  let consume_fuel_steps n f =
    match Fuel_in_place.wrap (Fuel.consume_fuel_steps n) with
    | Fuel.Exhausted -> Logging.debug (fun m -> m "Exhausted step fuel")
    | Fuel.Not_exhausted -> f ()

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
          | Some false -> ()
          | None -> aux (l :: acc) ls)
    in
    aux [] learned

  let assert_ value f =
    let value = Solver.simplify value in
    match Value.as_bool value with
    | Some true -> f true
    | Some false -> f false
    | None ->
        Symex_state.save ();
        Solver.add_constraints [ Value.(not value) ];
        let sat = Solver.sat () in
        Solver.backtrack_n 1;
        f (not sat)

  let nondet ?(constrs = fun _ -> []) ty f =
    let v = Solver.fresh_var ty in
    let v = Value.mk_var v ty in
    Solver.add_constraints (constrs v);
    f v

  let fresh_var ty f = f (Solver.fresh_var ty)

  let branch_on guard ~(then_ : unit -> 'a Iter.t) ~(else_ : unit -> 'a Iter.t)
      : 'a Iter.t =
   fun f ->
    let guard = Solver.simplify guard in
    let left_sat = ref true in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        if Solver.sat () then then_ () f else left_sat := false;
        Solver.backtrack_n 1;
        Solver.add_constraints [ Value.(not guard) ];
        if !left_sat then (
          if
            (* We have to check right *)
            Solver.sat ()
          then
            match Fuel_in_place.wrap (Fuel.consume_branching 1) with
            | Exhausted -> Logging.debug (fun m -> m "Exhausted branching fuel")
            | Not_exhausted -> else_ () f)
        else (* Right must be sat since left was not! *)
          else_ () f

  let branch_on_take_one guard ~then_ ~else_ : 'a Iter.t =
   fun f ->
    let guard = Solver.simplify guard in

    match Value.as_bool guard with
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        Symex_state.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        if Solver.sat () then then_ () f
        else (
          Symex_state.backtrack_n 1;
          Solver.add_constraints [ Value.(not guard) ];
          else_ () f)

  let batched s =
    Iter.flat_map
      (fun x -> if Solver.sat () then Iter.return x else Iter.empty)
      (s ())

  let branches (brs : (unit -> 'a Iter.t) list) : 'a Iter.t =
   fun f ->
    let brs = List.take (Fuel_in_place.wrap (Fuel.branching_left ()) + 1) brs in
    let () =
      match
        Fuel_in_place.wrap (Fuel.consume_branching (List.length brs - 1))
      with
      | Fuel.Not_exhausted -> ()
      | Fuel.Exhausted -> failwith "Exhausted fuel? Unreachable"
    in
    match brs with
    | [] -> ()
    | [ a ] -> a () f
    (* Optimised case *)
    | [ a; b ] ->
        Symex_state.save ();
        a () f;
        Symex_state.backtrack_n 1;
        b () f
    | a :: (_ :: _ as r) ->
        (* First branch should not backtrack and last branch should not save *)
        let rec loop brs =
          match brs with
          | [ x ] ->
              Symex_state.backtrack_n 1;
              x () f
          | x :: r ->
              Symex_state.backtrack_n 1;
              Symex_state.save ();
              x () f;
              loop r
          | [] -> failwith "unreachable"
        in
        Symex_state.save ();
        a () f;
        loop r

  let run iter =
    Symex_state.reset ();
    let l = ref [] in
    (iter @@ fun x -> l := (x, Solver.as_values ()) :: !l);
    List.rev !l

  let vanish () _f = ()
end)
