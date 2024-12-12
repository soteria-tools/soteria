module List = ListLabels

module type S = sig
  module Value : Value.S

  type 'a t

  val return : 'a -> 'a t
  val assume : Value.t list -> unit t
  val vanish : unit -> 'a t
  val nondet : ?constrs:(Value.t -> Value.t list) -> Value.ty -> Value.t t
  val fresh_var : Value.ty -> Var.t t
  val batched : (unit -> 'a t) -> 'a t

  val branch_on :
    Value.t -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t

  val branches : (unit -> 'a t) list -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [run] p actually performs symbolic execution and returns a list of obtained branches
      which capture the outcome together with a path condition that is a list of boolean symbolic values *)
  val run : 'a t -> ('a * Value.t list) list

  val all : 'a t list -> 'a list t
  val fold_left : 'a list -> init:'acc -> f:('acc -> 'a -> 'acc t) -> 'acc t
  val fold_seq : 'a Seq.t -> init:'acc -> f:('acc -> 'a -> 'acc t) -> 'acc t
  val fold_iter : 'a Iter.t -> init:'acc -> f:('acc -> 'a -> 'acc t) -> 'acc t

  module Result : sig
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    val ok : 'a -> ('a, 'b) t
    val error : 'b -> ('a, 'b) t
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
    val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

    val fold_left :
      'a list -> init:'acc -> f:('acc -> 'a -> ('acc, 'b) t) -> ('acc, 'b) t

    val fold_seq :
      'a Seq.t -> init:'acc -> f:('acc -> 'a -> ('acc, 'b) t) -> ('acc, 'b) t
  end

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let** ) :
      ('a, 'b) Result.t -> ('a -> ('c, 'b) Result.t) -> ('c, 'b) Result.t

    val ( let++ ) : ('a, 'c) Result.t -> ('a -> 'b) -> ('b, 'c) Result.t
    val ( let+- ) : ('a, 'b) Result.t -> ('b -> 'c) -> ('a, 'c) Result.t

    module Symex_syntax : sig
      val branch_on :
        Value.t -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t
    end
  end
end

(* module Extend (Base : Base) :
   S with module Value = Base.Value and type 'a t = 'a Base.t = struct
   end *)

module Make_seq (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value = struct
  module Solver = Solver.Mutable_to_in_place (Sol)
  module Value = Solver.Value

  type 'a t = 'a Seq.t

  let return x = Seq.return x

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

  let branch_on guard ~then_ ~else_ =
    let guard = Solver.simplify guard in
    let left_sat = ref true in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ ()
    | Some false -> else_ ()
    | None ->
        Seq.append
          (fun () ->
            Solver.save ();
            Solver.add_constraints ~simplified:true [ guard ];
            if Solver.sat () then then_ () ()
            else (
              left_sat := false;
              Seq.empty ()))
          (fun () ->
            Solver.backtrack_n 1;
            Solver.add_constraints [ Value.(not guard) ];
            if !left_sat then
              (* We have to check right *)
              if Solver.sat () then else_ () () else Seq.empty ()
            else (* Right must be sat since left was not! *)
              else_ () ())

  let branches (brs : (unit -> 'a t) list) : 'a t =
    match brs with
    | [] -> Seq.empty
    | [ a ] -> a ()
    (* Optimised case *)
    | [ a; b ] ->
        Seq.append
          (fun () ->
            Solver.save ();
            a () ())
          (fun () ->
            Solver.backtrack_n 1;
            b () ())
    | a :: (_ :: _ as r) ->
        (* First branch should not backtrack and last branch should not save *)
        let rec loop brs =
          match brs with
          | [ x ] ->
              fun () ->
                Solver.backtrack_n 1;
                x () ()
          | x :: r ->
              Seq.append
                (fun () ->
                  Solver.backtrack_n 1;
                  Solver.save ();
                  x () ())
                (loop r)
          | [] -> failwith "unreachable"
        in
        Seq.append
          (fun () ->
            Solver.save ();
            a () ())
          (loop r)

  let bind x f = Seq.concat_map f x
  let map = Seq.map
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
    bind (s ()) @@ fun x -> if Solver.sat () then return x else vanish ()

  let all xs =
    let rec aux acc rs =
      match rs with
      | [] -> return (List.rev acc)
      | r :: rs -> bind r @@ fun x -> aux (x :: acc) rs
    in
    aux [] xs

  let fold_left xs ~init ~f =
    List.fold_left xs ~init:(return init) ~f:(fun acc x ->
        bind acc @@ fun acc -> f acc x)

  let fold_seq xs ~init ~f =
    Seq.fold_left (fun acc x -> bind acc @@ fun acc -> f acc x) (return init) xs

  let fold_iter xs ~init ~f =
    Iter.fold (fun acc x -> bind acc @@ fun acc -> f acc x) (return init) xs

  module Result = struct
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    let ok x = return (Ok x)
    let error x = return (Error x)
    let bind x f = bind x (function Ok x -> f x | Error z -> return (Error z))
    let map_error f x = map (Result.map_error f) x
    let map f x = map (Result.map f) x

    let fold_left xs ~init ~f =
      List.fold_left xs ~init:(ok init) ~f:(fun acc x ->
          bind acc @@ fun acc -> f acc x)

    let fold_seq xs ~init ~f =
      Seq.fold_left (fun acc x -> bind acc @@ fun acc -> f acc x) (ok init) xs
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) x f = map f x
    let ( let** ) = Result.bind
    let ( let++ ) x f = Result.map f x
    let ( let+- ) x f = Result.map_error f x

    module Symex_syntax = struct
      let branch_on = branch_on
    end
  end
end

module Make_iter (Sol : Solver.Mutable_incremental) :
  S with module Value = Sol.Value = struct
  module Solver = Solver.Mutable_to_in_place (Sol)
  module Value = Solver.Value

  type 'a t = 'a Iter.t

  let return x f = f x

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

  let nondet ?(constrs = fun _ -> []) ty f =
    let v = Solver.fresh_var ty in
    let v = Value.mk_var v ty in
    Solver.add_constraints (constrs v);
    f v

  let fresh_var ty f = f (Solver.fresh_var ty)

  let branch_on guard ~(then_ : unit -> 'a t) ~(else_ : unit -> 'a t) : 'a t =
   fun f ->
    let guard = Solver.simplify guard in
    let left_sat = ref true in
    match Value.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ () f
    | Some false -> else_ () f
    | None ->
        Solver.save ();
        Solver.add_constraints ~simplified:true [ guard ];
        if Solver.sat () then then_ () f else left_sat := false;
        Solver.backtrack_n 1;
        Solver.add_constraints [ Value.(not guard) ];
        if !left_sat then (
          if (* We have to check right *)
             Solver.sat () then else_ () f)
        else (* Right must be sat since left was not! *)
          else_ () f

  let batched s =
    Iter.flat_map
      (fun x -> if Solver.sat () then Iter.return x else Iter.empty)
      (s ())

  let branches (brs : (unit -> 'a t) list) : 'a t =
   fun f ->
    match brs with
    | [] -> ()
    | [ a ] -> a () f
    (* Optimised case *)
    | [ a; b ] ->
        Solver.save ();
        a () f;
        Solver.backtrack_n 1;
        b () f
    | a :: (_ :: _ as r) ->
        (* First branch should not backtrack and last branch should not save *)
        let rec loop brs =
          match brs with
          | [ x ] ->
              Solver.backtrack_n 1;
              x () f
          | x :: r ->
              Solver.backtrack_n 1;
              Solver.save ();
              x () f;
              loop r
          | [] -> failwith "unreachable"
        in
        Solver.save ();
        a () f;
        loop r

  let bind x f = Iter.flat_map f x
  let map = Iter.map

  let run iter =
    Solver.reset ();
    let l = ref [] in
    (iter @@ fun x -> l := (x, Solver.as_values ()) :: !l);
    List.rev !l

  let vanish () _f = ()

  let all xs =
    let rec aux acc rs =
      match rs with
      | [] -> return (List.rev acc)
      | r :: rs -> bind r @@ fun x -> aux (x :: acc) rs
    in
    aux [] xs

  let fold_left xs ~init ~f =
    List.fold_left xs ~init:(return init) ~f:(fun acc x ->
        bind acc @@ fun acc -> f acc x)

  let fold_seq xs ~init ~f =
    Seq.fold_left (fun acc x -> bind acc @@ fun acc -> f acc x) (return init) xs

  let fold_iter xs ~init ~f =
    Iter.fold (fun acc x -> bind acc @@ fun acc -> f acc x) (return init) xs

  module Result = struct
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    let ok x = return (Ok x)
    let error x = return (Error x)
    let bind x f = bind x (function Ok x -> f x | Error z -> return (Error z))
    let map_error f x = map (Result.map_error f) x
    let map f x = map (Result.map f) x

    let fold_left xs ~init ~f =
      List.fold_left xs ~init:(ok init) ~f:(fun acc x ->
          bind acc @@ fun acc -> f acc x)

    let fold_seq xs ~init ~f =
      Seq.fold_left (fun acc x -> bind acc @@ fun acc -> f acc x) (ok init) xs
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) x f = map f x
    let ( let** ) = Result.bind
    let ( let++ ) x f = Result.map f x
    let ( let+- ) x f = Result.map_error f x

    module Symex_syntax = struct
      let branch_on = branch_on
    end
  end
end
