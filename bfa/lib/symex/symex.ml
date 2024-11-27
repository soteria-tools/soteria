module List = ListLabels

module type S = sig
  module Value : Value.S

  type 'a t

  val return : ?learned:Value.t list -> 'a -> 'a t
  val vanish : unit -> 'a t
  val nondet : ?constrs:(Value.t -> Value.t list) -> Value.ty -> Value.t t
  val must : Value.t list -> (unit, string) Result.t t
  val value_eq : Value.t -> Value.t -> Value.t

  val branch_on :
    Value.t -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t

  val branches : (unit -> 'a t) list -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a list
  val all : 'a t list -> 'a list t
  val abort : unit -> 'a t
  val fold_left : 'a list -> init:'acc -> f:('acc -> 'a -> 'acc t) -> 'acc t

  (** Careful, this consumes the symbolic execution! *)
  val iter : ('a -> unit) -> 'a t -> unit

  module Result : sig
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    val ok : ?learned:Value.t list -> 'a -> ('a, 'b) t
    val error : ?learned:Value.t list -> 'b -> ('a, 'b) t
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
    val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

    val fold_left :
      'a list -> init:'acc -> f:('acc -> 'a -> ('acc, 'b) t) -> ('acc, 'b) t
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

module M (Solver : Solver.S) : S with module Value = Solver.Value = struct
  module Value = Solver.Value

  type 'a t = 'a Seq.t

  let return ?learned x () =
    Solver.add_constraints (Option.value ~default:[] learned);
    Seq.Cons (x, Seq.empty)

  let must constraints () =
    let res =
      if Solver.check_entailment constraints then Ok ()
      else
        Fmt.error "must failed: %a"
          (Fmt.list ~sep:(Fmt.any "/\\") Value.pp)
          constraints
    in
    Seq.Cons (res, Seq.empty)

  let nondet ?constrs ty =
    let v = Solver.fresh ty in
    let () =
      match constrs with
      | Some constrs -> Solver.add_constraints (constrs v)
      | None -> ()
    in
    Seq.return v

  let value_eq x y = Value.sem_eq x y

  let branch_on guard ~then_ ~else_ =
    let guard = Solver.simplify guard in
    match Solver.as_bool guard with
    (* [then_] and [else_] could be ['a t] instead of [unit -> 'a t],
       if we remove the Some true and Some false optimisation. *)
    | Some true -> then_ ()
    | Some false -> else_ ()
    | None ->
        Seq.append
          (fun () ->
            Solver.save ();
            Solver.add_constraints ~simplified:true [ guard ];
            if Solver.sat () then then_ () () else Seq.empty ())
          (fun () ->
            Solver.backtrack ();
            Solver.add_constraints [ Value.(not guard) ];
            if Solver.sat () then else_ () () else Seq.empty ())

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
            Solver.backtrack ();
            b () ())
    | a :: (_ :: _ as r) ->
        (* First branch should not backtrack and last branch should not save *)
        let rec loop brs =
          match brs with
          | [ x ] ->
              fun () ->
                Solver.backtrack ();
                x () ()
          | x :: r ->
              Seq.append
                (fun () ->
                  Solver.backtrack ();
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
  let run = Stdlib.List.of_seq
  let iter = Seq.iter
  let vanish () = Seq.empty

  (* Under-approximating behaviour *)
  let abort () =
    Logs.debug (fun m -> m "Aborting execution here");
    vanish ()

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

  module Result = struct
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    let ok ?learned x = return ?learned (Ok x)
    let error ?learned x = return ?learned (Error x)
    let bind x f = bind x (function Ok x -> f x | Error z -> return (Error z))
    let map_error f x = map (Result.map_error f) x
    let map f x = map (Result.map f) x

    let fold_left xs ~init ~f =
      List.fold_left xs ~init:(ok init) ~f:(fun acc x ->
          bind acc @@ fun acc -> f acc x)
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
