module type S = sig
  module Value : Value.S

  type 'a t

  val return : ?learned:Value.t list -> 'a -> 'a t
  val vanish : unit -> 'a t
  val nondet : Value.ty -> Value.t t
  val must : Value.t list -> (unit, string) result t
  val value_eq : Value.t -> Value.t -> Value.t
  val branch_on : Value.t -> then_:'a t -> else_:'a t -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val force : 'a t -> 'a list
  val all : 'a t list -> 'a list t
  val abort : unit -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  (** Forces the sequence, providing an iterator through the outcomes *)

  module Result : sig
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    val ok : ?learned:Value.t list -> 'a -> ('a, 'b) t
    val error : ?learned:Value.t list -> 'b -> ('a, 'b) t
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  end

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let** ) :
      ('a, 'b) Result.t -> ('a -> ('c, 'b) Result.t) -> ('c, 'b) Result.t

    val ( let++ ) : ('a, 'c) Result.t -> ('a -> 'b) -> ('b, 'c) Result.t

    module Symex_syntax : sig
      val branch_on : Value.t -> then_:'a t -> else_:'a t -> 'a t
    end
  end
end

type 'a t = 'a Seq.t

module M (Solver : Solver.S) : S with module Value = Solver.Value = struct
  module Value = Solver.Value

  type nonrec 'a t = 'a t

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

  let nondet ty = Seq.return (Solver.fresh ty)
  let value_eq x y = Value.sem_eq x y

  let branch_on guard ~then_ ~else_ =
    match Solver.simplified_bool guard with
    | Some true -> then_
    | Some false -> else_
    | None ->
        Seq.append
          (fun () ->
            Solver.save ();
            Solver.add_constraints [ guard ];
            if Solver.delayed_sat () then then_ () else Seq.empty ())
          (fun () ->
            Solver.backtrack ();
            Solver.add_constraints [ Value.(not guard) ];
            if Solver.delayed_sat () then else_ () else Seq.empty ())

  let bind x f = Seq.concat_map f x
  let map = Seq.map
  let force = Stdlib.List.of_seq
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

  (* let mapM f xs =
     let rec aux acc rs =
       match rs with
       | [] -> return (List.rev acc)
       | r :: rs -> bind r @@ fun x -> aux (f x :: acc) rs
     in
     aux [] xs *)

  (* let foldM f init xs =
     let rec aux acc rs =
       match rs with [] -> acc | r :: rs -> bind r @@ fun x -> aux (f acc x) rs
     in
     aux (f init) xs *)

  module Result = struct
    type nonrec ('a, 'b) t = ('a, 'b) Result.t t

    let ok ?learned x = return ?learned (Ok x)
    let error ?learned x = return ?learned (Error x)
    let bind x f = bind x (function Ok x -> f x | Error z -> return (Error z))
    let map f x = map (Result.map ~f) x
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) x f = map f x
    let ( let** ) = Result.bind
    let ( let++ ) x f = Result.map f x

    module Symex_syntax = struct
      let branch_on = branch_on
    end
  end
end
