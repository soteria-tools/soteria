(** Monad interfaces and implementations. *)

(** Basic interface for a monad with one type parameter. *)
module type Base = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** Functor to create a monadic fold function over a foldable container. *)
module FoldM (M : Base) (F : Foldable.S) = struct
  type ('a, 'b) folder = 'a F.t -> init:'b -> f:('b -> 'a -> 'b M.t) -> 'b M.t
end

(** Generic monadic fold function. *)
let foldM ~return ~bind ~fold xs ~init ~f =
  fold xs ~init:(return init) ~f:(fun acc x -> bind (fun acc -> f acc x) acc)

(** Generic monadic map function that collects results. *)
let all ~return ~bind fn xs =
  let rec aux acc rs =
    match rs with
    | [] -> return (List.rev acc)
    | r :: rs -> bind (fun x -> aux (x :: acc) rs) (fn r)
  in
  aux [] xs

(** Syntax extension for monads (let* and let+). *)
module type Syntax = sig
  type 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

(** Complete monad interface including generic operations and syntax. *)
module type S = sig
  include Base

  val all : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_list : init:'a -> f:('a -> 'elem -> 'a t) -> 'elem list -> 'a t

  module Syntax : Syntax with type 'a t := 'a t
end

(** Functor to extend a basic monad with derived operations and syntax. *)
module Extend (Base : Base) = struct
  include Base

  (** Generic monadic map function that collects results. *)
  let all fn xs =
    let rec aux vs l =
      match l with
      | [] -> return (List.rev vs)
      | x :: xs -> bind (fun x -> aux (x :: vs) xs) (fn x)
    in
    aux [] xs

  (** Generic monadic fold function over a list. *)
  let fold_list ~init ~f xs =
    foldM ~return ~bind ~fold:Foldable.List.fold xs ~init ~f

  module Syntax = struct
    let ( let* ) x f = Base.bind f x
    let ( let+ ) x f = Base.map f x
  end
end

(** Basic interface for a monad with two type parameters (e.g. Result). *)
module type Base2 = sig
  type ('a, 'b) t

  val ok : 'a -> ('a, 'b) t
  val bind : ('a -> ('c, 'b) t) -> ('a, 'b) t -> ('c, 'b) t
  val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val error : 'b -> ('a, 'b) t
  val bind_error : ('b -> ('a, 'c) t) -> ('a, 'b) t -> ('a, 'c) t
  val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end

(** Functor to create a monadic fold for two-parameter monads. *)
module FoldM2 (M : Base2) (F : Foldable.S) = struct
  type ('elem, 'a, 'b) folder =
    'elem F.t -> init:'a -> f:('a -> 'elem -> ('a, 'b) M.t) -> ('a, 'b) M.t
end

(** Interface for a type with three type parameters. *)
module type Ty3 = sig
  type ('a, 'b, 'c) t
end

(** Functor to create a monadic fold for three-parameter monads. *)
module FoldM3 (M : Ty3) (F : Foldable.S) = struct
  type ('elem, 'a, 'b, 'c) folder =
    'elem F.t ->
    init:'a ->
    f:('a -> 'elem -> ('a, 'b, 'c) M.t) ->
    ('a, 'b, 'c) M.t
end

(** Syntax extension for two-parameter monads. *)
module type Syntax2 = sig
  type ('a, 'b) t

  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val ( let/ ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val ( let- ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
end

(** Complete interface for two-parameter monads. *)
module type S2 = sig
  include Base2

  val fold_list :
    init:'a -> f:('a -> 'elem -> ('a, 'b) t) -> 'elem list -> ('a, 'b) t

  val all : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t

  module Syntax : Syntax2 with type ('a, 'b) t := ('a, 'b) t
end

(** Functor to extend a basic two-parameter monad. *)
module Extend2 (Base : Base2) : S2 with type ('a, 'b) t = ('a, 'b) Base.t =
struct
  include Base

  (** Generic monadic map function that collects results. *)
  let all fn xs =
    let rec aux vs l =
      match l with
      | [] -> ok (List.rev vs)
      | x :: xs -> bind (fun x -> aux (x :: vs) xs) (fn x)
    in
    aux [] xs

  (** Generic monadic fold function over a list. *)
  let fold_list ~init ~f xs =
    foldM ~return:ok ~bind ~fold:Foldable.List.fold xs ~init ~f

  module Syntax = struct
    let ( let* ) x f = Base.bind f x
    let ( let+ ) x f = Base.map f x
    let ( let/ ) x f = Base.bind_error f x
    let ( let- ) x f = Base.map_error f x
  end
end

(** Identity monad. *)
module Id = Extend (struct
  type 'a t = 'a

  let return = Fun.id
  let bind = ( @@ )
  let map = ( @@ )
end)

(** Result transformer. *)
module ResultT (M : Base) : Base2 with type ('a, 'b) t = ('a, 'b) Result.t M.t =
struct
  type ('a, 'b) t = ('a, 'b) Result.t M.t

  let ok x = M.return (Ok x)
  let error x = M.return (Error x)
  let bind f = M.bind (function Ok x -> f x | Error z -> M.return (Error z))

  let bind_error f =
    M.bind (function Ok x -> M.return (Ok x) | Error z -> f z)

  let map f = M.map (Result.map f)
  let map_error f = M.map (Result.map_error f)
end

(** List monad. *)
module ListM = Extend (List)

(** Result monad. *)
module ResultM = Extend2 (Result)

(** Option monad. *)
module OptionM = Extend (Option)

(** Sequence monad. *)
module SeqM = Extend (Seq)

(** Iterator monad. *)
module IterM = Extend (Iter_soteria)

(** State monad. *)
module StateM (State : sig
  type t
end) =
Extend (struct
  type 'a t = State.t -> 'a * State.t

  let[@inline] return x s = (x, s)

  let[@inline] bind f x s =
    let x', s' = x s in
    f x' s'

  let[@inline] map f x s =
    let x', s' = x s in
    (f x', s')
end)

module StateT_base
    (State : sig
      type t
    end)
    (M : Base) =
struct
  type 'a t = State.t -> ('a * State.t) M.t

  let[@inline] return x s = M.return (x, s)
  let[@inline] bind f x s = M.bind (fun (x', s') -> f x' s') (x s)
  let[@inline] map f x s = M.map (fun (x', s') -> (f x', s')) (x s)
  let[@inline] get_state () : State.t t = fun s -> M.return (s, s)
  let[@inline] set_state (s : State.t) : unit t = fun _ -> M.return ((), s)

  let[@inline] map_state (f : State.t -> State.t) : unit t =
   fun s -> M.return ((), f s)

  let[@inline] run_with_state ~(state : State.t) (x : 'a t) : ('a * State.t) M.t
      =
    x state

  let[@inline] with_state ~(state : State.t) (x : 'a t) : 'a t =
   fun st -> M.map (fun (res, _) -> (res, st)) (x state)

  let[@inline] lift (m : 'a M.t) : 'a t = fun s -> M.map (fun x -> (x, s)) m
end

module StateT
    (State : sig
      type t
    end)
    (M : Base) =
  Extend (StateT_base (State) (M))

module StateT_p (M : Base) = struct
  type ('a, 'state) t = 'state -> ('a * 'state) M.t

  let[@inline] return x s = M.return (x, s)
  let[@inline] bind f x s = M.bind (fun (x', s') -> f x' s') (x s)
  let[@inline] map f x s = M.map (fun (x', s') -> (f x', s')) (x s)
  let[@inline] get_state () : ('state, 'state) t = fun s -> M.return (s, s)

  let[@inline] set_state (s : 'state) : (unit, 'state) t =
   fun _ -> M.return ((), s)

  let[@inline] map_state (f : 'state -> 'state) : (unit, 'state) t =
   fun s -> M.return ((), f s)

  let[@inline] lift (m : 'a M.t) : ('a, 'state) t =
   fun s -> M.map (fun x -> (x, s)) m

  module Syntax = struct
    let ( let* ) x f = bind f x
    let ( let+ ) x f = map f x
    let ( let*^ ) x f = bind f (lift x)
    let ( let+^ ) x f = map f (lift x)
  end
end
