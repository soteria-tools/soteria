(** Monad interfaces and implementations. *)

(** Basic interface for a monad with one type parameter. *)
module type Base = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
end

(** Functor to create a monadic fold function over a foldable container. *)
module FoldM (M : Base) (F : Foldable.S) = struct
  type ('a, 'b) folder = 'a F.t -> init:'b -> f:('b -> 'a -> 'b M.t) -> 'b M.t
end

(** Generic monadic fold function. *)
let foldM ~return ~bind ~fold xs ~init ~f =
  fold xs ~init:(return init) ~f:(fun acc x -> bind acc @@ fun acc -> f acc x)

(** Generic monadic map function that collects results. *)
let all ~return ~bind fn xs =
  let rec aux acc rs =
    match rs with
    | [] -> return (List.rev acc)
    | r :: rs -> bind (fn r) @@ fun x -> aux (x :: acc) rs
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
      | x :: xs -> bind (fn x) (fun x -> aux (x :: vs) xs)
    in
    aux [] xs

  (** Generic monadic fold function over a list. *)
  let fold_list ~init ~f xs =
    foldM ~return ~bind ~fold:Foldable.List.fold xs ~init ~f

  module Syntax = struct
    let ( let* ) = Base.bind
    let ( let+ ) = Base.map
  end
end

(** Basic interface for a monad with two type parameters (e.g. Result). *)
module type Base2 = sig
  type ('a, 'b) t

  val ok : 'a -> ('a, 'b) t
  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val map : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val error : 'b -> ('a, 'b) t
  val bind_error : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val map_error : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
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
      | x :: xs -> bind (fn x) (fun x -> aux (x :: vs) xs)
    in
    aux [] xs

  (** Generic monadic fold function over a list. *)
  let fold_list ~init ~f xs =
    foldM ~return:ok ~bind ~fold:Foldable.List.fold xs ~init ~f

  module Syntax = struct
    let ( let* ) = Base.bind
    let ( let+ ) = Base.map
    let ( let/ ) = Base.bind_error
    let ( let- ) = Base.map_error
  end
end

(** Identity monad. *)
module Id = Extend (struct
  type 'a t = 'a

  let[@inline] return x = x
  let[@inline] bind x f = f x
  let[@inline] map x f = f x
end)

(** Result transformer. *)
module ResultT (M : Base) : Base2 with type ('a, 'b) t = ('a, 'b) Result.t M.t =
struct
  type ('a, 'b) t = ('a, 'b) Result.t M.t

  let ok x = M.return (Ok x)
  let error x = M.return (Error x)

  let bind x f =
    M.bind x (function Ok x -> f x | Error z -> M.return (Error z))

  let bind_error x f =
    M.bind x (function Ok x -> M.return (Ok x) | Error z -> f z)

  let map x f = M.map x (Result.map f)
  let map_error x f = M.map x (Result.map_error f)
end

(** List monad. *)
module ListM = Extend (struct
  type 'a t = 'a list

  let return x = [ x ]
  let bind x f = List.concat_map f x
  let map x f = List.map f x
end)

(** Result monad. *)
module ResultM = Extend2 (struct
  type ('a, 'b) t = ('a, 'b) result

  let ok x = Ok x
  let bind = Result.bind
  let map x f = Result.map f x
  let error x = Error x
  let bind_error x f = match x with Ok _ as x -> x | Error e -> f e
  let map_error x f = Result.map_error f x
end)

(** Option monad. *)
module OptionM = Extend (struct
  type 'a t = 'a option

  let bind = Option.bind
  let map x f = Option.map f x
  let return x = Some x
end)

(** Sequence monad. *)
module SeqM = Extend (struct
  type 'a t = 'a Seq.t

  let[@inline] bind x f = Seq.concat_map f x
  let[@inline] map x f = Seq.map f x
  let[@inline] return x = Seq.return x
end)

(** Iterator monad. *)
module IterM = Extend (struct
  type 'a t = 'a Iter.t

  let[@inline] bind x f = Iter.flat_map f x
  let[@inline] map x f = Iter.map f x
  let[@inline] return x = Iter.return x
end)

(** State monad. *)
module StateM (State : sig
  type t
end) =
Extend (struct
  type 'a t = State.t -> 'a * State.t

  let[@inline] return x s = (x, s)

  let[@inline] bind x f s =
    let x', s' = x s in
    f x' s'

  let[@inline] map x f s =
    let x', s' = x s in
    (f x', s')
end)

module StateT_p (M : Base) = struct
  type ('a, 'state) t = 'state -> ('a * 'state) M.t

  let[@inline] return x s = M.return (x, s)
  let[@inline] bind x f s = M.bind (x s) (fun (x', s') -> f x' s')
  let[@inline] map x f s = M.map (x s) (fun (x', s') -> (f x', s'))
  let[@inline] get_state () : ('state, 'state) t = fun s -> M.return (s, s)

  let[@inline] set_state (s : 'state) : (unit, 'state) t =
   fun _ -> M.return ((), s)

  let[@inline] map_state (f : 'state -> 'state) : (unit, 'state) t =
   fun s -> M.return ((), f s)

  let[@inline] lift (m : 'a M.t) : ('a, 'state) t =
   fun s -> M.map m (fun x -> (x, s))

  module Syntax = struct
    let ( let* ) x f = bind x f
    let ( let+ ) x f = map x f
    let ( let*^ ) x f = bind x f
    let ( let+^ ) x f = map x f
  end
end
