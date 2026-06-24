(** Monad interfaces and implementations. *)

(** {2 Module Types} *)

(** {3 Monads with one type parameter} *)

module type Base = sig
  (** Basic interface for a monad with one type parameter. *)

  type 'a t

  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Syntax = sig
  (** Syntax extension for monads (let* and let+). *)

  type 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

module type Extension = sig
  type 'a t

  val fold :
    (module M : Sigs.Foldable) ->
    'elem M.t -> init:'a -> f:('a -> 'elem -> 'a t) -> 'a t

  val fold_list : 'elem list -> init:'a -> f:('a -> 'elem -> 'a t) -> 'a t
  val fold_iter : 'elem Iter.t -> init:'a -> f:('a -> 'elem -> 'a t) -> 'a t

  val iter :
    (module M : Sigs.Foldable) -> 'elem M.t -> f:('elem -> unit t) -> unit t

  val iter_list : 'elem list -> f:('elem -> unit t) -> unit t
  val iter_iter : 'elem Iter.t -> f:('elem -> unit t) -> unit t
  val map_list : 'elem list -> f:('elem -> 'a t) -> 'a list t
  val map_iter : 'elem Iter.t -> f:('elem -> 'a t) -> 'a list t
  val all : ('a -> 'b t) -> 'a list -> 'b list t
end

module type S = sig
  (** Complete monad interface including generic operations and syntax. *)

  include Base
  include Extension with type 'a t := 'a t
  module Syntax : Syntax with type 'a t = 'a t
end

(** {3 Monads with two type parameters} *)

module type Base2 = sig
  (** Basic interface for a monad with two type parameters (e.g. Result). *)

  type ('a, 'b) t

  val ok : 'a -> ('a, 'b) t
  val bind : ('a -> ('c, 'b) t) -> ('a, 'b) t -> ('c, 'b) t
  val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val error : 'b -> ('a, 'b) t
  val bind_error : ('b -> ('a, 'c) t) -> ('a, 'b) t -> ('a, 'c) t
  val map_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end

module type Syntax2 = sig
  (** Syntax extension for two-parameter monads. *)

  type ('a, 'b) t

  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val ( let/ ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val ( let- ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
end

module type Extension2 = sig
  type ('a, 'b) t

  val fold :
    (module M : Sigs.Foldable) ->
    'elem M.t -> init:'a -> f:('a -> 'elem -> ('a, 'b) t) -> ('a, 'b) t

  val fold_list :
    'elem list -> init:'a -> f:('a -> 'elem -> ('a, 'b) t) -> ('a, 'b) t

  val fold_iter :
    'elem Iter.t -> init:'a -> f:('a -> 'elem -> ('a, 'b) t) -> ('a, 'b) t

  val iter :
    (module M : Sigs.Foldable) ->
    'elem M.t -> f:('elem -> (unit, 'b) t) -> (unit, 'b) t

  val iter_list : 'elem list -> f:('elem -> (unit, 'b) t) -> (unit, 'b) t
  val iter_iter : 'elem Iter.t -> f:('elem -> (unit, 'b) t) -> (unit, 'b) t
  val map_list : 'elem list -> f:('elem -> ('a, 'b) t) -> ('a list, 'b) t
  val map_iter : 'elem Iter.t -> f:('elem -> ('a, 'b) t) -> ('a list, 'b) t
  val all : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
end

module type S2 = sig
  (** Complete interface for two-parameter monads. *)

  include Base2
  include Extension2 with type ('a, 'b) t := ('a, 'b) t
  module Syntax : Syntax2 with type ('a, 'b) t = ('a, 'b) t
end

(** {2 Lifters and implementations} *)

(** Generic monadic fold function. *)
let[@inline] foldM (module M : Sigs.Foldable) ~return ~bind xs ~init ~f =
  M.fold (fun acc x -> bind (fun acc -> f acc x) acc) (return init) xs

let[@inline] iterM (module M : Sigs.Foldable) ~return ~bind xs ~f =
  M.fold (fun acc x -> bind (fun () -> f x) acc) (return ()) xs

let[@inline] mapM (module M : Sigs.Foldable) ~return ~bind ~map xs ~f =
  foldM
    (module M)
    ~return ~bind xs ~init:[]
    ~f:(fun acc x -> map (fun y -> y :: acc) (f x))
  |> bind (fun l -> return (List.rev l))

(** Generic monadic map function that collects results. *)
let all ~return ~bind fn xs =
  let rec aux acc rs =
    match rs with
    | [] -> return (List.rev acc)
    | r :: rs -> bind (fun x -> aux (x :: acc) rs) (fn r)
  in
  aux [] xs

module Make_extension (Base : Base) : Extension with type 'a t := 'a Base.t =
struct
  (** Functor to create generic monadic operations for a basic monad. *)

  let[@inline] fold (module M : Sigs.Foldable) xs ~init ~f =
    foldM (module M) ~return:Base.return ~bind:Base.bind xs ~init ~f

  let[@inline] fold_list xs ~init ~f = fold (module List) xs ~init ~f
  let[@inline] fold_iter xs ~init ~f = fold (module Iter) xs ~init ~f

  let[@inline] iter (module M : Sigs.Foldable) xs ~f =
    iterM (module M) ~return:Base.return ~bind:Base.bind xs ~f

  let[@inline] iter_list xs ~f = iter (module List) xs ~f
  let[@inline] iter_iter xs ~f = iter (module Iter) xs ~f

  let[@inline] map_list xs ~f =
    mapM (module List) ~return:Base.return ~bind:Base.bind ~map:Base.map xs ~f

  let[@inline] map_iter xs ~f =
    mapM (module Iter) ~return:Base.return ~bind:Base.bind ~map:Base.map xs ~f

  let[@inline] all fn xs = all ~return:Base.return ~bind:Base.bind fn xs
end

module Make_syntax (Base : Base) : Syntax with type 'a t = 'a Base.t = struct
  (** Functor to create syntax for a basic monad. *)

  type 'a t = 'a Base.t

  let[@inline] ( let* ) x f = Base.bind f x
  let[@inline] ( let+ ) x f = Base.map f x
end

module Extend (Base : Base) : S with type 'a t = 'a Base.t = struct
  (** Functor to extend a basic monad with derived operations and syntax. *)

  include Base

  (* TODO: include functor *)
  include Make_extension (Base)
  module Syntax = Make_syntax (Base)
end

module Make_extension2 (Base : sig
  type ('a, 'b) t

  val ok : 'a -> ('a, 'b) t
  val bind : ('a -> ('c, 'b) t) -> ('a, 'b) t -> ('c, 'b) t
  val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
end) : Extension2 with type ('a, 'b) t := ('a, 'b) Base.t = struct
  (** Functor to create generic operations for a basic two-parameter monad. *)

  let[@inline] fold (module M : Sigs.Foldable) xs ~init ~f =
    foldM (module M) ~return:Base.ok ~bind:Base.bind xs ~init ~f

  let[@inline] fold_list xs ~init ~f = fold (module List) xs ~init ~f
  let[@inline] fold_iter xs ~init ~f = fold (module Iter) xs ~init ~f

  let[@inline] iter (module M : Sigs.Foldable) xs ~f =
    iterM (module M) ~return:Base.ok ~bind:Base.bind xs ~f

  let[@inline] iter_list xs ~f = iter (module List) xs ~f
  let[@inline] iter_iter xs ~f = iter (module Iter) xs ~f

  let[@inline] map_list xs ~f =
    mapM (module List) ~return:Base.ok ~bind:Base.bind ~map:Base.map xs ~f

  let[@inline] map_iter xs ~f =
    mapM (module Iter) ~return:Base.ok ~bind:Base.bind ~map:Base.map xs ~f

  let[@inline] all fn xs = all ~return:Base.ok ~bind:Base.bind fn xs
end

module Make_syntax2 (Base : Base2) :
  Syntax2 with type ('a, 'b) t = ('a, 'b) Base.t = struct
  type nonrec ('a, 'b) t = ('a, 'b) Base.t

  let[@inline] ( let* ) x f = Base.bind f x
  let[@inline] ( let+ ) x f = Base.map f x
  let[@inline] ( let/ ) x f = Base.bind_error f x
  let[@inline] ( let- ) x f = Base.map_error f x
end

module Extend2 (Base : Base2) : S2 with type ('a, 'b) t = ('a, 'b) Base.t =
struct
  (** Functor to extend a basic two-parameter monad. *)

  include Base
  include Make_extension2 (Base)
  module Syntax = Make_syntax2 (Base)
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

  let[@inline] ok x = M.return (Ok x)
  let[@inline] error x = M.return (Error x)

  let[@inline] bind f =
    M.bind (function Ok x -> f x | Error z -> M.return (Error z))

  let[@inline] bind_error f =
    M.bind (function Ok x -> M.return (Ok x) | Error z -> f z)

  let[@inline] map f = M.map (Result.map f)
  let[@inline] map_error f = M.map (Result.map_error f)
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

(** State monad transformer. *)
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

module StateT_p (M : Base) = struct
  (** State monad transformer with polymorphic state. *)

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
