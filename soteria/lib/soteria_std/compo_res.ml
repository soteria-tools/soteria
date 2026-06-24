(** This module defines a three-way result type for compositional symbolic
    execution. Unlike standard [Result.t] which has two cases (Ok/Error), this
    type adds a third case [Missing] for bi-abduction scenarios where anti-frame
    inference is needed, or more generally to represent incompletenesses in the
    engine.

    When performing a function call, the current state may not contain all
    resources needed by the callee. Rather than immediately failing, we can
    infer what's missing (the "anti-frame") and continue analysis.

    {b Three Cases}:
    - {b Ok}: Operation succeeded, resources matched
    - {b Error}: Definite error found (e.g., null dereference, assertion
      failure)
    - {b Missing}: Resources needed but not present; includes fixes. Missing
      contains a {b list} of fixes, providing different ways to resolve the
      missing resources; for instance, when accessing a location in memory, two
      options are possible: either the location is allocated, or the location
      has been freed.

    This module also provides a functor to lift this result type into any
    monadic context. *)

type ('ok, 'err, 'fix) t = Ok of 'ok | Error of 'err | Missing of 'fix list

let pp ~ok ~err ~miss fmt = function
  | Ok x -> Format.fprintf fmt "Ok: %a" ok x
  | Error e -> Format.fprintf fmt "Error: %a" err e
  | Missing fix -> Format.fprintf fmt "Missing: %a" (Fmt.Dump.list miss) fix

let[@inline] ok x = Ok x
let[@inline] error x = Error x
let[@inline] miss x = Missing x
let is_ok = function Ok _ -> true | _ -> false
let is_error = function Error _ -> true | _ -> false
let is_missing = function Missing _ -> true | _ -> false
let get_ok = function Ok x -> x | _ -> failwith "get_ok"
let get_error = function Error x -> x | _ -> failwith "get_error"
let get_missing = function Missing x -> x | _ -> failwith "get_missing"
let only_oks l = List.filter_map (function Ok x -> Some x | _ -> None) l
let only_errors l = List.filter_map (function Error x -> Some x | _ -> None) l

let only_missings l =
  List.filter_map (function Missing x -> Some x | _ -> None) l

let bind f = function
  | Ok x -> f x
  | Error e -> Error e
  | Missing fix -> Missing fix

let map f = function
  | Ok x -> Ok (f x)
  | Error e -> Error e
  | Missing fix -> Missing fix

let bind_error f = function
  | Ok x -> Ok x
  | Error e -> f e
  | Missing fix -> Missing fix

let map_error f = function
  | Ok x -> Ok x
  | Error e -> Error (f e)
  | Missing fix -> Missing fix

let map_missing f = function
  | Ok x -> Ok x
  | Error e -> Error e
  | Missing fixes -> Missing (List.map f fixes)

let to_result_opt = function
  | Ok x -> Some (Result.Ok x)
  | Error e -> Some (Result.Error e)
  | Missing _ -> None

let of_result = function Result.Ok x -> Ok x | Result.Error e -> Error e

module Syntax = struct
  let ( let* ) x f = bind f x
  let ( let+ ) x f = map f x
  let ( let/ ) x f = bind_error f x
  let ( let- ) x f = map_error f x
  let ( let+? ) x f = map_missing f x
end

(** {2 Module types} *)

(** {3 Monads with three type parameters} *)

module type Base = sig
  (** Basic interface for the compositional result monad. *)

  type ('a, 'b, 'c) t

  val ok : 'a -> ('a, 'b, 'c) t
  val error : 'b -> ('a, 'b, 'c) t
  val miss : 'c list -> ('a, 'b, 'c) t
  val bind : ('a -> ('d, 'b, 'c) t) -> ('a, 'b, 'c) t -> ('d, 'b, 'c) t
  val map : ('a -> 'd) -> ('a, 'b, 'c) t -> ('d, 'b, 'c) t
  val bind_error : ('b -> ('a, 'd, 'c) t) -> ('a, 'b, 'c) t -> ('a, 'd, 'c) t
  val map_error : ('b -> 'd) -> ('a, 'b, 'c) t -> ('a, 'd, 'c) t
  val map_missing : ('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t

  val bind2 :
    f:('a -> ('e, 'f, 'c) t) ->
    fe:('b -> ('e, 'f, 'c) t) ->
    ('a, 'b, 'c) t ->
    ('e, 'f, 'c) t
end

module type Syntax = sig
  type ('a, 'b, 'c) t

  val ( let** ) : ('a, 'b, 'c) t -> ('a -> ('d, 'b, 'c) t) -> ('d, 'b, 'c) t
  val ( let++ ) : ('a, 'b, 'c) t -> ('a -> 'd) -> ('d, 'b, 'c) t
  val ( let*- ) : ('a, 'b, 'c) t -> ('b -> ('a, 'd, 'c) t) -> ('a, 'd, 'c) t
  val ( let+- ) : ('a, 'b, 'c) t -> ('b -> 'd) -> ('a, 'd, 'c) t
  val ( let+? ) : ('a, 'b, 'c) t -> ('c -> 'd) -> ('a, 'b, 'd) t
end

module type S = sig
  (** Complete interface for the compositional result monad, including generic
      operations and syntax. *)

  include Base

  val fold :
    (module M : Sigs.Foldable) ->
    'elem M.t -> init:'a -> f:('a -> 'elem -> ('a, 'b, 'c) t) -> ('a, 'b, 'c) t

  val fold_list :
    'elem list -> init:'a -> f:('a -> 'elem -> ('a, 'b, 'c) t) -> ('a, 'b, 'c) t

  val fold_iter :
    'elem Iter.t ->
    init:'a ->
    f:('a -> 'elem -> ('a, 'b, 'c) t) ->
    ('a, 'b, 'c) t

  val iter :
    (module M : Sigs.Foldable) ->
    'elem M.t -> f:('elem -> (unit, 'b, 'c) t) -> (unit, 'b, 'c) t

  val iter_list :
    'elem list -> f:('elem -> (unit, 'b, 'c) t) -> (unit, 'b, 'c) t

  val iter_iter :
    'elem Iter.t -> f:('elem -> (unit, 'b, 'c) t) -> (unit, 'b, 'c) t

  val map_list :
    'elem list -> f:('elem -> ('a, 'b, 'c) t) -> ('a list, 'b, 'c) t

  val map_iter :
    'elem Iter.t -> f:('elem -> ('a, 'b, 'c) t) -> ('a list, 'b, 'c) t

  val all : ('a -> ('b, 'c, 'd) t) -> 'a list -> ('b list, 'c, 'd) t
end

(** {2 Functors} *)

module Extend (M : Base) :
  S with type ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) M.t = struct
  include M

  let[@inline] fold (module M : Sigs.Foldable) xs ~init ~f =
    Monad.foldM (module M) ~return:ok ~bind ~init ~f xs

  let[@inline] fold_list xs ~init ~f = fold (module List) xs ~init ~f
  let[@inline] fold_iter xs ~init ~f = fold (module Iter) xs ~init ~f

  let[@inline] iter (module M : Sigs.Foldable) xs ~f =
    Monad.iterM (module M) ~return:ok ~bind ~f xs

  let[@inline] iter_list xs ~f = iter (module List) xs ~f
  let[@inline] iter_iter xs ~f = iter (module Iter) xs ~f

  let[@inline] map_list xs ~f =
    Monad.mapM (module List) ~return:ok ~bind ~map ~f xs

  let[@inline] map_iter xs ~f =
    Monad.mapM (module Iter) ~return:ok ~bind ~map ~f xs

  let[@inline] all f xs = Monad.all ~return:ok ~bind f xs
end

module Make_syntax (M : Base) :
  Syntax with type ('ok, 'err, 'fix) t := ('ok, 'err, 'fix) M.t = struct
  open M

  let ( let** ) x f = bind f x
  let ( let++ ) x f = map f x
  let ( let*- ) x f = bind_error f x
  let ( let+- ) x f = map_error f x
  let ( let+? ) x f = map_missing f x
end

module T (M : Monad.Base) :
  S with type ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) t M.t = Extend (struct
  type nonrec ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) t M.t

  let ok x = M.return (Ok x)
  let error x = M.return (Error x)
  let miss x = M.return (Missing x)

  let bind f =
    M.bind (function
      | Ok x -> f x
      | Error z -> M.return (Error z)
      | Missing fix -> M.return (Missing fix))

  let bind2 ~f ~fe =
    M.bind (function
      | Ok x -> f x
      | Error z -> fe z
      | Missing fix -> M.return (Missing fix))

  let bind_error f =
    M.bind (function
      | Ok x -> M.return (Ok x)
      | Error e -> f e
      | Missing fix -> M.return (Missing fix))

  let map f = M.map (map f)
  let map_error f = M.map (map_error f)
  let map_missing f = M.map (map_missing f)
end)
