(** This module defines a three-way result type for compositional symbolic
    execution. Unlike standard [Result.t] which has two cases (Ok/Error), this
    type adds a third case [Missing] for bi-abduction scenarios where anti-frame
    inference is needed.

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

open Soteria_std

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

let bind x f =
  match x with Ok x -> f x | Error e -> Error e | Missing fix -> Missing fix

let map x f =
  match x with
  | Ok x -> Ok (f x)
  | Error e -> Error e
  | Missing fix -> Missing fix

let bind_error x f =
  match x with Ok x -> Ok x | Error e -> f e | Missing fix -> Missing fix

let map_error x f =
  match x with
  | Ok x -> Ok x
  | Error e -> Error (f e)
  | Missing fix -> Missing fix

let map_missing x f =
  match x with
  | Ok x -> Ok x
  | Error e -> Error e
  | Missing fixes -> Missing (List.map f fixes)

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) = map
  let ( let/ ) = bind_error
  let ( let- ) = map_error
  let ( let+? ) = map_missing
end

module T (M : Monad.Base) = struct
  type nonrec ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) t M.t

  let ok x = M.return (Ok x)
  let error x = M.return (Error x)
  let miss x = M.return (Missing x)

  let bind x f =
    M.bind x (function
      | Ok x -> f x
      | Error z -> M.return (Error z)
      | Missing fix -> M.return (Missing fix))

  let bind_2 x ~f ~fe =
    M.bind x (function
      | Ok x -> f x
      | Error z -> fe z
      | Missing fix -> M.return (Missing fix))

  let bind_error x f =
    M.bind x (function
      | Ok x -> M.return (Ok x)
      | Error e -> f e
      | Missing fix -> M.return (Missing fix))

  let map x f = M.map x (fun x -> map x f)
  let map_error x f = M.map x (fun x -> map_error x f)
  let map_missing x f = M.map x (fun x -> map_missing x f)
end
