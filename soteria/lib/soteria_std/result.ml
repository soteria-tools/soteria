(** Extensions to [Stdlib.Result] with conversion and extraction utilities. *)

include Stdlib.Result

(** The bind operator; we swap it to match the usual order of arguments for
    monads. *)
let bind f x = bind x f

(** The error bind operator. *)
let bind_error f x = match x with Ok _ as x -> x | Error e -> f e

(** Convert an option to a result. *)
let of_opt ~err = function Some v -> Ok v | None -> Error err

(** Get the value of a result or apply a function to the error. *)
let get_or ~err = function Ok v -> v | Error e -> err e

(** Get the value of a result or raise an exception with the error message. *)
let get_or_raise ex = function Ok v -> v | Error msg -> raise (ex msg)
