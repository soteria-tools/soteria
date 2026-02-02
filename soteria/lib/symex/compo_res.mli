(** {1 Compo_res - Compositional Result Type}

    This module defines a three-way result type for compositional symbolic
    execution. Unlike standard [Result.t] which has two cases (Ok/Error), this
    type adds a third case [Missing] for bi-abduction scenarios where frame
    inference is needed.

    {2 Key Concepts}

    {b Compositional Reasoning}: When verifying a function call, the current
    state may not contain all resources needed by the callee. Rather than
    immediately failing, we can infer what's missing (the "frame") and
    continue analysis.

    {b Three Cases}:
    - {b Ok}: Operation succeeded, resources matched
    - {b Error}: Definite error found (e.g., null dereference, assertion failure)
    - {b Missing}: Resources needed but not present; includes fix suggestions

    {2 Usage}

    {[
      open Compo_res.Syntax

      let consume_resource state key =
        match State.find key state with
        | Some v -> Ok v
        | None -> Missing [Fix.need_resource key]

      let example =
        let* x = consume_resource state key1 in
        let* y = consume_resource state key2 in
        Ok (x + y)
    ]}
*)

open Soteria_std

(** Compositional result type.

    - ['ok]: Type of successful result
    - ['err]: Type of definite errors
    - ['fix]: Type of fix suggestions for missing resources *)
type ('ok, 'err, 'fix) t =
  | Ok of 'ok
  | Error of 'err
  | Missing of 'fix list

(** {2 Pretty Printing} *)

val pp :
  ok:(Format.formatter -> 'ok -> unit) ->
  err:(Format.formatter -> 'err -> unit) ->
  miss:(Format.formatter -> 'fix -> unit) ->
  Format.formatter ->
  ('ok, 'err, 'fix) t ->
  unit

(** {2 Constructors} *)

val ok : 'ok -> ('ok, 'err, 'fix) t
val error : 'err -> ('ok, 'err, 'fix) t
val miss : 'fix list -> ('ok, 'err, 'fix) t

(** {2 Predicates} *)

val is_ok : ('ok, 'err, 'fix) t -> bool
val is_error : ('ok, 'err, 'fix) t -> bool
val is_missing : ('ok, 'err, 'fix) t -> bool

(** {2 Extractors}

    These raise [Failure] if the result is of the wrong variant. *)

val get_ok : ('ok, 'err, 'fix) t -> 'ok
val get_error : ('ok, 'err, 'fix) t -> 'err
val get_missing : ('ok, 'err, 'fix) t -> 'fix list

(** {2 List Filters} *)

(** [only_oks results] extracts all [Ok] values. *)
val only_oks : ('ok, 'err, 'fix) t list -> 'ok list

(** [only_errors results] extracts all [Error] values. *)
val only_errors : ('ok, 'err, 'fix) t list -> 'err list

(** [only_missings results] extracts all [Missing] fix lists. *)
val only_missings : ('ok, 'err, 'fix) t list -> 'fix list list

(** {2 Monadic Operations} *)

(** [bind res f] applies [f] to the ok value, propagating Error and Missing. *)
val bind :
  ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

(** [map res f] transforms the ok value. *)
val map : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t

(** [bind_error res f] applies [f] to the error, propagating Ok and Missing. *)
val bind_error :
  ('ok, 'err, 'fix) t -> ('err -> ('ok, 'a, 'fix) t) -> ('ok, 'a, 'fix) t

(** [map_error res f] transforms the error value. *)
val map_error : ('ok, 'err, 'fix) t -> ('err -> 'a) -> ('ok, 'a, 'fix) t

(** [map_missing res f] transforms each fix in the missing list. *)
val map_missing : ('ok, 'err, 'fix) t -> ('fix -> 'a) -> ('ok, 'err, 'a) t

(** {2 Syntax Module} *)

module Syntax : sig
  (** Bind (ok) *)
  val ( let* ) :
    ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

  (** Map (ok) *)
  val ( let+ ) : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t

  (** Bind (error) *)
  val ( let/ ) :
    ('ok, 'err, 'fix) t -> ('err -> ('ok, 'a, 'fix) t) -> ('ok, 'a, 'fix) t

  (** Map (error) *)
  val ( let- ) : ('ok, 'err, 'fix) t -> ('err -> 'a) -> ('ok, 'a, 'fix) t

  (** Map (missing) *)
  val ( let+? ) : ('ok, 'err, 'fix) t -> ('fix -> 'a) -> ('ok, 'err, 'a) t
end

(** {2 Monad Transformer}

    Lifts compositional results into another monad. *)
module T (M : Monad.Base) : sig
  type nonrec ('ok, 'err, 'fix) t = ('ok, 'err, 'fix) t M.t

  val ok : 'ok -> ('ok, 'err, 'fix) t
  val error : 'err -> ('ok, 'err, 'fix) t
  val miss : 'fix list -> ('ok, 'err, 'fix) t

  val bind :
    ('ok, 'err, 'fix) t -> ('ok -> ('a, 'err, 'fix) t) -> ('a, 'err, 'fix) t

  (** [bind_2 res ~f ~fe] handles both ok and error cases. *)
  val bind_2 :
    ('ok, 'err, 'fix) t ->
    f:('ok -> ('a, 'b, 'fix) t) ->
    fe:('err -> ('a, 'b, 'fix) t) ->
    ('a, 'b, 'fix) t

  val bind_error :
    ('ok, 'err, 'fix) t -> ('err -> ('ok, 'a, 'fix) t) -> ('ok, 'a, 'fix) t

  val map : ('ok, 'err, 'fix) t -> ('ok -> 'a) -> ('a, 'err, 'fix) t
  val map_error : ('ok, 'err, 'fix) t -> ('err -> 'a) -> ('ok, 'a, 'fix) t
  val map_missing : ('ok, 'err, 'fix) t -> ('fix -> 'a) -> ('ok, 'err, 'a) t
end
