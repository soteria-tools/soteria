(** A server instance. The server is not necessarily created or running. *)
type instance

val log_message :
  instance:instance ->
  [< `Debug | `Error | `Info | `Warn ] ->
  ('a, unit, string, unit) format4 ->
  'a

(** An empty instance. No server has been instantiated. *)
val empty_instance : unit -> instance

(** (Re)starts an instance. *)
val start : instance -> unit Promise.t

(** Stops an instance if it is running, otherwise does nothing. *)
val stop : instance -> unit Promise.t

(** Makes a disposable out of an instance. *)
val disposable : instance -> Vscode.Disposable.t
