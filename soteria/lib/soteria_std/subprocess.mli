(** Tracked OS subprocesses.

    A subprocess opened with {!open_} is recorded in an internal registry and
    force-killed when the host process exits -- normally or on an uncaught
    exception, via [at_exit] -- or is interrupted by SIGINT/SIGTERM, via a
    signal handler this module installs. This keeps a child that is busy and not
    reading its stdin (e.g. an SMT solver mid-check) from being orphaned: such a
    child does not notice its stdin pipe closing, so closing our end is not
    enough.

    {!stop_process} and {!force_stop_process} are idempotent and unregister the
    subprocess, so one stopped explicitly is not touched again at exit. *)

(** A subprocess and the channels connected to its standard streams. *)
type t

(** [open_ prog args] spawns [prog] with argument vector [args] (as for
    {!Unix.open_process_args_full}, so [args.(0)] is conventionally [prog]) and
    registers it for cleanup. *)
val open_ : string -> string array -> t

(** The channel writing to the subprocess's standard input. *)
val stdin : t -> out_channel

(** The channel reading the subprocess's standard output. *)
val stdout : t -> in_channel

(** The channel reading the subprocess's standard error. *)
val stderr : t -> in_channel

(** Close the subprocess with {!Unix.close_process_full} and unregister it.
    Idempotent. *)
val stop_process : t -> unit

(** Kill the subprocess (SIGKILL), then close and unregister it as
    {!stop_process} does. Idempotent. *)
val force_stop_process : t -> unit
