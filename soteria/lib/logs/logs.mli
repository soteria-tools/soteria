(** Logging facilities for symbolic execution.

    This module (and particularly {!L}) contains logging facilities for users of
    Soteria. The library knows how to produce text logs to stderr or structured
    logs in HTML format. *)

type ('a, 'b) msgf = (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

module Level : sig
  type t =
    | Smt  (** For messages that describe interaction with an SMT solver *)
    | Trace  (** For very detailed trace of execution. *)
    | Debug  (** Debugging messages, helpful when something goes wrong *)
    | Info  (** Information that is not crucial to the execution. *)
    | Warn  (** Warnings *)
    | App  (** User-facing messages *)
    | Error  (** User-facing messages when something went wrong. *)
end

module Config : sig
  (** The forms of logging currently supported by Soteria. *)
  type log_kind = Stderr | Html

  type t = {
    level : Level.t option;
        (** The current level of logging, [None] if disabled.*)
    kind : log_kind;
    always_log_smt : bool;
        (** Whether to always log SMT queries, even when the level is above
            {!Level.Smt}. *)
  }

  (** Receives a result wrapping a configuration (obtained from parsing the Cli
      arguments using {!Soteria.Logs.Cli.term}), checks that parsing went
      correctly, sets the contained configuration as global configuartion, and
      locks the configuration to prevent the configuration to be modified during
      execution. *)
  val check_set_and_lock : (t, string) result -> unit

  (** [with_interject ~interject f] runs [f] while using [interject] as function
      to interact with Stderr.

      This is mostly used to define {!Soteria.Terminal.Progress_bar.run}, which
      requires interjecting while printing a progress bar to the terminal. Users
      of Soteria should probably use {!Soteria.Terminal.Progress_bar} directly,
      and ignore this function. *)
  val with_interject : interject:((unit -> unit) -> unit) -> (unit -> 'a) -> 'a
end

module Cli : sig
  (** A [Cmdliner] term for parsing cli arguments and obtain a {!Config.t}. *)
  val term : (Config.t, string) result Cmdliner.Term.t
end

module L : sig
  (** [with_section ~is_branch name f] runs [f] and aggregates its log messages
      within a collapsible section of the log file (when logs are in HTML mode).

      Sections should not be used while defining symbolic processes, as
      branching will cause a section to be opened once but closed several times.
  *)
  val with_section : ?is_branch:bool -> string -> (unit -> 'a) -> 'a

  (** Logs a message at a given level.

      For instance, one can log the string "The number is 42" at level [Info]
      with
      {[
        log ~level:Level.Info (fun m -> m "The number is %d" 42)
      ]} *)
  val log : level:Level.t -> ('a, unit) msgf -> unit

  (** [smt] is [log ~level:Smt] *)
  val smt : ('a, unit) msgf -> unit

  (** [trace] is [log ~level:Trace] *)
  val trace : ('a, unit) msgf -> unit

  (** [debug] is [log ~level:Debug] *)
  val debug : ('a, unit) msgf -> unit

  (** [info] is [log ~level:Info] *)
  val info : ('a, unit) msgf -> unit

  (** [warn] is [log ~level:Warn] *)
  val warn : ('a, unit) msgf -> unit

  (** [app] is [log ~level:App] *)
  val app : ('a, unit) msgf -> unit

  (** [error] is [log ~level:Error] *)
  val error : ('a, unit) msgf -> unit
end

(** Helper module to be opened when using logging.

    {[
      open Soteria.Logs.Import
      ...

      L.info (fun m -> m "Hello world")
    ]} *)
module Import : sig
  module L : module type of L
end
