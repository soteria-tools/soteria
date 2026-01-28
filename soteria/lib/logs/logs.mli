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
    kind : log_kind;  (** The kind of logging to use. *)
    always_log_smt : bool;
        (** Whether to always log SMT queries, even when the level is above
            {!Level.Smt}. *)
    no_color : bool;  (** Whether to disable colors in stderr logging. *)
    hide_unstable : bool;
        (** Whether to hide unstable values like durations (e.g. for diffing
            purposes). *)
  }

  type cli

  val make :
    ?level:Level.t option ->
    ?kind:log_kind ->
    ?always_log_smt:bool ->
    ?no_color:bool ->
    ?hide_unstable:bool ->
    unit ->
    t

  (** A [Cmdliner] term for parsing cli arguments and obtain a {!Config.t}. *)
  val cmdliner_term : unit -> cli Cmdliner.Term.t

  (** Bypass CLI parsing, and manually set the configuration. *)
  val set_and_lock : t -> unit

  (** Receives a CLI input configuration (obtained from parsing the Cli
      arguments using {!Soteria.Logs.Cli.cmdliner_term}), checks that parsing
      went correctly, sets the contained configuration as global configuartion,
      and locks the configuration to prevent the configuration to be modified
      during execution. *)
  val check_set_and_lock : cli -> unit

  (** [with_interject ~interject f] runs [f] while using [interject] as function
      to interact with Stderr.

      This is mostly used to define {!Soteria.Terminal.Progress_bar.run}, which
      requires interjecting while printing a progress bar to the terminal. Users
      of Soteria should probably use {!Soteria.Terminal.Progress_bar} directly,
      and ignore this function. *)
  val with_interject : interject:((unit -> unit) -> unit) -> (unit -> 'a) -> 'a
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

(** Information on the current printing profile, to know what capabilities the
    current output channel has. *)
module Profile : sig
  type t = {
    color : bool;  (** Whether colored output can be used. *)
    utf8 : bool;  (** Whether UTF-8 output can be used. *)
  }

  (** Returns the current printing profile. *)
  val get : unit -> t
end

(** Colors and styles that can be used in logging. *)
module Color : sig
  type t =
    [ `Black
    | `Blue
    | `Cyan
    | `Green
    | `Magenta
    | `Red
    | `White
    | `Yellow
    | `Purple
    | `Orange
    | `Maroon
    | `Forest
    | `Teal
    | `Silver
    | `Gray
    | `DarkBlue ]

  type style = [ `Bold | `Italic | `Underline ]
end

(** Utility printers, that are aware of the current logging configuration. *)
module Printers : sig
  (** Wraps a pretty-printer for unstable values, that should not be output into
      e.g. testing environments where diffs matter. If the config option
      [hide_unstable] is set, will replace *)
  val pp_unstable :
    name:string ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a ->
    unit

  (** Pretty-prints a time quantity in seconds, displaying it in either seconds
      or milli-seconds for smaller values (< 0.05s). Wrapped in {!pp_unstable}.
  *)
  val pp_time : Format.formatter -> float -> unit

  (** For a given value [(total, part)], prints what percentage of [total] is
      represented by [part]. e.g. [pp_percent stdout (5.0, 1.0)] will print
      [20.00%] *)
  val pp_percent : Format.formatter -> float * float -> unit

  (** Prints [1 sing] if [n = 1], [n plur] otherwise. *)
  val pp_plural : sing:string -> plur:string -> Format.formatter -> int -> unit

  (** Pretty-prints a string with a given color *)
  val pp_clr : Color.t -> Format.formatter -> string -> unit

  (** Pretty-prints a string with a given style *)
  val pp_style : Color.style -> Format.formatter -> string -> unit

  (** Pretty-prints a string with a given color and style *)
  val pp_clr2 : Color.t -> Color.style -> Format.formatter -> string -> unit

  (** Pretty-prints a string as an "ok"-colored message *)
  val pp_ok : Format.formatter -> string -> unit

  (** Pretty-prints a string as an "error"-colored message *)
  val pp_err : Format.formatter -> string -> unit

  (** Pretty-prints a string as a "fatal"-colored message *)
  val pp_fatal : Format.formatter -> string -> unit

  (** Pretty-prints a string as a "warning"-colored message *)
  val pp_warn : Format.formatter -> string -> unit
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
