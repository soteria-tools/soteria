(** Tracking of statistics across symbolic execution. *)

open Soteria_std.Hashtbl

(** {2 General definitions} *)

type stat_entry =
  | Int of int  (** An integer statistic; joined by addition. *)
  | Float of float  (** A float statistic; joined by addition. *)
  | StrSeq of string Dynarray.t
      (** A sequence of strings; joined by concatenation. *)
  | Map of stat_entry Hstring.t
      (** A map from strings to stat entries; joined by merging entries
          recursively. *)
  | Yojson of Yojson.Safe.t
      (** A JSON value; joined by creating a JSON array of the two values if
          both are not arrays, or concatenating the two arrays if they are. Use
          this as a last resort when no other type fits. *)

type t [@@deriving yojson]
type 'a with_stats = { res : 'a; stats : t }

exception Incompatible_entries

val with_empty_stats : 'a -> 'a with_stats
val map_with_stats : ('a -> 'b) -> 'a with_stats -> 'b with_stats

(** [merge s1 s2] merges two statistics [s1] and [s2] into a single statistic by
    combining their entries. Does not modify [s1] or [s2]. *)
val merge : t -> t -> t

(** Creates a new, empty statistics collection. *)
val create : unit -> t

(** {2 Accessors} *)

(** [as_int entry] returns the integer value of the statistic entry [entry].
    Raises [Incompatible_entries] if [entry] is not an integer. *)
val as_int : stat_entry -> int

(** [as_float entry] returns the float value of the statistic entry [entry].
    Raises [Incompatible_entries] if [entry] is not a float. *)
val as_float : stat_entry -> float

(** [as_strseq entry] returns the string sequence value of the statistic entry
    [entry]. Raises [Incompatible_entries] if [entry] is not a string sequence.
*)
val as_strseq : stat_entry -> string Dynarray.t

(** [as_map entry] returns the map value of the statistic entry [entry]. Raises
    [Incompatible_entries] if [entry] is not a map. *)
val as_map : stat_entry -> stat_entry Hstring.t

(** [as_yojson entry] returns the JSON value of the statistic entry [entry].
    Raises [Incompatible_entries] if [entry] is not a JSON value. *)
val as_yojson : stat_entry -> Yojson.Safe.t

(** [get_entry stats name] returns the statistic entry under the given name
    [name] in the statistics collection [stats], or [None] if no such entry
    exists. *)
val get_entry : t -> string -> stat_entry option

(** [get_int ?default stats name] returns the integer statistic under the given
    name [name] in the statistics collection [stats]. If no such entry exists,
    returns [default] if provided (defaults to [0]). Raises
    [Incompatible_entries] if the entry under [name] is not an integer. *)
val get_int : ?default:int -> t -> string -> int

(** [get_float ?default stats name] returns the float statistic under the given
    name [name] in the statistics collection [stats]. If no such entry exists,
    returns [default] if provided (defaults to [0.0]). Raises
    [Incompatible_entries] if the entry under [name] is not a float. *)
val get_float : ?default:float -> t -> string -> float

(** [get_strseq stats name] returns the string sequence statistic under the
    given name [name] in the statistics collection [stats]. If no such entry
    exists, returns an empty sequence. Raises [Incompatible_entries] if the
    entry under [name] is not a string sequence. *)
val get_strseq : t -> string -> string Dynarray.t

(** [get_map stats name] returns the map statistic under the given name [name]
    in the statistics collection [stats]. If no such entry exists, returns an
    empty map. Raises [Incompatible_entries] if the entry under [name] is not a
    map. *)
val get_map : t -> string -> stat_entry Hstring.t

(** {2 Printing, outputting} *)

(** Default printer for statistics entries. *)
val default_printer : Format.formatter -> stat_entry -> unit

(** Utility printer to print a list given an iteration function. Exposed for
    convenience and consistency with the default stats printing. *)
val pp_iter_list :
  (('a -> unit) -> 'b -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'b ->
  unit

(** Utility printer to print a list of bindings given an iteration function.
    Exposed for convenience and consistency with the default stats printing. *)
val pp_iter_bindings_list :
  (('a -> 'b -> unit) -> 'c -> unit) ->
  (Format.formatter -> 'a * 'b -> unit) ->
  Format.formatter ->
  'c ->
  unit

(** [register key ?name printer] Registers a new printer for statistics entries
    of name [name], possibly overriding previously set printers. If an entry
    with the given name is found during pretty-printing and no printer has been
    registered for that name, [default_printer] is used instead. [printer] also
    receives the full stats object, to enable more context-aware printing. A
    string [name] can be provided to be used as a header when printing the
    statistic; if none is provided, [key] is used instead. *)
val register_printer :
  string ->
  ?name:string ->
  (t -> Format.formatter -> stat_entry -> unit) ->
  unit

(** [disable_printer key] disables any printing for the statistic for [key],
    meaning it will not be printed at all. Use [register_printer] to revert
    this. *)
val disable_printer : string -> unit

(** Convenience function to register a printer for [Int] statistics, fallsback
    to [default_printer] for other types. *)
val register_int_printer :
  string -> ?name:string -> (t -> Format.formatter -> int -> unit) -> unit

(** Convenience function to register a printer for [Float] statistics, fallsback
    to [default_printer] for other types. *)
val register_float_printer :
  string -> ?name:string -> (t -> Format.formatter -> float -> unit) -> unit

(** Convenience function to register a printer for [StrSeq] statistics,
    fallsback to [default_printer] for other types. *)
val register_strseq_printer :
  string ->
  ?name:string ->
  (t -> Format.formatter -> string Dynarray.t -> unit) ->
  unit

(** Convenience function to register a printer for [Map] statistics, fallsback
    to [default_printer] for other types. *)
val register_map_printer :
  string ->
  ?name:string ->
  (t -> Format.formatter -> stat_entry Hstring.t -> unit) ->
  unit

(** Pretty-prints statistics to the given formatter. *)
val pp : Format.formatter -> t -> unit

(** Outputs the stats to a file, to stdout, or nowhere, according to the current
    configuration. *)
val output : t -> unit

module As_ctx : sig
  (** Module for manipulating statistics as a context using algebraic effects
      (that are hidden). All calls to any function in this module should happen
      only inside a function wrapped with {!val-with_stats}, ensuring that the
      statistics are properly passed around. *)

  (** [with_stats () f] runs function [f] and handles effects raised by the
      functions of this module such as {!push_entry}, and returns a record
      containing the result of executing [f] together with the obtained
      statistics. *)
  val with_stats : unit -> (unit -> 'a) -> 'a with_stats

  (** [with_stats_ignored () f] runs function [f] and handles effects raised by
      the functions of this module, but ignores their effect. This is to be used
      when the user does not wish to pay the (minor) performance cost of stats
      bookkeeping. *)
  val with_stats_ignored : unit -> (unit -> 'a) -> 'a

  (** [push_entry name entry] adds the given statistic [entry] under the given
      name [name] to the current statistics context. *)
  val push_entry : string -> stat_entry -> unit

  (** [push_int name n] is [push_entry name (Int n)]. *)
  val add_int : string -> int -> unit

  (** [incr name] is [add_int name 1]. *)
  val incr : string -> unit

  (** [push_float name f] is [push_entry name (Float f)]. *)
  val add_float : string -> float -> unit

  (** [add_time_of_to name f] measures the execution time of function [f] in
      seconds, and adds it as a float statistic under the given name [name]. It
      returns the result of executing [f]. *)
  val add_time_of_to : string -> (unit -> 'a) -> 'a

  (** [push_str name s] adds the given string [s] as a single entry in a string
      sequence statistic under the given name [name]. *)
  val push_str : string -> string -> unit

  (** [push_binding name key entry] adds the given statistic [entry] under the
      given key [key] in a map statistic under the given name [name]. *)
  val push_binding : string -> string -> stat_entry -> unit

  (** [push_string_binding name key s] adds the given string [s] as a single
      entry in a string sequence statistic under the given key [key] in a map
      statistic under the given name [name]. *)
  val push_string_binding : string -> string -> string -> unit
end

module Config : sig
  (** Configuration options for statistics output. *)

  type t

  val cmdliner_term : unit -> t Cmdliner.Term.t
  val set_and_lock : t -> unit
end
