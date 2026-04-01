(** Tracking of source coverage across symbolic execution.

    This module exposes two layers:
    - an aggregate model ([t], [report], JSON/export writers), and
    - an effect context ([As_ctx]) used during symbolic execution.

    Instrumented code records events through {!As_ctx.mark_line} and
    {!As_ctx.mark_branch}. The current effect handler accumulates events into a
    coverage value, which can later be transformed to a report and exported
    (JSON, Cobertura XML, LCOV, and other writers).

    {b Important: branch identity and [branch_id] robustness}

    Branch coverage is keyed by source location plus [branch_id]. The line
    number is useful for human-facing reports, but line alone is not enough to
    uniquely identify a conditional: multiple branch points may share the same
    line (e.g. short-circuit expressions, compact formatting, macro-generated
    code).

    To keep branch coverage robust, [branch_id] should be:
    - stable across runs of the same source,
    - unique among branch points that could otherwise collide,
    - derived from frontend information that survives translation passes.

    In practice, good [branch_id] candidates are frontend-specific location
    identifiers (or [file:line:col]-like identities when column information is
    trustworthy). Avoid volatile ids tied to solver state or fresh-variable
    naming. *)

open Soteria_std.Hashtbl

type branch_side = Then | Else

type branch_span = {
  file : string;  (** Source file path as reported by the frontend. *)
  line : int;  (** 1-based source line for human-facing reporting. *)
  branch_id : string;
      (** Stable identifier for the branch point; used for disambiguation and
          aggregation. *)
}

type branch_coverage = { line : int; then_hits : int; else_hits : int }
type function_coverage = { line : int; end_line : int option; hits : int }

type file_hits = {
  lines : int Hint.t;
      (** Reachable lines with hit counts. A value of [0] means reachable but
          not reached. *)
  branches : branch_coverage Hstring.t;
      (** For each conditional id, then/else hit counters and the source line
          where that conditional lives. Note this cannot be stored along the
          lines, as there may be several conditionals on one line. *)
  functions : function_coverage Hstring.t;
      (** Registered function definitions and their hit counts. *)
}

type t = file_hits Hstring.t
type 'a with_coverage = { res : 'a; coverage : t }

val create : unit -> t

(** [merge c1 c2] merges two coverage collections [c1] and [c2] into a single
    coverage object by combining hit counts and reached branches. *)
val merge : t -> t -> t

val with_empty_coverage : 'a -> 'a with_coverage
val map_with_coverage : ('a -> 'b) -> 'a with_coverage -> 'b with_coverage
val to_yojson : t -> Yojson.Safe.t

module type Writer = sig
  val to_formatter : Format.formatter -> t -> unit
  val to_file : string -> t -> unit
end

module JsonWriter : Writer
module CoberturaWriter : Writer
module LcovWriter : Writer

val output : t -> unit

module As_ctx : sig
  (** [with_coverage () f] runs [f], handling coverage effects and returning
      both result and aggregated coverage. *)
  val with_coverage : unit -> (unit -> 'a) -> 'a with_coverage

  (** [with_coverage_ignored () f] runs [f] but ignores coverage effects. *)
  val with_coverage_ignored : unit -> (unit -> 'a) -> 'a

  (** [with_coverage_dumped () f] runs [f], handles coverage effects, and dumps
      coverage according to current configuration if enabled. *)
  val with_coverage_dumped : unit -> (unit -> 'a) -> 'a

  (** [register_line ~file ~line] marks a source line as reachable without
      recording a hit. Useful to include not-yet-reached lines in coverage
      reports. *)
  val register_line : file:string -> line:int -> unit

  (** [register_lines ~file lines] marks all lines from [lines] as reachable
      without recording hits. Existing hit counts are preserved. *)
  val register_lines : file:string -> int Iter.t -> unit

  (** [register_files_lines spans] marks reachable lines for multiple files. The
      outer iterator yields [(file, lines)] pairs. *)
  val register_files_lines : (string * int Iter.t) Iter.t -> unit

  (** [register_branch span] registers a branch point as reachable even if
      neither side is taken. *)
  val register_branch : branch_span -> unit

  (** [register_function ~file ~name ~line ?end_line ()] registers function
      metadata for export. *)
  val register_function :
    file:string -> name:string -> line:int -> ?end_line:int -> unit -> unit

  (** [register_functions fns] registers function metadata in bulk. The iterator
      yields [(file, name, line, end_line)] tuples. *)
  val register_functions : (string * string * int * int option) Iter.t -> unit

  (** [mark_line ~file ~line] records one hit for a source line. *)
  val mark_line : file:string -> line:int -> unit

  (** [mark_branch span side] increments hit count for [side] of [span]. *)
  val mark_branch : branch_side -> branch_span -> unit

  (** [mark_function ~file ~name ~line ?end_line ()] increments function hit
      count and registers function metadata if it was missing. *)
  val mark_function :
    file:string -> name:string -> line:int -> ?end_line:int -> unit -> unit

  (** [get_copy ()] retrieves a copy of the coverage aggregated in the current
      environment. *)
  val get_copy : unit -> t
end

module Config : sig
  type t

  val cmdliner_term : unit -> t Cmdliner.Term.t
  val set_and_lock : t -> unit
end
