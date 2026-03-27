(** Tracking of source coverage across symbolic execution. *)

open Soteria_std.Hashtbl

type branch_side = [ `Then | `Else ]
type source_span = { file : string; line : int; branch_id : string }
type t
type 'a with_coverage = { res : 'a; coverage : t }
type branch_coverage = { line : int; then_reached : bool; else_reached : bool }

type file_coverage = {
  lines : int Hstring.t;
      (** Covered lines with their hit counts. Missing lines are omitted. *)
  branches : branch_coverage Hstring.t;
      (** For each conditional id, whether then/else was reached and on which
          line that conditional lives. *)
}

type report = file_coverage Hstring.t

type coverage_writer = {
  write_to_formatter : Format.formatter -> report -> unit;
  write_to_file : string -> report -> unit;
}

val create : unit -> t

(** [merge c1 c2] merges two coverage collections [c1] and [c2] into a single
    coverage object by combining hit counts and reached branches. *)
val merge : t -> t -> t

val with_empty_coverage : 'a -> 'a with_coverage
val map_with_coverage : ('a -> 'b) -> 'a with_coverage -> 'b with_coverage

(** [to_report t] returns a per-file aggregated report suitable for exporters.
*)
val to_report : t -> report

val to_yojson : t -> Yojson.Safe.t
val report_to_yojson : report -> Yojson.Safe.t

module Writers : sig
  val json : coverage_writer
  val cobertura : coverage_writer
  val codecov_json : coverage_writer
end

val output_with : coverage_writer -> t -> unit
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

  val mark_line : file:string -> line:int -> unit

  (** [mark_branch span side] records that [side] of [span] has been reached. *)
  val mark_branch : source_span -> branch_side -> unit

  (** [get_copy ()] retrieves a copy of the coverage aggregated in the current
      environment. *)
  val get_copy : unit -> t
end

module Config : sig
  type t

  val cmdliner_term : unit -> t Cmdliner.Term.t
  val set_and_lock : t -> unit
end
