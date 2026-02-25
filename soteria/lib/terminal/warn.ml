open Soteria_std
module WarnHSet = Hashset.Make (String.Interned)

module WarnTraceHSet = Hashset.Make (struct
  include Int

  let pp = Fmt.int
end)

let issued_warnings = WarnHSet.with_capacity 8
let issued_traces = WarnTraceHSet.with_capacity 8
let was_issued msg = WarnHSet.mem issued_warnings msg
let warn msg = Diagnostic.print_diagnostic_simple ~severity:Warning msg

(** Like warn but receives an interned string and only warns once per identical
    string. *)
let warn_once msg =
  if not (was_issued msg) then (
    WarnHSet.add issued_warnings msg;
    warn (String.Interned.to_string msg))

let warn_trace_once ~id ~as_ranges ~call_trace msg =
  if not (WarnTraceHSet.mem issued_traces id) then (
    WarnTraceHSet.add issued_traces id;
    Diagnostic.print_diagnostic ~severity:Warning ~as_ranges ~call_trace ~msg)
