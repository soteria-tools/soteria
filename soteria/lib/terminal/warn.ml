open Soteria_std

module WarnHSet = Hashset.Make (struct
  type t = string * Diagnostic.severity [@@deriving eq, ord, show]

  let hash = Hashtbl.hash
end)

let issued_warnings = WarnHSet.with_capacity 8
let was_issued ~severity msg = WarnHSet.mem issued_warnings (msg, severity)

let warn ?(severity = Diagnostic.Warning) msg =
  Diagnostic.print_diagnostic_simple ~severity msg

let warn_once ?(severity = Diagnostic.Warning) msg =
  if not (was_issued ~severity msg) then (
    WarnHSet.add issued_warnings (msg, severity);
    warn ~severity msg)
