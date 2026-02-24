open Soteria_std

module WarnHSet = Hashset.Make (struct
  type t = String.Interned.t

  let hash s = Int.hash @@ String.Interned.tag s
  let equal s1 s2 = String.Interned.tag s1 = String.Interned.tag s2
  let pp ft s = Fmt.string ft (String.Interned.to_string s)
end)

let issued_warnings = WarnHSet.with_capacity 8
let was_issued msg = WarnHSet.mem issued_warnings msg
let warn msg = Diagnostic.print_diagnostic_simple ~severity:Warning msg

(** Like warn but receives an interned string and only warns once per identical
    string. *)
let warn_once msg =
  if not (was_issued msg) then (
    WarnHSet.add issued_warnings msg;
    warn (String.Interned.to_string msg))
