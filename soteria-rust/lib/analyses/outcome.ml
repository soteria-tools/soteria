open Soteria.Logs.Printers

type t = Ok | Error | Fatal

let merge o1 o2 =
  match (o1, o2) with
  | Fatal, _ | _, Fatal -> Fatal
  | Error, _ | _, Error -> Error
  | Ok, Ok -> Ok

let merge_list l = List.fold_left (fun o1 (_, o2) -> merge o1 o2) Ok l
let as_status_code = function Ok -> 0 | Error -> 1 | Fatal -> 2
let exit o = exit (as_status_code o)

let pp ft = function
  | Ok -> pp_ok ft "ok"
  | Error -> pp_err ft "error"
  | Fatal -> pp_fatal ft "unknown"
