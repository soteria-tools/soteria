(** Wraps a pretty-printer for unstable values, that should not be output into
    e.g. testing environments where diffs matter. If the config option
    [hide_unstable] is set, will replace *)
let pp_unstable ~name pp ft x =
  if (Config.get ()).hide_unstable then Fmt.pf ft "<%s>" name else pp ft x

(** Pretty-prints a time quantity in seconds, displaying it in either seconds or
    milli-seconds for smaller values (< 0.05s). Wrapped in {!pp_unstable}. *)
let pp_time =
  let pp_time ft t =
    if t < 0.05 then Fmt.pf ft "%ams" (Fmt.float_dfrac 2) (t *. 1000.)
    else Fmt.pf ft "%as" (Fmt.float_dfrac 2) t
  in
  pp_unstable ~name:"time" pp_time

(** For a given value [(total, part)], prints what percentage of [total] is
    represented by [part]. e.g. [pp_percent stdout (5, 1)] will print [20.00%]
*)
let pp_percent ft (total, part) =
  Fmt.pf ft "%a%%" (Fmt.float_dfrac 2) (100. *. part /. total)

(** Prints [1 sing] if [n = 1], [n plur] otherwise. *)
let pp_plural ~sing ~plur ft n =
  Fmt.pf ft "%d %s" n (if n = 1 then sing else plur)
