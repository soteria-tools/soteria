let pp_unstable ~name pp ft x =
  if (Config.get ()).hide_unstable then Fmt.pf ft "<%s>" name else pp ft x

let pp_time =
  let pp_time ft t =
    if t < 0.05 then Fmt.pf ft "%ams" (Fmt.float_dfrac 2) (t *. 1000.)
    else Fmt.pf ft "%as" (Fmt.float_dfrac 2) t
  in
  pp_unstable ~name:"time" pp_time

let pp_percent ft (total, part) =
  Fmt.pf ft "%a%%" (Fmt.float_dfrac 2) (100. *. part /. total)

let pp_plural ~sing ~plur ft n =
  Fmt.pf ft "%d %s" n (if n = 1 then sing else plur)

let pp_clr c = Fmt.styled (Color.to_fmt c) Fmt.string
let pp_style s = Fmt.styled (Color.to_fmt_style s) Fmt.string

let pp_clr2 c s =
  Fmt.styled (Color.to_fmt c) (Fmt.styled (Color.to_fmt_style s) Fmt.string)

let pp_ok = pp_clr2 `Green `Bold
let pp_warn = pp_clr2 `Yellow `Bold
let pp_err = pp_clr2 `Red `Bold
let pp_fatal = pp_clr2 `Maroon `Bold
