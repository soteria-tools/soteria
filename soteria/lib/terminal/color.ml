type color =
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

let to_progress (c : color) : Terminal_ansi.Color.t =
  (match c with
    | `Black -> `black
    | `Maroon -> `red
    | `Forest -> `green
    | `Orange -> `yellow
    | `DarkBlue -> `blue
    | `Purple -> `magenta
    | `Teal -> `cyan
    | `Silver -> `white
    | `Gray -> `bright `black
    | `Red -> `bright `red
    | `Green -> `bright `green
    | `Blue -> `bright `blue
    | `Cyan -> `bright `cyan
    | `Magenta -> `bright `magenta
    | `White -> `bright `white
    | `Yellow -> `bright `yellow)
  |> Progress.Color.ansi

let to_fmt : color -> Fmt.style = function
  | `Black -> `Fg `Black
  | `Maroon -> `Fg `Red
  | `Forest -> `Fg `Green
  | `Orange -> `Fg `Yellow
  | `DarkBlue -> `Fg `Blue
  | `Purple -> `Fg `Magenta
  | `Teal -> `Fg `Cyan
  | `Silver -> `Fg `White
  | `Gray -> `Fg (`Hi `Black)
  | `Red -> `Fg (`Hi `Red)
  | `Green -> `Fg (`Hi `Green)
  | `Blue -> `Fg (`Hi `Blue)
  | `Cyan -> `Fg (`Hi `Cyan)
  | `Magenta -> `Fg (`Hi `Magenta)
  | `White -> `Fg (`Hi `White)
  | `Yellow -> `Fg (`Hi `Yellow)

let to_fmt_style : style -> Fmt.style = function
  | `Bold -> `Bold
  | `Italic -> `Italic
  | `Underline -> `Underline

let pp_clr c = Fmt.styled (to_fmt c) Fmt.string
let pp_style s = Fmt.styled (to_fmt_style s) Fmt.string
let pp_clr2 c s = Fmt.styled (to_fmt c) (Fmt.styled (to_fmt_style s) Fmt.string)
let pp_ok = pp_clr2 `Green `Bold
let pp_err = pp_clr2 `Red `Bold
let pp_fatal = pp_clr2 `Maroon `Bold
