type t =
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

let to_fmt : t -> Fmt.style = function
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
