open Soteria_std

let color_from_env () =
  let term = Sys.getenv_opt "TERM" in
  let color_term = Sys.getenv_opt "COLORTERM" in
  let is_term sub_str =
    term |> Option.map (String.contains ~sub_str) |> Option.value ~default:false
  in
  let is_256color = is_term "256color" in
  let is_color = is_term "color" in
  let is_ansi = is_term "ansi" in

  match (term, color_term) with
  | _, Some ("true" | "truecolor")
  | Some ("xterm-kitty" | "wezterm" | "linux"), _ ->
      true
  | Some _, _ when is_256color || is_color || is_ansi -> true
  | _ -> false

let supports_utf8_from_env () =
  let var =
    Option.merge
      (fun f _ -> f)
      (Sys.getenv_opt "LC_CTYPE")
      (Sys.getenv_opt "LANG")
  in
  Option.fold ~none:false ~some:(String.contains ~sub_str:"UTF-8") var

type t = { color : bool; utf8 : bool }

let mk_profile () =
  let utf8 = supports_utf8_from_env () in
  let color = color_from_env () in
  { color; utf8 }

let default = { color = false; utf8 = false }
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Profile" ~default ()

let check_set_and_lock ?(no_color = false) () =
  let p = mk_profile () in
  let p = if no_color then { p with color = false } else p in
  set_and_lock p;
  Fmt.set_style_renderer Format.std_formatter
    (if p.color then `Ansi_tty else `None)
