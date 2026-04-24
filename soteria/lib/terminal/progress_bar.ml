let to_progress (c : Logs.Color.t) : Terminal_ansi.Color.t =
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

let bar_style ~color:user_color () =
  let open Progress.Line in
  let { color; utf8 } : Logs.Profile.t = Logs.Profile.get () in
  let base = if utf8 then Bar_style.utf8 else Bar_style.ascii in
  let colored =
    if color then Bar_style.with_color (to_progress user_color) base else base
  in
  Some (`Custom colored)

let bar ~color ~msg ~total =
  let open Progress.Line in
  let style = bar_style ~color () in
  list [ spinner (); const msg; bar ?style total; count_to total ]

type _ Effect.t += Progress : int -> unit Effect.t

let signal_progress n = Effect.perform (Progress n)

let run ?(color = `Cyan) ~msg ~total () k =
  let config = Progress.Config.v () in
  (* This config will hide the bar by default stderr isn't a tty *)
  Progress.with_reporter ~config (bar ~color ~msg ~total) (fun f ->
      Logs.Config.with_interject ~interject:Progress.interject_with @@ fun () ->
      try k ()
      with effect Progress n, k ->
        f n;
        Effect.Deep.continue k ())
