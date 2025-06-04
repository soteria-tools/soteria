let rec string_contains ~sub_str = function
  | "" -> sub_str = "" (* the empty string contains itself *)
  | s ->
      String.starts_with ~prefix:sub_str s
      || string_contains ~sub_str (String.sub s 1 (String.length s - 1))

let color_from_env () =
  let term = Sys.getenv_opt "TERM" in
  let color_term = Sys.getenv_opt "COLORTERM" in
  let is_term sub_str =
    term |> Option.map (string_contains ~sub_str) |> Option.value ~default:false
  in
  let is_256color = is_term "256color" in
  let is_color = is_term "color" in
  let is_ansi = is_term "ansi" in

  match (term, color_term) with
  | _, Some "true"
  | _, Some "truecolor"
  | Some ("xterm-kitty" | "wezterm"), _
  | Some "linux", _ ->
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
  Option.fold ~none:false ~some:(string_contains ~sub_str:"UTF-8") var

type profile = { color : bool; utf8 : bool }

let profile () =
  let utf8 = supports_utf8_from_env () in
  let color = color_from_env () in
  { color; utf8 }

let profile = lazy (profile ())

let bar_style ~color:user_color () =
  let open Progress.Line in
  let { color; utf8 } = Lazy.force profile in
  let base = if utf8 then Bar_style.utf8 else Bar_style.ascii in
  let colored =
    if color then Bar_style.with_color (Progress.Color.ansi user_color) base
    else base
  in
  Some (`Custom colored)

let bar ~color ~msg ~total =
  let open Progress.Line in
  let style = bar_style ~color () in
  list [ spinner (); const msg; bar ?style total; count_to total ]

type _ Effect.t += Progress : int -> unit Effect.t

let signal_progress n = Effect.perform (Progress n)

let run ?(color = `cyan) ~msg ~total () k =
  let config = Progress.Config.v () in
  (* This config will hide the bar by default stderr isn't a tty *)
  Progress.with_reporter ~config (bar ~color ~msg ~total) (fun f ->
      Soteria_logs.Config.interject := Progress.interject_with;
      let res =
        try k ()
        with effect Progress n, k ->
          f n;
          Effect.Deep.continue k ()
      in
      let () = Soteria_logs.Config.interject := fun f -> f () in
      res)
