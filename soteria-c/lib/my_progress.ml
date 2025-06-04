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
  if not (Unix.isatty Unix.stderr) then None
  else
    let utf8 = supports_utf8_from_env () in
    let color = color_from_env () in
    Some { color; utf8 }

let profile = lazy (profile ())
let no_bar () = Option.is_none (Lazy.force profile)

let bar_style () =
  let open Progress.Line in
  match Lazy.force profile with
  | None -> None
  | Some { color; utf8 } ->
      let base = if utf8 then Bar_style.utf8 else Bar_style.ascii in
      let colored =
        if color then Bar_style.with_color (Progress.Color.ansi `cyan) base
        else base
      in
      Some (`Custom colored)

let bar_style = lazy (bar_style ())
let bar_style () = Lazy.force bar_style

let bar ~msg ~total =
  let open Progress.Line in
  let style = bar_style () in
  list [ spinner (); const msg; bar ?style total; count_to total ]

type _ Effect.t += Progress : int -> unit Effect.t

let signal_progress n = Effect.perform (Progress n)

let run ~msg ~total k =
  if no_bar () then
    try k () with effect Progress _, k -> Effect.Deep.continue k ()
  else
    Progress.with_reporter (bar ~msg ~total) (fun f ->
        Soteria_logs.Config.interject := Progress.interject_with;
        let res =
          try k ()
          with effect Progress n, k ->
            f n;
            Effect.Deep.continue k ()
        in
        let () = Soteria_logs.Config.interject := fun f -> f () in
        res)
