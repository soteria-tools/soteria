let bar_style ~color:user_color () =
  let open Progress.Line in
  let { color; utf8 } : Profile.t = !Profile.profile in
  let base = if utf8 then Bar_style.utf8 else Bar_style.ascii in
  let colored =
    if color then Bar_style.with_color (Color.to_progress user_color) base
    else base
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
      Soteria_logs.Config.interject := Progress.interject_with;
      let res =
        try k ()
        with effect Progress n, k ->
          f n;
          Effect.Deep.continue k ()
      in
      let () = Soteria_logs.Config.interject := fun f -> f () in
      res)
