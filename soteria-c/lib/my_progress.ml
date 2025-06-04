let bar ~msg ~total =
  let open Progress.Line in
  list [ spinner (); const msg; bar total; count_to total ]

type _ Effect.t += Progress : int -> unit Effect.t

let signal_progress n = Effect.perform (Progress n)

let run ~msg ~total k =
  if !Config.current.no_progress_bar then
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
