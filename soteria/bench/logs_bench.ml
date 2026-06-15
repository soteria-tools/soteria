open Soteria.Logs.Import

let iters = 50_000_000

let bench name f =
  Gc.full_major ();
  let w0 = Gc.minor_words () in
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  let w1 = Gc.minor_words () in
  Printf.printf "%-22s %7.3fs  %14.0f words  %5.2f words/iter\n%!" name
    (t1 -. t0) (w1 -. w0)
    ((w1 -. w0) /. float_of_int iters)

let () =
  (* Disabled hot path: level Error, so Debug is below the threshold. *)
  Soteria.Logs.Config.(
    set_and_lock (make ~level:(Some Soteria.Logs.Level.Error) ()));
  Printf.printf "== logging DISABLED (Debug below threshold), %d iters ==\n"
    iters;
  bench "old closure 1 arg" (fun () ->
      for i = 1 to iters do
        L.debug (fun m -> m "value %d" i)
      done);
  bench "new ppx 1 arg" (fun () ->
      for i = 1 to iters do
        [%l.debug "value %d" i]
      done);
  bench "old closure 3 args" (fun () ->
      for i = 1 to iters do
        L.debug (fun m -> m "%d %d %d" i (i + 1) (i + 2))
      done);
  bench "new ppx 3 args" (fun () ->
      for i = 1 to iters do
        [%l.debug "%d %d %d" i (i + 1) (i + 2)]
      done)
