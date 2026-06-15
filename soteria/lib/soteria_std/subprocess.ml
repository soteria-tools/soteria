type t = {
  pid : int;
  stdin : out_channel;
  stdout : in_channel;
  stderr : in_channel;
  id : int;
  stopped : bool Atomic.t;
}

let stdin t = t.stdin
let stdout t = t.stdout
let stderr t = t.stderr

let close_channels t =
  try ignore (Unix.close_process_full (t.stdout, t.stdin, t.stderr))
  with _ -> ()

(* Registry of live subprocesses. A lock-free [Atomic] list so the signal
   handler can read it without risking a deadlock against a registry update it
   may have interrupted. *)
let live_processes : t list Atomic.t = Atomic.make []
let next_process_id = Atomic.make 0
let fresh_process_id () = Atomic.fetch_and_add next_process_id 1

let rec register_process t =
  let cur = Atomic.get live_processes in
  if not (Atomic.compare_and_set live_processes cur (t :: cur)) then
    register_process t

let rec unregister_process t =
  let cur = Atomic.get live_processes in
  let next = List.filter (fun t' -> t'.id <> t.id) cur in
  if not (Atomic.compare_and_set live_processes cur next) then
    unregister_process t

let stop_process t =
  if not (Atomic.exchange t.stopped true) then (
    unregister_process t;
    close_channels t)

let force_stop_process t =
  if not (Atomic.exchange t.stopped true) then (
    unregister_process t;
    (try Unix.kill t.pid Sys.sigkill with Unix.Unix_error _ -> ());
    close_channels t)

(* Force-kill every still-live subprocess. Idempotent and safe to call from a
   signal handler: each [force_stop_process] is guarded and any exception it
   raises is swallowed so one failure cannot strand the others. *)
let force_stop_all () =
  let processes = Atomic.exchange live_processes [] in
  List.iter (fun t -> try force_stop_process t with _ -> ()) processes

let () = at_exit force_stop_all

(* On SIGINT/SIGTERM the [at_exit] handlers do not run, so we install our own
   handler to reap the subprocesses, then restore the default action and
   re-raise so the process still terminates with the expected signal status. We
   only take over a signal that is still at its default disposition, to avoid
   clobbering a handler an embedding application may have installed -- and to
   leave SIGINT ignored for a background job, as the shell sets it. *)
let () =
  let handle signum =
    force_stop_all ();
    Sys.set_signal signum Sys.Signal_default;
    Unix.kill (Unix.getpid ()) signum
  in
  let install signum =
    match Sys.signal signum (Sys.Signal_handle handle) with
    | Sys.Signal_default -> ()
    | prev -> Sys.set_signal signum prev
  in
  install Sys.sigint;
  install Sys.sigterm

let open_ prog args =
  let proc = Unix.open_process_args_full prog args [||] in
  let pid = Unix.process_full_pid proc in
  let stdout, stdin, stderr = proc in
  let t =
    {
      pid;
      stdin;
      stdout;
      stderr;
      id = fresh_process_id ();
      stopped = Atomic.make false;
    }
  in
  register_process t;
  t
