open Test_register

(* When the test executable is re-spawned with [SUBPROCESS_TEST_MODE] set it
   acts as a helper rather than running the suite: it opens a tracked subprocess
   (a [sleep] that does not read its stdin, so closing our end never reaches it)
   and then either exits -- to exercise the [at_exit] cleanup -- or blocks
   reading the child -- to exercise the SIGTERM cleanup. In both cases the
   grandchild must be reaped by {!Subprocess}, never orphaned. The helper relays
   the grandchild's pid on its own stdout so the parent can check it. *)
let () =
  match Sys.getenv_opt "SUBPROCESS_TEST_MODE" with
  | None -> ()
  | Some mode ->
      let child =
        Subprocess.open_ "/bin/sh"
          [| "/bin/sh"; "-c"; "echo $$; exec sleep 30" |]
      in
      print_string (input_line (Subprocess.stdout child));
      print_newline ();
      flush stdout;
      (match mode with
      | "block" -> ignore (input_line (Subprocess.stdout child))
      | _ (* "exit" *) -> ());
      exit 0

let register = register "Subprocess"

(* Whether [pid] still names a live process, used to tell whether a child has
   been reaped. *)
let process_alive pid =
  match Unix.kill pid Sys.signull with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | exception Unix.Unix_error (Unix.EPERM, _, _) -> true

let wait_until ?(timeout = 10.0) f =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if f () then true
    else if Unix.gettimeofday () > deadline then false
    else (
      Unix.sleepf 0.01;
      loop ())
  in
  loop ()

(* A subprocess whose [echo $$] prints its own pid as the first stdout line. *)
let read_pid child =
  int_of_string (String.trim (input_line (Subprocess.stdout child)))

let open_echo_pid script =
  let child =
    Subprocess.open_ "/bin/sh" [| "/bin/sh"; "-c"; "echo $$; " ^ script |]
  in
  (child, read_pid child)

(* {1 Channels} *)

let stdin_is_piped_to_stdout =
  let@ () = register "open_ connects stdin and stdout" in
  let child = Subprocess.open_ "/bin/cat" [| "/bin/cat" |] in
  let roundtrip line =
    output_string (Subprocess.stdin child) (line ^ "\n");
    flush (Subprocess.stdin child);
    input_line (Subprocess.stdout child)
  in
  Alcotest.(check string) "echoes first line" "hello" (roundtrip "hello");
  Alcotest.(check string) "echoes second line" "world" (roundtrip "world");
  Subprocess.stop_process child

let stderr_is_readable =
  let@ () = register "open_ connects stderr" in
  let child =
    Subprocess.open_ "/bin/sh" [| "/bin/sh"; "-c"; "echo oops 1>&2" |]
  in
  Alcotest.(check string)
    "stderr line" "oops"
    (input_line (Subprocess.stderr child));
  Subprocess.stop_process child

(* {1 Stopping} *)

let stop_process_reaps =
  let@ () = register "stop_process closes and reaps the process" in
  (* [cat] would block forever on stdin; closing it (via [stop_process]) gives
     it EOF, so it exits and is reaped. *)
  let child, pid = open_echo_pid "exec cat" in
  Alcotest.(check bool) "alive before stop" true (process_alive pid);
  Subprocess.stop_process child;
  Alcotest.(check bool)
    "reaped after stop" true
    (wait_until (fun () -> not (process_alive pid)))

let force_stop_kills_unresponsive =
  let@ () =
    register "force_stop_process kills a process that ignores its stdin"
  in
  (* [sleep] never reads stdin, so a graceful stop could not reap it for 30s;
     [force_stop_process] must actively kill it. *)
  let child, pid = open_echo_pid "exec sleep 30" in
  Alcotest.(check bool) "alive before force_stop" true (process_alive pid);
  Subprocess.force_stop_process child;
  Alcotest.(check bool)
    "killed by force_stop" true
    (wait_until (fun () -> not (process_alive pid)))

let force_stop_overrides_sigterm =
  let@ () = register "force_stop_process kills a process that traps SIGTERM" in
  let child, pid = open_echo_pid "trap '' TERM INT; while : ; do : ; done" in
  Alcotest.(check bool) "alive initially" true (process_alive pid);
  (* It traps SIGTERM, so a plain TERM leaves it running. *)
  (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
  Unix.sleepf 0.1;
  Alcotest.(check bool) "survives SIGTERM" true (process_alive pid);
  (* [force_stop_process] uses SIGKILL, which cannot be trapped. *)
  Subprocess.force_stop_process child;
  Alcotest.(check bool)
    "killed by force_stop" true
    (wait_until (fun () -> not (process_alive pid)))

(* {1 Idempotency} *)

let stop_process_is_idempotent =
  let@ () = register "stop_process is idempotent" in
  let child, pid = open_echo_pid "exec cat" in
  Subprocess.stop_process child;
  (* Repeated stops -- and a follow-up force_stop -- must not raise nor act on a
     possibly-reused pid. *)
  Subprocess.stop_process child;
  Subprocess.force_stop_process child;
  Alcotest.(check bool)
    "still reaped" true
    (wait_until (fun () -> not (process_alive pid)))

let force_stop_process_is_idempotent =
  let@ () = register "force_stop_process is idempotent" in
  let child, pid = open_echo_pid "exec sleep 30" in
  Subprocess.force_stop_process child;
  Subprocess.force_stop_process child;
  Subprocess.stop_process child;
  Alcotest.(check bool)
    "still reaped" true
    (wait_until (fun () -> not (process_alive pid)))

(* {1 Cleanup on exit and on signals}

   These re-spawn the test executable in helper mode (see the dispatch at the
   top of this file) and check that a grandchild the helper never stops is
   reaped when the helper exits ([at_exit]) or is killed by SIGTERM. *)

let spawn_helper mode =
  let exe = Sys.executable_name in
  let env =
    Array.append (Unix.environment ()) [| "SUBPROCESS_TEST_MODE=" ^ mode |]
  in
  Unix.open_process_args_full exe [| exe |] env

let at_exit_reaps_subprocess =
  let@ () = register "at_exit reaps live subprocesses on normal exit" in
  let ((ic, _, _) as proc) = spawn_helper "exit" in
  let grandchild = int_of_string (String.trim (input_line ic)) in
  (* Wait for the helper to finish, so its [at_exit] handler has run. *)
  ignore (Unix.close_process_full proc);
  let reaped = wait_until (fun () -> not (process_alive grandchild)) in
  (* Never leave a stray process behind, even if the assertion is about to
     fail. *)
  (if not reaped then try Unix.kill grandchild Sys.sigkill with _ -> ());
  Alcotest.(check bool) "grandchild reaped at exit" true reaped

let sigterm_reaps_subprocess =
  let@ () = register "SIGTERM reaps live subprocesses while blocked" in
  let ((ic, _, _) as proc) = spawn_helper "block" in
  let helper = Unix.process_full_pid proc in
  let grandchild = int_of_string (String.trim (input_line ic)) in
  Unix.kill helper Sys.sigterm;
  let reaped = wait_until (fun () -> not (process_alive grandchild)) in
  (* Tear everything down unconditionally so a regression cannot hang or
     leak. *)
  (try Unix.kill grandchild Sys.sigkill with _ -> ());
  (try Unix.kill helper Sys.sigkill with _ -> ());
  ignore (Unix.close_process_full proc);
  Alcotest.(check bool) "grandchild reaped on SIGTERM" true reaped
