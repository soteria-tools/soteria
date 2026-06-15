(** Extensions to [Stdlib.Sys] with utility functions. *)

include Stdlib.Sys

let with_working_dir dir f =
  let old_dir = getcwd () in
  chdir dir;
  Fun.protect ~finally:(fun () -> chdir old_dir) f

(** When used in Unix.kill, the "null signal" delivers nothing but still runs
    kill(2)'s error checks, so it reports whether a pid still names a process --
    the POSIX liveness probe. *)
let signull = 0
