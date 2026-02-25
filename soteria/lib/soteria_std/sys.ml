(** Extensions to [Stdlib.Sys] with utility functions. *)

include Stdlib.Sys

let with_working_dir dir f =
  let old_dir = getcwd () in
  chdir dir;
  Fun.protect ~finally:(fun () -> chdir old_dir) f
