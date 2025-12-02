(** Extensions to [Stdlib.Sys] with utility functions. *)

include Stdlib.Sys

let with_working_dir dir f =
  let old_dir = getcwd () in
  chdir dir;
  let result = f () in
  chdir old_dir;
  result
