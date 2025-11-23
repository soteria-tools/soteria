(** Extensions to [Stdlib.Sys] with helpers for working directories. *)

include Stdlib.Sys

let with_working_dir dir f =
  let old_dir = getcwd () in
  chdir dir;
  let result = f () in
  chdir old_dir;
  result
