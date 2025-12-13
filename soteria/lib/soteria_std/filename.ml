(** Extensions to [Stdlib.Filename] with utility functions. *)

include Stdlib.Filename

(** Normalize a path by converting it to an absolute path if it is relative. *)
let normalise path =
  if is_relative path then concat (Sys.getcwd ()) path else path
