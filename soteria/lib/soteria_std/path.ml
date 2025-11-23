(** Path normalization utilities. *)

let normalise path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path
