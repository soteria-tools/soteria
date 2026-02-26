(** Cmdliner argument converters for file and directory paths. *)

open Cmdliner

(** Converter for file paths. Normalizes the path to an absolute path and checks
    if the file exists. *)
let file_as_absolute =
  let pp = Format.pp_print_string in
  let parser path =
    let path = Filename.normalise path in
    if Sys.file_exists path then Ok path
    else Error ("No such file or directory: " ^ path)
  in
  Arg.Conv.make ~completion:Arg.Completion.complete_files ~docv:"FILE" ~parser
    ~pp

(** Converter for directory paths. Normalizes the path to an absolute path and
    checks if the directory exists. *)
let dir_as_absolute =
  let pp = Format.pp_print_string in
  let parser path =
    let path = Filename.normalise path in
    if Sys.file_exists path then
      if Sys.is_directory path then Ok path
      else Error ("Not a directory " ^ path)
    else Error ("No such file or directory: " ^ path)
  in
  Arg.Conv.make ~completion:Arg.Completion.complete_dirs ~docv:"DIR" ~parser ~pp

(** Converter for file or directory paths. Normalizes the path to an absolute
    path and checks if it exists, returning either a file or a directory. *)
let file_or_dir_as_absolute =
  let parser path =
    let path = Filename.normalise path in
    if Sys.file_exists path then
      if Sys.is_directory path then Ok (`Dir path) else Ok (`File path)
    else Error ("No such file or directory: " ^ path)
  in
  let pp fmt = function
    | `File path -> Format.fprintf fmt "File: %s" (Filename.quote path)
    | `Dir path -> Format.fprintf fmt "Directory: %s" (Filename.quote path)
  in
  let completion = Arg.Completion.complete_paths in
  Arg.Conv.make ~docv:"PATH" ~parser ~pp ~completion ()

let regex =
  let parser s =
    try Ok (Str.regexp s)
    with Failure msg -> Error ("Invalid regular expression: " ^ msg)
  in
  let pp fmt _re = Format.fprintf fmt "<regex>" in
  Arg.Conv.make ~docv:"REGEX" ~parser ~pp ()
