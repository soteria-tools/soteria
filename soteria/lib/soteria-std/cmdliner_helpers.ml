let file_as_absolute =
  let pp = Format.pp_print_string in
  let parse path =
    let path = Path.normalise path in
    if Sys.file_exists path then Ok path
    else Error ("No such file or directory: " ^ path)
  in
  Cmdliner.Arg.conv' ~docv:"FILE" (parse, pp)

let dir_as_absolute =
  let pp = Format.pp_print_string in
  let parse path =
    let path = Path.normalise path in
    if Sys.file_exists path then
      if Sys.is_directory path then Ok path
      else Error ("Not a directory " ^ path)
    else Error ("No such file or directory: " ^ path)
  in
  Cmdliner.Arg.conv' ~docv:"DIR" (parse, pp)
