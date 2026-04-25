include Unix

let fs_kind path =
  match Sys.is_directory path with
  | true -> `Dir
  | false -> `File
  | exception Sys_error _ -> `Nonexistent

let ensure_dir_exists path =
  let rec aux current =
    match (current, fs_kind current) with
    | ("." | ".." | "/"), _ -> ()
    | _, `File ->
        Fmt.failwith "Expected a directory, but found a file at: %s" current
    | _, `Nonexistent ->
        aux (Filename.dirname current);
        Unix.mkdir current 0o755
    | _, `Dir -> ()
  in
  aux path
