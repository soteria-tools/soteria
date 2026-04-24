include Unix

let dir_exists path =
  match Sys.is_directory path with
  | true -> true
  | false -> Fmt.failwith "Expected a directory but found a file: %s" path
  | exception Sys_error _ -> false

let ensure_dir_exists path =
  let rec aux current =
    match current with
    | "." | ".." | "/" -> ()
    | other when dir_exists other -> ()
    | other ->
        aux (Filename.dirname other);
        Unix.mkdir current 0o755
  in
  aux path
