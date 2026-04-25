include Unix

(** [fs_kind path] returns [`File] if the path is an existing file, [`Dir] if it
    is an existing directory, and [`Nonexistent] if no file exists at this path.
*)
let fs_kind path =
  match Sys.is_directory path with
  | true -> `Dir
  | false -> `File
  | exception Sys_error _ -> `Nonexistent

(** After calling [ensure_dir_exists path], [path] is guaranteed to be an
    existing directory (equivalent to [mkdir -p]).

    @raise Failure if [path] exists but is not a directory. *)
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
