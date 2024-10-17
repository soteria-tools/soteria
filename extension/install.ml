open Import

let url =
  let computed = ref None in
  fun () ->
    match !computed with
    | Some url -> url
    | None ->
        (* We might have a complex computation involving github requests at some point.
           We only do it the first time. *)
        let url =
          "https://www.doc.ic.ac.uk/~sja3417/artifact/macos-latest-package.zip"
        in
        computed := Some url;
        url

let check_version storage_path =
  let dyld_lib_path =
    let name = "DYLD_LIBRARY_PATH" in
    let new_path = Filename.concat storage_path "/lib" in
    let path =
      match Node.Process.Env.get name with
      | None -> new_path
      | Some old_path -> new_path ^ ":" ^ old_path
    in
    (name, path)
  in
  let z3_path =
    let name = "BFA_Z3_PATH" in
    let path = storage_path ^ "/bin/z3" in
    (name, path)
  in
  let env = Interop.Dict.of_list [ dyld_lib_path; z3_path ] in
  let options = Node.ChildProcess.Options.create ~env () in
  let command = Filename.concat storage_path "/bin/bfa-c" in
  let args = [| "lsp"; "--version" |] in
  let+ Node.ChildProcess.{ stdout; exitCode; _ } =
    Node.ChildProcess.spawn ~options command args
  in
  let () =
    Import.show_message `Info "Version of installed bfa-c: %s (exit status: %d)"
      stdout exitCode
  in
  exitCode = 0
