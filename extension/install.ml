open Import

let archive_name () =
  match (Node.Process.arch, Node.Process.platform) with
  | "x64", "darwin" -> "macos-13-package" (* Intel MacOS *)
  | "arm64", "darwin" -> "macos-latest-package" (* Apple Silicon MacOS *)
  | _ -> raise (StopExtension "Unsupported platform")

let url =
  let computed = ref None in
  fun () ->
    match !computed with
    | Some url -> url
    | None ->
        (* We might have a complex computation involving github requests at some point.
           We only do it the first time. *)
        let url =
          "https://www.doc.ic.ac.uk/~sja3417/artifact/"
          ^ archive_name ()
          ^ ".zip"
        in
        computed := Some url;
        url

type executable = {
  command : string;
  args : string list;
  env : (string * string) list; [@default []]
}
[@@deriving make]

let executable ?(args = []) installed_path =
  let ( / ) = Filename.concat in
  let dyld_lib_path =
    let name = "DYLD_LIBRARY_PATH" in
    let new_path = installed_path / "lib" in
    let path =
      match Node.Process.Env.get name with
      | None -> new_path
      | Some old_path -> new_path ^ ":" ^ old_path
    in
    (name, path)
  in
  let z3_path =
    let name = "BFA_Z3_PATH" in
    let path = installed_path / "bin" / "z3" in
    (name, path)
  in
  let command = installed_path / "bin" / "bfa-c" in
  let args = "lsp" :: args in
  make_executable ~command ~args ~env:[ dyld_lib_path; z3_path ] ()

let check_version installed_path =
  let exe = executable ~args:[ "--version" ] installed_path in
  let env = Interop.Dict.of_list exe.env in
  let options = Node.ChildProcess.Options.create ~env () in
  let args = Array.of_list exe.args in
  let+ Node.ChildProcess.{ exitCode; stdout; stderr } =
    Node.ChildProcess.spawn ~options exe.command args
  in
  Logging.debug "Checked version: got %d: OUT: %s ERR: %s " exitCode stdout
    stderr;
  exitCode = 0

let install storage_path =
  let ( / ) = Filename.concat in
  let url = url () in
  let* () = Download_file.download_and_extract url storage_path in
  let* () =
    Download_file.rename
      (storage_path / archive_name ())
      (storage_path / "installed")
  in
  let* () =
    Download_file.make_executable (storage_path / "installed" / "bin" / "z3")
  in
  let* () =
    Download_file.make_executable (storage_path / "installed" / "bin" / "bfa-c")
  in
  let+ _ = check_version (storage_path / "installed") in
  ()
