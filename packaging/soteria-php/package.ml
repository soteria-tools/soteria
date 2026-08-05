open Shexp_process
open Stdlib (* Shexp overrides `List`... *)
open Cmdliner
open Packaging_common

module Copy_frontend = struct
  let copy_frontend source_dir dest_dir =
    let ( / ) = Filename.concat in
    let lower_script = source_dir / "bin" / "lower.php" in
    let vendor_dir = source_dir / "vendor" in
    if not (Sys.file_exists (vendor_dir / "autoload.php")) then
      failwith
        "PHP frontend dependencies are missing; run `composer install \
         --working-dir=soteria-php/frontend`";
    let () = run "rm" [ "-rf"; dest_dir ] |> eval in
    let () = run "mkdir" [ "-p"; dest_dir ] |> eval in
    let () =
      run "cp"
        [ source_dir / "composer.json"; source_dir / "composer.lock"; dest_dir ]
      |> eval
    in
    let () = run "cp" [ lower_script; dest_dir ] |> eval in
    let () = run "cp" [ "-rL"; vendor_dir; dest_dir ] |> eval in
    Printf.printf "Copied Soteria PHP frontend from %s to %s\n" source_dir
      dest_dir

  let source_dir_arg =
    Arg.(
      required
      & pos 0 (some dir) None
      & info [] ~docv:"SOURCE_DIR"
          ~doc:"Path to the Soteria PHP frontend source directory")

  let term =
    Term.(const copy_frontend $ source_dir_arg $ Common_lib.dest_dir_arg 1)

  let cmd = Cmd.v (Cmd.info "copy-frontend") term
end

let cmd =
  Cmd.group (Cmd.info "package")
    [
      Common_lib.Infer_Dylibs.cmd; Common_lib.Copy_files.cmd; Copy_frontend.cmd;
    ]

let () = exit @@ Cmd.eval cmd
