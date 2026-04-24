open Shexp_process
open Stdlib (* Shexp overrides `List`... *)
open Cmdliner
open Packaging_common

module Copy_cerb_runtime = struct
  let copy_cerb_runtime dest_dir =
    let ( / ) = Filename.concat in
    let path = Cerb_runtime.in_runtime "" in
    let dest_dir = dest_dir / "cerberus-lib" / "runtime" in
    let () = run "mkdir" [ "-p"; dest_dir ] |> eval in
    let () = run "cp" [ "-r"; path; dest_dir ] |> eval in
    let () = run "rm" [ "-rf"; Filename.concat dest_dir "bmc" ] |> eval in
    Printf.printf "Copied Cerb runtime from %s to %s\n" path dest_dir

  let term = Term.(const copy_cerb_runtime $ Common_lib.dest_dir_arg 0)
  let cmd = Cmd.v (Cmd.info "copy-cerb-runtime") term
end

module Copy_soteria_c_auto_includes = struct
  let copy_includes dest_dir =
    let ( / ) = Filename.concat in
    let path =
      Stdlib.List.nth Soteria_c_lib.Auto_include_site.Sites.includes 0
    in
    let path = path / "soteria-c.h" in
    let dest_dir = dest_dir / "soteria-c" in
    let () = run "mkdir" [ "-p"; dest_dir ] |> eval in
    let () = run "cp" [ path; dest_dir ] |> eval in
    Printf.printf "Copied %s to %s\n" path dest_dir

  let term = Term.(const copy_includes $ Common_lib.dest_dir_arg 0)
  let cmd = Cmd.v (Cmd.info "copy-soteria-c-auto-includes") term
end

(* Main command *)

let cmd =
  Cmd.group (Cmd.info "package")
    [
      Common_lib.Infer_Dylibs.cmd;
      Common_lib.Copy_files.cmd;
      Copy_cerb_runtime.cmd;
      Copy_soteria_c_auto_includes.cmd;
    ]

let () = exit @@ Cmd.eval cmd
