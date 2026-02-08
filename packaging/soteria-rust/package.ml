open Shexp_process
open Stdlib (* Shexp overrides `List`... *)
open Cmdliner
open Packaging_common

module Copy_soteria_rust_plugins = struct
  let copy_plugins dest_dir =
    let path = Stdlib.List.nth Soteria_rust_lib.Runtime_sites.Sites.plugins 0 in
    let () = run "mkdir" [ "-p"; dest_dir ] |> eval in
    let () = run "cp" [ "-r"; path ^ "/."; dest_dir ] |> eval in
    Printf.printf "Copied Soteria Rust plugins from %s to %s\n" path dest_dir

  let term = Term.(const copy_plugins $ Common_lib.dest_dir_arg 0)
  let cmd = Cmd.v (Cmd.info "copy-soteria-rust-plugins") term
end

(* Main command *)

let cmd =
  Cmd.group (Cmd.info "package")
    [
      Common_lib.Infer_Dylibs.cmd;
      Common_lib.Copy_files.cmd;
      Copy_soteria_rust_plugins.cmd;
    ]

let () = exit @@ Cmd.eval cmd
