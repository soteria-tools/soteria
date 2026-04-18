open Charon
open Common.Charon_util
open Syntaxes.FunctionWrap

(** Something wrong during compilation to ULLBC *)
exception FrontendError of string

let frontend_err msg = raise (FrontendError msg)
let ( / ) = Filename.concat

(** Utilities to run commands *)
module Exe = struct
  let run_in path f =
    let pwd = Unix.getcwd () in
    Fun.protect
      ~finally:(fun () -> Unix.chdir pwd)
      (fun () ->
        Unix.chdir path;
        if (Config.get ()).log_compilation then
          [%l.info "Changed working directory to %s" path];
        f ())

  let pp_status ft = function
    | Unix.WEXITED 0 -> Fmt.string ft "Exited successfully"
    | Unix.WEXITED n -> Fmt.pf ft "Exit(%d)" n
    | Unix.WSIGNALED n -> Fmt.pf ft "Killed(%d)" n
    | Unix.WSTOPPED n -> Fmt.pf ft "Stopped(%d)" n

  let is_ok = function Unix.WEXITED 0 -> true | _ -> false

  (** Read from both channels simultaneously to avoid blocking *)
  let read_both_nonblocking out err =
    let out_fd = Unix.descr_of_in_channel out in
    let err_fd = Unix.descr_of_in_channel err in

    let buffer_size = 4096 in
    let buf = Bytes.create buffer_size in
    let out_buffer = Buffer.create buffer_size in
    let err_buffer = Buffer.create buffer_size in
    let out_open = ref true in
    let err_open = ref true in

    while !out_open || !err_open do
      (* Build the list of open file descriptors *)
      let fd_to_read =
        (if !out_open then [ out_fd ] else [])
        @ if !err_open then [ err_fd ] else []
      in
      (* Wait until one of the file descriptor is ready to be read. Note: the
         -1.0 means that there is no timeout, we wait as long as needed.*)
      let ready, _, _ = Unix.select fd_to_read [] [] (-1.0) in
      List.iter
        (fun fd ->
          let n = Unix.read fd buf 0 buffer_size in
          if n = 0 then
            (* EOF, stop reading from that channel *)
            if fd = out_fd then out_open := false else err_open := false
          else
            (* Data available *)
            let chunk = Bytes.sub_string buf 0 n in
            if fd = out_fd then Buffer.add_string out_buffer chunk
            else Buffer.add_string err_buffer chunk)
        ready
    done;

    (* Split into lines *)
    let output = Buffer.contents out_buffer |> String.split_on_char '\n' in
    let error = Buffer.contents err_buffer |> String.split_on_char '\n' in
    (output, error)

  let exec ?(env = []) cmd args =
    (* let args = Array.of_list args in *)
    let current_env = Unix.environment () in
    let cmd = String.concat " " (cmd :: args) in
    if (Config.get ()).log_compilation then
      [%l.info
        "Running command: %s@.With env:@.%a@." cmd
          Fmt.(list ~sep:(any "@.") string)
          env];
    let env = Array.append current_env (Array.of_list env) in
    let out, inp, err = Unix.open_process_full cmd env in
    let output, error = read_both_nonblocking out err in
    let status = Unix.close_process_full (out, inp, err) in
    if (Config.get ()).log_compilation then
      [%l.info
        "Command finished with status: %a@.stdout:@.%a@.stderr:@.%a" pp_status
          status
          Fmt.(list ~sep:(any "@\n") string)
          output
          Fmt.(list ~sep:(any "@\n") string)
          error];
    (output, error, status)

  let exec_exn ?env cmd args =
    let output, _, status = exec ?env cmd args in
    (if not (is_ok status) then
       match status with
       | WEXITED 127 ->
           Fmt.kstr frontend_err
             "Command %s %a not found; do you have %a installed?" cmd
             Fmt.(list ~sep:(any " ") string)
             args Config.pp_frontend (Config.get ()).frontend
       | _ ->
           Fmt.kstr frontend_err "Command %s %a failed with status %a" cmd
             Fmt.(list ~sep:(any " ") string)
             args pp_status status);
    output
end

(** Simple utility to create and then delete files *)
module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
  let () = at_exit (fun () -> if (Config.get ()).cleanup then cleanup ())
end

(** Organise commands to send to the Soteria-Rust frontend *)
module Cmd = struct
  type entry = Attrib of string | Name of string | Pub

  let entry_as_flag = function
    | Attrib a -> [ "--start-from-attribute=" ^ a ]
    | Name n -> [ "--start-from"; n ]
    | Pub -> [ "--start-from-pub" ]

  let entry_matches_fn (fn : UllbcAst.fun_decl) = function
    | Attrib attrib -> decl_has_attr fn attrib
    | Name n -> (
        match List.last_opt fn.item_meta.name with
        | Some (PeIdent (name, _)) -> name = n
        | _ -> false)
    | Pub -> fn.item_meta.is_local && fn.item_meta.attr_info.public

  type t = {
    charon : string list; [@default []]
        (** Arguments passed to Charon (only when using the [Charon] frontend)
        *)
    obol : string list; [@default []]
        (** Arguments passed to Obol (only when using the [Obol] frontend) *)
    features : string list; [@default []]
        (** Features to enable for compilation (as in --cfg) *)
    rustc : string list; [@default []]
        (** DEPRECATED?: rustc flags. For Cargo we use RUSTFLAGS, but when
            possible it would be nicer to use the Cargo-specific command (as
            with features)? *)
    entry_points : entry list; [@default []]
        (** Functions to mark as entry points, e.g.
            [Attrib "soteriatool::test"], when we are interested in filtering
            the entry-points. *)
    expect_error : entry list; [@default []]
        (** Markers to know that an entry point is expected to fail. This is
            used to inverse the outcomes of the execution, so that we can use
            the same plugin for both expected-success and expected-failure
            tests. *)
  }
  [@@deriving make]

  type mode = Cargo | Rustc

  let empty = make ()

  let concat_cmd c1 c2 =
    {
      charon = c1.charon @ c2.charon;
      obol = c1.obol @ c2.obol;
      features = c1.features @ c2.features;
      rustc = c1.rustc @ c2.rustc;
      entry_points = c1.entry_points @ c2.entry_points;
      expect_error = c1.expect_error @ c2.expect_error;
    }

  let frontend_cmd () =
    match (Config.get ()).frontend with
    | Obol -> (Config.get ()).obol_path
    | Charon -> (Config.get ()).charon_path

  let toolchain_path =
    lazy
      (let cmd = frontend_cmd () in
       Exe.exec_exn cmd [ "toolchain-path" ] |> List.hd)

  let cargo () = Lazy.force toolchain_path / "bin" / "cargo"
  let rustc () = Lazy.force toolchain_path / "bin" / "rustc"
  let rustc_as_env () = [ "RUSTC=" ^ rustc () ]
  let split_on_space s = String.split_on_char ' ' s |> List.filter (( <> ) "")

  let current_rustc_flags () =
    let rustc = (Config.get ()).rustc_flags in
    let sysroot =
      match (Config.get ()).sysroot with
      | Some path -> [ "--sysroot"; path ]
      | None -> []
    in
    rustc @ sysroot

  let cargo_flags () = (Config.get ()).cargo_flags
  let is_crate_type_flag = String.starts_with ~prefix:"--crate-type"
  let is_edition_flag = String.starts_with ~prefix:"--edition"

  let flags_for_cargo =
    (* Cargo already specifies the edition and the crate type! *)
    List.filter (fun s -> not (is_edition_flag s || is_crate_type_flag s))

  let flags_as_rustc_env args =
    if List.is_empty args then [] else [ "RUSTFLAGS=" ^ String.concat " " args ]

  let build_cmd ~mode
      { charon; obol; features; rustc; entry_points; expect_error = _ } =
    let features = List.concat_map (fun f -> [ "--cfg"; f ]) features in
    let user_specified = current_rustc_flags () in
    let rustc =
      if List.exists is_crate_type_flag user_specified then
        List.filter (Fun.negate is_crate_type_flag) rustc
      else rustc
    in
    let rustc = rustc @ user_specified @ features in
    let cmd, args =
      match (Config.get ()).frontend with
      | Obol ->
          let entries = List.concat_map entry_as_flag entry_points in
          ((Config.get ()).obol_path, obol @ entries)
      | Charon ->
          (* FIXME: PR Charon to change this! *)
          let attribs, non_attribs =
            List.partition
              (function Attrib _ -> true | _ -> false)
              entry_points
          in
          let attribs =
            match attribs with
            | [] -> []
            | [ h ] -> [ h ]
            | h :: _ ->
                [%l.warn
                  "Charon currently only support one entry attribute; more \
                   than one was specified, only the first will be used"];
                [ h ]
          in
          let entries = List.concat_map entry_as_flag (attribs @ non_attribs) in
          ((Config.get ()).charon_path, charon @ entries)
    in
    match mode with
    | Rustc ->
        (* If these arguments are passed to the command line, we need to quote
           them appropriately (since crate-attr) has parenthesis. We don't need
           to do this for Cargo since they go in the environment, and adding
           quotes there would make them wrong! This is lovely! *)
        let rustc =
          List.map
            (fun arg ->
              if String.starts_with ~prefix:"crate-attr" arg then
                "\"" ^ arg ^ "\""
              else arg)
            rustc
        in
        (cmd, ("rustc" :: args) @ [ "--" ] @ rustc, [])
    | Cargo ->
        let cargo =
          match (Config.get ()).test with
          | Some "lib" -> [ "--lib" ]
          | Some test -> [ "--test"; test ]
          | None -> []
        in
        let cargo = cargo @ cargo_flags () in
        let rustc = flags_for_cargo rustc in
        let env = rustc_as_env () @ flags_as_rustc_env rustc in
        (cmd, ("cargo" :: args) @ [ "--" ] @ cargo, env)

  let exec_in ~mode folder cmd =
    let cmd, args, env = build_cmd ~mode cmd in
    let@ () = Exe.run_in folder in
    Exe.exec ~env cmd args
end
