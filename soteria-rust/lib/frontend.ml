open Charon
open Common.Charon_util
open Syntaxes.FunctionWrap

(** Something wrong internally with plugins *)
exception PluginError of string

(** Something wrong during compilation to ULLBC *)
exception FrontendError of string

(** Compilation of the code failed at the rustc level *)
exception CompilationError of string

let plugin_err msg = raise (PluginError msg)
let frontend_err msg = raise (FrontendError msg)
let compilation_err msg = raise (CompilationError msg)
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

  let current_rustc_flags () =
    let rustc = (Config.get ()).rustc_flags in
    let sysroot =
      match (Config.get ()).sysroot with
      | Some path -> [ "--sysroot=" ^ path ]
      | None -> []
    in
    rustc @ sysroot

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
          | Some test -> [ "--test"; test ]
          | None -> []
        in
        let rustc = flags_for_cargo rustc in
        let env = rustc_as_env () @ flags_as_rustc_env rustc in
        (cmd, ("cargo" :: args) @ [ "--" ] @ cargo, env)

  let exec_in ~mode folder cmd =
    let cmd, args, env = build_cmd ~mode cmd in
    let@ () = Exe.run_in folder in
    Exe.exec ~env cmd args
end

module Lib = struct
  let target =
    lazy
      (match (Config.get ()).target with
      | Some t -> t
      | None -> (
          let env = Cmd.rustc_as_env () in
          let info = Exe.exec_exn ~env (Cmd.cargo ()) [ "-vV" ] in
          match List.find_opt (String.starts_with ~prefix:"host") info with
          | Some s -> String.sub s 6 (String.length s - 6)
          | None -> plugin_err "Couldn't find target host"))

  let root =
    lazy
      (match
         ((Config.get ()).plugin_directory, Runtime_sites.Sites.plugins)
       with
      | Some root, _ -> root
      | None, root :: _ -> root
      | None, [] -> plugin_err "Couldn't find plugin directory")

  type t = Std | Kani | Miri

  let name = function Std -> "std" | Kani -> "kani" | Miri -> "miri"
  let path lib = Lazy.force root / name lib

  (** [exec_cargo ?env lib args] executes the command [cargo <args>] for the
      library [lib].

      @raise PluginError
        if the command fails, with the error message from Cargo. *)
  let exec_cargo ?(env = []) lib args =
    let path = path lib in
    let env = Cmd.rustc_as_env () @ env in
    let _, err, status =
      let@ () = Exe.run_in path in
      Exe.exec ~env (Cmd.cargo ()) args
    in
    match status with
    | WEXITED (0 | 255) -> ()
    | _ ->
        Fmt.kstr plugin_err "Couldn't compile lib at %s@.%a" path
          Fmt.(list string)
          err

  (** Deletes the target directory of a target, to avoid duplicate builds *)
  let clean lib = exec_cargo lib [ "clean" ]

  let compile lib =
    if not (Config.get ()).no_compile_plugins then
      let env =
        Cmd.(current_rustc_flags () |> flags_for_cargo |> flags_as_rustc_env)
      in
      let args =
        [ "build"; "--lib"; "--target"; Lazy.force target ]
        @ if (Config.get ()).log_compilation then [ "--verbose" ] else []
      in
      exec_cargo ~env lib args

  let with_compiled lib f =
    let path = path lib in
    let target = Lazy.force target in
    compile lib;
    let config : Cmd.t = f (path, target) in
    let lib_imports =
      [
        "-L" ^ (path / "target" / target / "debug" / "deps");
        "-L" ^ (path / "target" / "debug" / "deps");
      ]
    in
    { config with rustc = config.rustc @ lib_imports }
end

type entry_point_filter = {
  filter : Cmd.entry list;
  expect_error : Cmd.entry list;
}

type entry_point = {
  fun_decl : UllbcAst.fun_decl;
  expect_error : bool;
  fuel : Soteria.Symex.Fuel_gauge.t;
}

type mk_cmd = ?input:string -> output:string -> unit -> Cmd.t

let default () =
  let@ std_lib_path, target = Lib.with_compiled Std in
  let opaque_names =
    List.concat_map (fun n -> [ "--opaque"; n ]) Builtins.Eval.opaque_names
  in
  Cmd.make
    ~charon:
      ([
         "--ullbc";
         "--extract-opaque-bodies";
         "--mir elaborated";
         "--reconstruct-fallible-operations";
         "--reconstruct-asserts";
         "--desugar-drops";
         "--precise-drops";
       ]
      @ opaque_names
      @ if (Config.get ()).polymorphic then [] else [ "--monomorphize" ])
    ~obol:opaque_names
    ~entry_points:[ Name "main"; Attrib "soteriatool::test" ]
    ~expect_error:[ Attrib "soteriatool::expect_fail" ]
    ~features:[ "soteria" ]
    ~rustc:
      [
        (* i.e. not always a binary! *)
        "--crate-type=lib";
        "-Z";
        "unstable-options";
        (* No warning *)
        "-Awarnings";
        "--cap-lints=allow";
        (* include our std and soteria crates *)
        "-Z";
        "crate-attr=feature(register_tool)";
        "-Z";
        "crate-attr=register_tool(soteriatool)";
        "--extern";
        "soteria";
        (* include the std *)
        "--extern";
        "noprelude:std="
        ^ (std_lib_path / "target" / target / "debug" / "libstd.rlib");
      ]
    ()

let kani () =
  let@ _ = Lib.with_compiled Kani in
  Cmd.make ~features:[ "kani" ]
    ~entry_points:[ Attrib "kanitool::proof" ]
    ~expect_error:[ Attrib "kanitool::should_panic" ]
    ~rustc:[ "-Z"; "crate-attr=register_tool(kanitool)"; "--extern"; "kani" ]
    ()

let miri () =
  let@ _ = Lib.with_compiled Miri in
  Cmd.make ~features:[ "miri" ]
    ~rustc:[ "--extern"; "miristd"; "--edition"; "2021" ]
    ~entry_points:[ Name "miri_start" ] ()

(** Filters a name, according to the current {!Config.t.filter} and
    {!Config.t.exclude} settings. If there are no filters, all names are
    included; otherwise, a name is included if it matches any filter and doesn't
    match any exclude. *)
let filter_name crate name =
  let fmt_env = PrintUllbcAst.Crate.crate_to_fmt_env crate in
  let name = PrintTypes.name_to_string fmt_env name in

  let filters = (Config.get ()).filter in
  let excludes = (Config.get ()).exclude in

  let any_contains =
    List.exists (fun r ->
        try Str.search_forward r name 0 >= 0 with Not_found -> false)
  in
  (List.is_empty filters || any_contains filters) && not (any_contains excludes)

let get_entry_point (filter : entry_point_filter) crate
    (decl : UllbcAst.fun_decl) =
  let ( let*! ) b f = if b then f () else None in
  (* check it's a valid entry-point *)
  let*! () =
    List.is_empty filter.filter
    || List.exists (Cmd.entry_matches_fn decl) filter.filter
  in
  (* check it's filtered *)
  let*! () = filter_name crate decl.item_meta.name in
  (* build the entry point *)
  let expect_error =
    List.exists (Cmd.entry_matches_fn decl) filter.expect_error
  in
  let open Soteria.Symex in
  let fuel : Fuel_gauge.t =
    let get_or name default : Fuel_gauge.Fuel_value.t =
      match (decl_get_attr decl name, default) with
      | Some fuel, _ -> Finite (int_of_string fuel)
      | None, Some fuel -> Finite fuel
      | None, None -> Infinite
    in
    {
      steps = get_or "soteriatool::step_fuel" (Config.get ()).step_fuel;
      branching = get_or "soteriatool::branch_fuel" (Config.get ()).branch_fuel;
    }
  in
  Some { fun_decl = decl; expect_error; fuel }

let create_using_current_config () : mk_cmd * entry_point_filter =
  let config = Config.get () in
  let cmd_parts =
    [ default () ]
    @ (if config.with_kani then [ kani () ] else [])
    @ if config.with_miri then [ miri () ] else []
  in
  let cmd = List.fold_left Cmd.concat_cmd Cmd.empty cmd_parts in
  let cmd =
    match config.test with
    | None -> cmd
    | Some _ ->
        (* HACK: if we're in test mode, we want to ignore main because Rust will
           compile it in a quirky way and it requires having a sysroot. Instead
           we want to look for the tests directly! So we add #[test] *)
        let entry_points =
          Cmd.Attrib "test" :: cmd.entry_points
          |> List.filter (function Cmd.Pub | Name "main" -> false | _ -> true)
        in
        let expect_error = Cmd.Attrib "should_panic" :: cmd.expect_error in
        { cmd with entry_points; expect_error }
  in
  let mk_cmd =
   fun ?input ~output () ->
    let input =
      Option.fold ~none:[] ~some:(fun s -> [ Filename.quote s ]) input
    in
    Cmd.make
      ~charon:[ "--dest-file"; Filename.quote output ]
      ~obol:[ "--dest-file"; Filename.quote output ]
      ~rustc:input ()
    |> Cmd.concat_cmd cmd
  in
  (mk_cmd, { filter = cmd.entry_points; expect_error = cmd.expect_error })

let modify_mk_cmd f mk_cmd =
 fun ?input ~output () -> f @@ mk_cmd ?input ~output ()

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc ~mode ~cmd ~output ~pwd () =
  if not (Config.get ()).no_compile then (
    let _, err, res = Cmd.exec_in ~mode pwd cmd in
    if not (Exe.is_ok res) then
      if res = WEXITED 2 then compilation_err (String.concat "\n" err)
      else
        Fmt.kstr frontend_err "Failed compilation to ULLBC:@,%a"
          Fmt.(list ~sep:(any "\n") string)
          err;
    Cleaner.touched output);
  let crate =
    match
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json
    with
    | Ok crate -> crate
    | Error _ ->
        Fmt.kstr frontend_err
          "Failed to parse ULLBC. Do you have the right version of %a \
           installed?"
          Config.pp_frontend (Config.get ()).frontend
    | exception Sys_error _ -> frontend_err "File doesn't exist"
    | exception e -> Fmt.kstr frontend_err "Unexpected error: %a" Fmt.exn e
  in
  if (Config.get ()).output_crate then (
    (* save pretty-printed crate to local file *)
    let crate_file = Printf.sprintf "%s.crate" output in
    let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
    let oc = open_out_bin crate_file in
    output_string oc str;
    close_out oc;
    Cleaner.touched crate_file);
  crate

let with_entry_points ~filter (crate : Charon.UllbcAst.crate) =
  let entry_points =
    Charon.Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map (get_entry_point filter crate)
  in
  (crate, entry_points)

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~(mk_cmd : mk_cmd) file_name =
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let cmd = mk_cmd ~input:file_name ~output () in
  parse_ullbc ~mode:Rustc ~cmd ~output ~pwd:parent_folder ()

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_crate ~(mk_cmd : mk_cmd) crate_dir =
  let filename =
    match (Config.get ()).test with
    | Some test -> "test-" ^ test ^ ".llbc.json"
    | None -> "crate.llbc.json"
  in
  let output = crate_dir / filename in
  let cmd = mk_cmd ~output () in
  parse_ullbc ~mode:Cargo ~cmd ~output ~pwd:crate_dir ()

let parse_ullbc_raw ~mk_cmd = function
  | `File file -> parse_ullbc_of_file ~mk_cmd file
  | `Dir path -> parse_ullbc_of_crate ~mk_cmd path

(** Given a path, if it's a file will parse the ULLBC of that single file using
    rustc; otherwise will assume it's a path to a crate and use cargo.
    {b Will translate all functions in the crate, without filtering
       entry-points.} *)
let parse_ullbc path =
  let mk_cmd, _filter = create_using_current_config () in
  let mk_cmd =
    modify_mk_cmd (fun c -> Cmd.{ c with entry_points = [] }) mk_cmd
  in
  parse_ullbc_raw ~mk_cmd path

(** Given a path, will check if it has a [.rs] extension, in which case it will
    parse the ULLBC of that single file using rustc; otherwise will assume it's
    a path to a crate and use cargo.
    {b Will only start translation from functions considered entry-points.} *)
let parse_ullbc_with_entry_points path =
  let mk_cmd, filter = create_using_current_config () in
  parse_ullbc_raw path ~mk_cmd |> with_entry_points ~filter

let compile_all_plugins () =
  List.iter
    (fun l ->
      Lib.clean l;
      Lib.compile l)
    [ Std; Kani; Miri ]
