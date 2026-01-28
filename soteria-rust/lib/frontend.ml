open Charon
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

(** Utilities to run commands *)
module Exe = struct
  let run_in path f =
    let pwd = Unix.getcwd () in
    Fun.protect
      ~finally:(fun () -> Unix.chdir pwd)
      (fun () ->
        Unix.chdir path;
        if (Config.get ()).log_compilation then
          L.app (fun g -> g "Changed working directory to %s" path);
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
      (* Wait until one of the file descriptor is ready to be read.
         Note: the -1.0 means that there is no timeout, we wait as long as needed.*)
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
    let env = Array.append current_env (Array.of_list env) in
    let cmd = String.concat " " (cmd :: args) in
    if (Config.get ()).log_compilation then
      L.app (fun g -> g "Running command: %s" cmd);
    let out, inp, err = Unix.open_process_full cmd env in
    let output, error = read_both_nonblocking out err in
    let status = Unix.close_process_full (out, inp, err) in
    if (Config.get ()).log_compilation then
      L.app (fun g ->
          g "Command finished with status: %a@.stdout:@.%a@.stderr:@.%a"
            pp_status status
            Fmt.(list ~sep:(any "@\n") string)
            output
            Fmt.(list ~sep:(any "@\n") string)
            error);
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
  type t = {
    charon : string list; [@default []]
        (** Arguments passed to Charon (only when not in [Obol] mode) *)
    obol : string list; [@default []]
        (** Arguments passed to Obol (only in [Obol] mode) *)
    features : string list; [@default []]
        (** Features to enable for compilation (as in --cfg) *)
    rustc : string list; [@default []]
        (** DEPRECATED?: rustc flags. For Cargo we use RUSTFLAGS, but when
            possible it would be nicer to use the Cargo-specific command (as
            with features)? *)
  }
  [@@deriving make]

  type mode = Cargo | Rustc

  let empty_cmd = make ()

  let concat_cmd c1 c2 =
    {
      charon = c1.charon @ c2.charon;
      obol = c1.obol @ c2.obol;
      features = c1.features @ c2.features;
      rustc = c1.rustc @ c2.rustc;
    }

  let toolchain_path =
    lazy
      (let cmd =
         match (Config.get ()).frontend with
         | Obol -> "obol"
         | Charon -> "charon"
       in
       Exe.exec_exn cmd [ "toolchain-path" ] |> List.hd)

  let cargo () = Lazy.force toolchain_path ^ "/bin/cargo"
  let rustc () = Lazy.force toolchain_path ^ "/bin/rustc"
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
    List.filter (fun s -> not (is_edition_flag s || is_crate_type_flag s))

  let flags_as_rustc_env args =
    if List.is_empty args then [] else [ "RUSTFLAGS=" ^ String.concat " " args ]

  let build_cmd ~mode { charon; obol; features; rustc } =
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
      | Obol -> ("obol", obol)
      | Charon -> ("charon", charon)
    in
    match mode with
    | Rustc ->
        (* If these arguments are passed to the command line, we need to quote them
           appropriately (since crate-attr) has parenthesis. We don't need to do this for
           Cargo since they go in the environment, and adding quotes there would make
           them wrong! This is lovely!  *)
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
        (* Cargo already specifies the edition *)
        let rustc = flags_for_cargo rustc in
        let env = rustc_as_env () @ flags_as_rustc_env rustc in
        (cmd, "cargo" :: args, env)

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
      (match (Config.get ()).plugin_directory with
      | Some root -> root
      | None -> List.hd Runtime_sites.Sites.plugins)

  type t = Std | Kani | Miri

  let name = function Std -> "std" | Kani -> "kani" | Miri -> "miri"
  let path lib = Filename.concat (Lazy.force root) (name lib)

  let compile lib =
    if not (Config.get ()).no_compile_plugins then
      let path = path lib in
      let verbosity =
        if (Config.get ()).log_compilation then [ "--verbose" ] else []
      in
      let env =
        Cmd.(current_rustc_flags () |> flags_for_cargo |> flags_as_rustc_env)
      in
      let env = Cmd.rustc_as_env () @ env in
      let _, err, status =
        let@ () = Exe.run_in path in
        Exe.exec ~env (Cmd.cargo ())
          ([ "build"; "--lib"; "--target"; Lazy.force target ] @ verbosity)
      in
      match status with
      | WEXITED (0 | 255) -> ()
      | _ ->
          Fmt.kstr plugin_err "Couldn't compile lib at %s@.%a" path
            Fmt.(list string)
            err

  let with_compiled lib f =
    let path = path lib in
    let target = Lazy.force target in
    compile lib;
    let config : Cmd.t = f (path, target) in
    let lib_imports =
      [
        Fmt.str "-L%s/target/%s/debug/deps" path target;
        Fmt.str "-L%s/target/debug/deps" path;
      ]
    in
    { config with rustc = config.rustc @ lib_imports }
end

type fun_decl = UllbcAst.fun_decl

type 'fuel entry_point = {
  fun_decl : fun_decl;
  expect_error : bool;
  fuel : 'fuel;
}

let mk_entry_point ?(expect_error = false) ?fuel fun_decl =
  Some { fun_decl; expect_error; fuel }

type 'fuel plugin = {
  mk_cmd : unit -> Cmd.t;
  get_entry_point : fun_decl -> 'fuel entry_point option;
}

let default =
  let mk_cmd () =
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
      ~obol:
        ([ "--entry-names"; "main"; "--entry-attribs"; "rusteriatool::test" ]
        @ opaque_names)
      ~features:[ "rusteria" ]
      ~rustc:
        [
          (* i.e. not always a binary! *)
          "--crate-type=lib";
          "-Z";
          "unstable-options";
          (* No warning *)
          "-Awarnings";
          (* include our std and rusteria crates *)
          "-Z";
          "crate-attr=feature(register_tool)";
          "-Z";
          "crate-attr=register_tool(rusteriatool)";
          "--extern";
          "rusteria";
          (* include the std *)
          "--extern";
          Fmt.str "noprelude:std=%s/target/%s/debug/libstd.rlib" std_lib_path
            target;
        ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    match List.last_opt decl.item_meta.name with
    | Some (PeIdent ("main", _)) -> mk_entry_point decl
    | _ when Charon_util.decl_has_attr decl "rusteriatool::test" ->
        let expect_error =
          Charon_util.decl_has_attr decl "rusteriatool::expect_fail"
        in
        mk_entry_point ~expect_error decl
    | _ -> None
  in
  { mk_cmd; get_entry_point }

let kani =
  let mk_cmd () =
    let@ _ = Lib.with_compiled Kani in
    Cmd.make ~features:[ "kani" ]
      ~obol:[ "--entry-attribs"; "kanitool::proof" ]
      ~rustc:[ "-Z"; "crate-attr=register_tool(kanitool)"; "--extern"; "kani" ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    if
      Charon_util.decl_has_attr decl "kanitool::proof"
      (* TODO: maybe we can raise an error or a warning here *)
      && List.is_empty decl.signature.inputs
    then
      let expect_error =
        Charon_util.decl_has_attr decl "kanitool::should_panic"
      in
      mk_entry_point ~expect_error decl
    else None
  in
  { mk_cmd; get_entry_point }

let miri =
  let mk_cmd () =
    let@ _ = Lib.with_compiled Miri in
    Cmd.make ~features:[ "miri" ]
      ~rustc:[ "--extern"; "miristd"; "--edition"; "2021" ]
      ~obol:[ "--entry-names"; "miri_start" ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    match List.last decl.item_meta.name with
    | PeIdent ("miri_start", _) -> mk_entry_point decl
    | _ -> None
  in
  { mk_cmd; get_entry_point }

type root_plugin = {
  mk_cmd : ?input:string -> output:string -> unit -> Cmd.t;
  get_entry_point :
    UllbcAst.crate -> fun_decl -> Soteria.Symex.Fuel_gauge.t entry_point option;
}

let merge_ifs (plugins : (bool * Soteria.Symex.Fuel_gauge.t option plugin) list)
    =
  let plugins =
    List.filter_map
      (fun (enabled, plugin) -> if enabled then Some plugin else None)
      plugins
  in

  let mk_cmd ?input ~output () =
    let input =
      Option.fold ~none:[] ~some:(fun s -> [ Filename.quote s ]) input
    in
    let init =
      Cmd.make
        ~charon:[ "--dest-file"; Filename.quote output ]
        ~obol:[ "--dest-file"; Filename.quote output ]
        ~rustc:input ()
    in
    List.map (fun (p : 'a plugin) -> p.mk_cmd ()) plugins
    |> List.fold_left Cmd.concat_cmd init
  in
  let get_entry_point crate (decl : fun_decl) =
    let rec aux acc rest =
      match (acc, rest) with
      | Some ep, _ ->
          let open Soteria.Symex in
          let fuel : Fuel_gauge.t =
            let get_or name default : Fuel_gauge.Fuel_value.t =
              match (Charon_util.decl_get_attr decl name, default) with
              | Some fuel, _ -> Finite (int_of_string fuel)
              | None, Some fuel -> Finite fuel
              | None, None -> Infinite
            in
            {
              steps = get_or "rusteriatool::step_fuel" (Config.get ()).step_fuel;
              branching =
                get_or "rusteriatool::branch_fuel" (Config.get ()).branch_fuel;
            }
          in
          Some { ep with fuel }
      | None, (p : 'a plugin) :: rest -> aux (p.get_entry_point decl) rest
      | None, [] -> None
    in
    let filters = (Config.get ()).filter in
    let filter_ok =
      match filters with
      | [] -> true
      | _ ->
          let fmt_env = PrintUllbcAst.Crate.crate_to_fmt_env crate in
          let name = PrintTypes.name_to_string fmt_env decl.item_meta.name in
          List.exists
            (fun f ->
              try Str.search_forward (Str.regexp f) name 0 >= 0
              with Not_found -> false)
            filters
    in
    if not filter_ok then None else aux None plugins
  in
  { mk_cmd; get_entry_point }

let create_using_current_config () =
  merge_ifs
    [
      (true, default);
      ((Config.get ()).with_kani, kani);
      ((Config.get ()).with_miri, miri);
    ]

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc ~mode ~plugin ?input ~output ~pwd () =
  if not (Config.get ()).no_compile then (
    let cmd = plugin.mk_cmd ?input ~output () in
    let _, err, res = Cmd.exec_in ~mode pwd cmd in
    if not (Exe.is_ok res) then
      if res = WEXITED 2 then compilation_err (String.concat "\n" err)
      else
        Fmt.kstr frontend_err "Failed compilation to ULLBC:@,%a"
          Fmt.(list string)
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

let normalize_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let with_entry_points ~plugin (crate : Charon.UllbcAst.crate) =
  let entry_points =
    Charon.Types.FunDeclId.Map.values crate.fun_decls
    |> List.filter_map (plugin.get_entry_point crate)
  in
  (crate, entry_points)

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file file_name =
  let plugin = create_using_current_config () in
  let file_name = normalize_path file_name in
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  parse_ullbc ~mode:Rustc ~plugin ~input:file_name ~output ~pwd:parent_folder ()
  |> with_entry_points ~plugin

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_crate crate_dir =
  let plugin = create_using_current_config () in
  let crate_dir = normalize_path crate_dir in
  let output = Printf.sprintf "%s/crate.llbc.json" crate_dir in
  parse_ullbc ~mode:Cargo ~plugin ~output ~pwd:crate_dir ()
  |> with_entry_points ~plugin

let compile_all_plugins () = List.iter Lib.compile [ Std; Kani; Miri ]

module Diagnostic = struct
  let to_loc (pos : Charon.Meta.loc) = (pos.line - 1, pos.col)

  let as_ranges (span : Charon.Meta.span_data) =
    match span.file.name with
    | Local file when String.starts_with ~prefix:"/rustc/" file -> []
    | Local file ->
        let root = Lazy.force Lib.root in
        let filename =
          if String.starts_with ~prefix:root file then
            let root_l = String.length root in
            let rel_path =
              String.sub file root_l (String.length file - root_l)
            in
            Some ("$SOTERIA-RUST" ^ rel_path)
          else
            let rustlib = Filename.concat "lib" "rustlib" in
            match String.index_of ~sub_str:rustlib file with
            | Some idx ->
                let idx = idx + String.length rustlib in
                let rel_path = String.sub file idx (String.length file - idx) in
                Some ("$RUSTLIB" ^ rel_path)
            | None -> None
        in
        [
          Soteria.Terminal.Diagnostic.mk_range_file ?filename
            ?content:span.file.contents file (to_loc span.beg_loc)
            (to_loc span.end_loc);
        ]
    | Virtual _ | NotReal _ -> []

  let print_diagnostic ~fname ~call_trace ~error =
    Soteria.Terminal.Diagnostic.print_diagnostic ~call_trace ~as_ranges
      ~error:(Fmt.to_to_string Error.pp error)
      ~severity:(Error.severity error) ~fname
end
