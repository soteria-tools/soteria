open Charon
open Common.Charon_util
open Syntaxes.FunctionWrap
open Frontend_runtime

exception FrontendError = FrontendError

(** Something wrong internally with plugins *)
exception PluginError of string

(** Compilation of the code failed at the rustc level *)
exception CompilationError of string

let compilation_err msg = raise (CompilationError msg)
let plugin_err msg = raise (PluginError msg)

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
         "--lax-start-from";
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
