open Charon
open Common.Charon_util
open Syntaxes.FunctionWrap
open Frontend_runtime

exception FrontendError = FrontendError

(** Compilation of the code failed; contains
    [(error message, where it happened)]. *)
exception CompilationError of string * string

let compilation_err info msg = raise (CompilationError (info, msg))

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
          | None ->
              compilation_err "executing rustc" "Couldn't find target host"))

  let root =
    lazy
      (match
         ((Config.get ()).plugin_directory, Runtime_sites.Sites.plugins)
       with
      | Some root, _ -> root
      | None, root :: _ -> root
      | None, [] ->
          compilation_err "loading plugins" "Couldn't find plugin directory")

  type t = Std | Soteria | Kani

  let name = function Std -> "std" | Soteria -> "soteria" | Kani -> "kani"
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
        Fmt.kstr
          (compilation_err ("compiling plugin " ^ name lib))
          "Couldn't compile lib at %s@.%a" path
          Fmt.(list string)
          err

  (** Deletes the target directory of a target, to avoid duplicate builds *)
  let clean lib = exec_cargo lib [ "clean" ]

  let compile lib =
    let config = Config.get () in
    if not config.no_compile_plugins then
      let env =
        Cmd.(current_rustc_flags () |> flags_for_cargo |> flags_as_rustc_env)
      in
      let args =
        [ "build"; "--lib"; "--target"; Lazy.force target ]
        @ if (Config.get ()).log_compilation then [ "--verbose" ] else []
      in
      let args = if config.offline then args @ [ "--offline" ] else args in
      exec_cargo ~env lib args

  let with_compiled lib f =
    let path = path lib in
    let target = Lazy.force target in
    compile lib;
    let config : Cmd.t = f (path, target) in
    let lib_imports =
      [
        "--extern";
        name lib
        ^ "="
        ^ (path / "target" / target / "debug" / ("lib" ^ name lib ^ ".rlib"));
        "-L" ^ (path / "target" / target / "debug" / "deps");
        "-L" ^ (path / "target" / "debug" / "deps");
      ]
    in
    { config with rustc = config.rustc @ lib_imports }
end

(** A single Cargo target to compile and analyse. [Default] analyses the crate's
    source (with [main] as the entry point); the others build a test harness, so
    their entry points are the [#[test]] functions. *)
type target =
  | Default
  | Lib
  | Test of string
  | Example of string
  | Bin of string

let pp_target ft = function
  | Default -> Fmt.pf ft "crate"
  | Lib -> Fmt.pf ft "lib tests"
  | Test name -> Fmt.pf ft "test \"%s\"" name
  | Example name -> Fmt.pf ft "example \"%s\"" name
  | Bin name -> Fmt.pf ft "bin \"%s\"" name

(** A machine-readable JSON description of the target, as
    [{ "kind": ..., "name": ... }] (the [name] is omitted for [crate]/[lib]).
    Used to tag entry points in [--list-tests] output. *)
let target_to_yojson target : Yojson.Safe.t =
  let kind, name =
    match target with
    | Default -> ("crate", None)
    | Lib -> ("lib", None)
    | Test name -> ("test", Some name)
    | Example name -> ("example", Some name)
    | Bin name -> ("bin", Some name)
  in
  let name = Option.fold ~none:`Null ~some:(fun n -> `String n) name in
  `Assoc [ ("kind", `String kind); ("name", name) ]

(** The Cargo flags selecting this target. *)
let target_cargo_flags = function
  | Default -> []
  | Lib -> [ "--lib" ]
  | Test name -> [ "--test"; name ]
  | Example name -> [ "--example"; name ]
  | Bin name -> [ "--bin"; name ]

(** Whether this target builds a test harness, in which case the entry points
    are the [#[test]] functions rather than [main]. *)
let target_is_test = function Default -> false | _ -> true

(** A filesystem-safe name for the ULLBC output of this target. *)
let target_filename = function
  | Default -> "crate.ullbc"
  | Lib -> "test-lib.ullbc"
  | Test name -> "test-" ^ name ^ ".ullbc"
  | Example name -> "example-" ^ name ^ ".ullbc"
  | Bin name -> "bin-" ^ name ^ ".ullbc"

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
  let@ _ = Lib.with_compiled Soteria in
  let opaque_names =
    List.concat_map (fun n -> [ "--opaque"; n ]) Builtins.Eval.opaque_names
  in
  Cmd.make
    ~charon:
      ([
         "--ullbc";
         "--extract-opaque-bodies";
         "--mir=elaborated";
         "--reconstruct-fallible-operations";
         "--reconstruct-asserts";
         "--desugar-drops";
         "--precise-drops";
         "--format=postcard";
         "--no-typecheck";
         "--no-normalize";
         (* Use the normal distributed sysroot; Charon otherwise defaults to a
            full-MIR Miri sysroot, whose std is incompatible with our
            separately-compiled [soteria] support crate. *)
         "--sysroot=default";
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
      ]
    ()

let kani () =
  let@ _ = Lib.with_compiled Kani in
  Cmd.make ~features:[ "kani" ]
    ~entry_points:[ Attrib "kanitool::proof" ]
    ~expect_error:[ Attrib "kanitool::should_panic" ]
    ~rustc:[ "-Z"; "crate-attr=register_tool(kanitool)" ]
    ()

let miri () =
  Cmd.make ~features:[ "miri" ] ~rustc:[ "--edition"; "2021" ]
    ~entry_points:[ Name "miri_start" ] ()

(** Filters a name, according to the current {!Config.t.filter} and
    {!Config.t.exclude} settings. If there are no filters, all names are
    included; otherwise, a name is included if it matches any filter and doesn't
    match any exclude. *)
let filter_name crate name =
  let fmt_env = Print.crate_to_fmt_env crate in
  let name = Print.name_to_string fmt_env name in

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

let create_using_current_config ~target () : mk_cmd * entry_point_filter =
  let config = Config.get () in
  let cmd_parts =
    [ default () ]
    @ (if config.with_kani then [ kani () ] else [])
    @ if config.with_miri then [ miri () ] else []
  in
  let cmd = List.fold_left Cmd.concat_cmd Cmd.empty cmd_parts in
  let cmd =
    if not (target_is_test target) then cmd
    else
      (* HACK: if we're in test mode, we want to ignore main because Rust will
         compile it in a quirky way and it requires having a sysroot. Instead we
         want to look for the tests directly! So we add #[test] *)
      let entry_points =
        Cmd.Attrib "test" :: cmd.entry_points
        |> List.filter (function Cmd.Pub | Name "main" -> false | _ -> true)
      in
      let expect_error = Cmd.Attrib "should_panic" :: cmd.expect_error in
      { cmd with entry_points; expect_error }
  in
  let cmd = { cmd with cargo = target_cargo_flags target } in
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
      if res = WEXITED 2 then
        compilation_err "compiling target code" (String.concat "\n" err)
      else
        Fmt.kstr frontend_err "Failed compilation to ULLBC:@,%a"
          Fmt.(list ~sep:(any "\n") string)
          err;
    Cleaner.touched output);
  let crate =
    match output |> Charon.OfPostcard.crate_of_postcard_file with
    | Ok crate -> crate
    | Error msg ->
        Fmt.kstr frontend_err
          "Failed to parse ULLBC. Do you have the right version of %a \
           installed?@.Error: %s"
          Config.pp_frontend (Config.get ()).frontend msg
    | exception Sys_error _ -> frontend_err "File doesn't exist"
    | exception e -> Fmt.kstr frontend_err "Unexpected error: %a" Fmt.exn e
  in
  if (Config.get ()).output_crate then (
    (* save pretty-printed crate to local file *)
    let crate_file = Filename.chop_suffix output ".ullbc" ^ ".crate" in
    let str = Charon.Print.crate_to_string crate in
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
  let output = Printf.sprintf "%s.ullbc" file_name in
  let cmd = mk_cmd ~input:file_name ~output () in
  parse_ullbc ~mode:Rustc ~cmd ~output ~pwd:parent_folder ()

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_crate ~(mk_cmd : mk_cmd) ~target crate_dir =
  let output = crate_dir / target_filename target in
  let cmd = mk_cmd ~output () in
  parse_ullbc ~mode:Cargo ~cmd ~output ~pwd:crate_dir ()

let parse_ullbc_raw ~mk_cmd ~target = function
  | `File file -> parse_ullbc_of_file ~mk_cmd file
  | `Dir path -> parse_ullbc_of_crate ~mk_cmd ~target path

(** Given a path, if it's a file will parse the ULLBC of that single file using
    rustc; otherwise will assume it's a path to a crate and use cargo.
    {b Will translate all functions in the crate, without filtering
       entry-points.} *)
let parse_ullbc path =
  let mk_cmd, _filter = create_using_current_config ~target:Default () in
  let mk_cmd =
    modify_mk_cmd (fun c -> Cmd.{ c with entry_points = [] }) mk_cmd
  in
  parse_ullbc_raw ~mk_cmd ~target:Default path

(** Query Obol for the targets of the crate at [dir], keeping those relevant for
    analysis: the [src/] unit tests and integration tests, plus examples and
    bins when [~only_tests:false]. *)
let list_targets ~only_tests dir =
  let output =
    let@ () = Exe.run_in dir in
    Exe.exec_exn (Cmd.frontend_cmd ()) [ "list-targets" ]
  in
  let parse_target item =
    let open Yojson.Safe.Util in
    let name = item |> member "name" |> to_string in
    match item |> member "kind" |> to_string with
    | "lib" -> Some Lib
    | "test" -> Some (Test name)
    | "example" when not only_tests -> Some (Example name)
    | "bin" when not only_tests -> Some (Bin name)
    | _ -> None
  in
  match Yojson.Safe.from_string (String.concat "" output) with
  | `List items -> List.filter_map parse_target items
  | _ | (exception _) ->
      compilation_err "listing targets"
        "Could not parse the targets returned by Obol"

(** The list of targets to compile and analyse for [path], as determined by the
    current configuration ([--example], [--test], [--tests], [--all-targets]).
    Single files always yield a single [Default] target. *)
let targets_to_run path =
  let config = Config.get () in
  match path with
  | `File _ -> [ Default ]
  | `Dir dir when config.all_targets -> list_targets ~only_tests:false dir
  | `Dir dir ->
      let all_tests =
        if config.tests then list_targets ~only_tests:true dir else []
      in
      let targets =
        all_tests
        @ List.map (function "lib" -> Lib | name -> Test name) config.test
        @ List.map (fun name -> Example name) config.example
      in
      if List.is_empty targets then [ Default ] else targets

(** Compile [path] for the given [target] and return the crate along with its
    entry points. Only starts translation from functions considered
    entry-points. *)
let compile_target ~target path =
  let mk_cmd, filter = create_using_current_config ~target () in
  parse_ullbc_raw ~mk_cmd ~target path |> with_entry_points ~filter

let compile_all_plugins () =
  List.iter
    (fun l ->
      Lib.clean l;
      Lib.compile l)
    [ Soteria; Std; Kani ]
