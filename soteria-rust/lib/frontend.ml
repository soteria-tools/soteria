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
        if !Config.current.log_compilation then
          L.app (fun g -> g "Changed working directory to %s" path);
        f ())

  let pp_status ft = function
    | Unix.WEXITED 0 -> Fmt.string ft "Exited successfully"
    | Unix.WEXITED n -> Fmt.pf ft "Exit(%d)" n
    | Unix.WSIGNALED n -> Fmt.pf ft "Killed(%d)" n
    | Unix.WSTOPPED n -> Fmt.pf ft "Stopped(%d)" n

  let is_ok = function Unix.WEXITED 0 -> true | _ -> false

  let exec ?(env = []) cmd args =
    (* let args = Array.of_list args in *)
    let current_env = Unix.environment () in
    let env = Array.append current_env (Array.of_list env) in
    let cmd = String.concat " " (cmd :: args) in
    if !Config.current.log_compilation then
      L.app (fun g -> g "Running command: %s" cmd);
    let out, inp, err = Unix.open_process_full cmd env in
    let output = In_channel.input_lines out in
    let error = In_channel.input_lines err in
    let status = Unix.close_process_full (out, inp, err) in
    if !Config.current.log_compilation then
      L.app (fun g ->
          g "Command finished with status: %a@.%a@.%a" pp_status status
            Fmt.(list ~sep:(any "@\n") string)
            output
            Fmt.(list ~sep:(any "@\n") string)
            error);
    (output, error, status)

  let exec_exn ?env cmd args =
    let output, _, status = exec ?env cmd args in
    assert (is_ok status);
    output
end

(** Simple utility to create and then delete files *)
module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
  let () = at_exit (fun () -> if !Config.current.cleanup then cleanup ())
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
         match !Config.current.frontend with
         | Obol -> "obol"
         | Charon -> "charon"
       in
       Exe.exec_exn cmd [ "toolchain-path" ] |> List.hd)

  let cargo () = Lazy.force toolchain_path ^ "/bin/cargo"
  let rustc () = Lazy.force toolchain_path ^ "/bin/rustc"
  let rustc_as_env () = [ "RUSTC=" ^ rustc () ]

  let current_rustc_flags () =
    let rustc = !Config.current.rustc_flags in
    let sysroot =
      match !Config.current.sysroot with
      | Some path -> [ "--sysroot=" ^ path ]
      | None -> []
    in
    rustc @ sysroot

  let flags_for_cargo =
    List.filter (Fun.negate (String.starts_with ~prefix:"--edition"))

  let flags_as_rustc_env args =
    if List.is_empty args then [] else [ "RUSTFLAGS=" ^ String.concat " " args ]

  let build_cmd ~mode { charon; obol; features; rustc } =
    let features = List.concat_map (fun f -> [ "--cfg"; f ]) features in
    let rustc = rustc @ current_rustc_flags () @ features in
    let cmd, args =
      match !Config.current.frontend with
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

  let exec_in_exn ~mode folder cmd =
    let cmd, args, env = build_cmd ~mode cmd in
    let@ () = Exe.run_in folder in
    Exe.exec_exn ~env cmd args
end

module Lib = struct
  let target =
    lazy
      (match !Config.current.target with
      | Some t -> t
      | None -> (
          let env = Cmd.rustc_as_env () in
          let info = Exe.exec_exn ~env (Cmd.cargo ()) [ "-vV" ] in
          match List.find_opt (String.starts_with ~prefix:"host") info with
          | Some s -> String.sub s 6 (String.length s - 6)
          | None -> plugin_err "Couldn't find target host"))

  let root =
    lazy
      (match Sys.getenv_opt "RUSTERIA_PLUGINS" with
      | Some root -> root
      | None -> List.hd Runtime_sites.Sites.plugins)

  type t = Std | Kani | Miri

  let name = function Std -> "std" | Kani -> "kani" | Miri -> "miri"
  let path lib = Filename.concat (Lazy.force root) (name lib)

  let compile lib =
    if not !Config.current.no_compile_plugins then
      let path = path lib in
      let verbosity =
        if !Config.current.log_compilation then [ "--verbose" ] else []
      in
      let env =
        Cmd.(current_rustc_flags () |> flags_for_cargo |> flags_as_rustc_env)
      in
      let env = Cmd.rustc_as_env () @ env in
      let _out, err, status =
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
      []
      (* List.concat_map (fun n -> [ "--opaque"; n ]) Builtins.Eval.opaque_names *)
    in
    Cmd.make
      ~charon:
        ([
           "--ullbc";
           "--extract-opaque-bodies";
           "--monomorphize";
           "--mir elaborated";
           "--raw-boxes";
         ]
        @ opaque_names)
      ~obol:
        ([ "--entry-names"; "main"; "--entry-attribs"; "rusteriatool::test" ]
        @ opaque_names)
      ~features:[ "rusteria" ]
      ~rustc:
        [
          (* i.e. not always a binary! *)
          "--crate-type";
          "lib";
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
              steps = get_or "rusteriatool::step_fuel" !Config.current.step_fuel;
              branching =
                get_or "rusteriatool::branch_fuel" !Config.current.branch_fuel;
            }
          in
          Some { ep with fuel }
      | None, (p : 'a plugin) :: rest -> aux (p.get_entry_point decl) rest
      | None, [] -> None
    in
    let filters = !Config.current.filter in
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
      (!Config.current.with_kani, kani);
      (!Config.current.with_miri, miri);
    ]

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc ~mode ~plugin ?input ~output ~pwd () =
  if not !Config.current.no_compile then (
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
    try
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json
    with
    | Sys_error _ -> frontend_err "File doesn't exist"
    | _ -> frontend_err "Failed to parse ULLBC"
  in
  let crate = Result.get_or_raise frontend_err crate in
  if !Config.current.output_crate then (
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

  let as_ranges (loc : Charon.Meta.span) =
    let span = Option.value ~default:loc.data loc.generated_from_span in
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
            Some ("$RUSTERIA" ^ rel_path)
          else None
        in
        [
          Soteria.Terminal.Diagnostic.mk_range_file ?filename
            ?content:span.file.contents file (to_loc span.beg_loc)
            (to_loc span.end_loc);
        ]
    | Virtual _ -> []

  let print_diagnostic ~fname ~call_trace ~error =
    Soteria.Terminal.Diagnostic.print_diagnostic ~call_trace ~as_ranges
      ~error:(Fmt.to_to_string Error.pp error)
      ~severity:(Error.severity error) ~fname
end
