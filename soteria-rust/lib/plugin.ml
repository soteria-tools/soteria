open Charon
open Syntaxes.FunctionWrap

exception PluginError of string

(** Utilities to run commands *)
module Exe = struct
  let run_in path f =
    let pwd = Unix.getcwd () in
    Fun.protect
      ~finally:(fun () -> Unix.chdir pwd)
      (fun () ->
        Unix.chdir path;
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

  let current_rustc_flags () =
    let rustc = !Config.current.rustc_flags in
    let sysroot =
      match !Config.current.sysroot with
      | Some path -> [ "--sysroot=" ^ path ]
      | None -> []
    in
    rustc @ sysroot

  let flags_as_rustc_env args =
    if List.is_empty args then [] else [ "RUSTFLAGS=" ^ String.concat " " args ]

  let build_cmd ~mode { charon; obol; features; rustc } =
    let with_obol = !Config.current.with_obol in
    let features = List.concat_map (fun f -> [ "--cfg"; f ]) features in
    let rustc = rustc @ current_rustc_flags () @ features in
    match mode with
    | Rustc ->
        if with_obol then ("obol", obol @ [ "--" ] @ rustc, [])
        else ("charon", ("rustc" :: charon) @ [ "--" ] @ rustc, [])
    | Cargo ->
        (* Cargo already specifies the edition *)
        let rustc =
          List.filter
            (Fun.negate (String.starts_with ~prefix:"--edition"))
            rustc
        in
        let env = flags_as_rustc_env rustc in
        if not with_obol then ("charon", "cargo" :: charon, env)
        else ("obol", "--cargo" :: obol, env)

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
  let toolchain_path =
    lazy (Exe.exec_exn "charon" [ "toolchain-path" ] |> List.hd)

  (* let cargo =
    "RUSTC=$(charon toolchain-path)/bin/rustc $(charon toolchain-path)/bin/cargo" *)

  let target =
    lazy
      (match !Config.current.target with
      | Some t -> t
      | None -> (
          let toolchain = Lazy.force toolchain_path in
          let info = Exe.exec_exn (toolchain ^ "/bin/cargo") [ "-vV" ] in
          match List.find_opt (String.starts_with ~prefix:"host") info with
          | Some s -> String.sub s 6 (String.length s - 6)
          | None -> raise (PluginError "Couldn't find target host")))

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
      let toolchain = Lazy.force toolchain_path in
      let env = Cmd.flags_as_rustc_env @@ Cmd.current_rustc_flags () in
      let env = ("RUSTC=" ^ toolchain ^ "/bin/rustc") :: env in
      let _out, err, status =
        let@ () = Exe.run_in path in
        Exe.exec ~env (toolchain ^ "/bin/cargo")
          ([ "build"; "--offline"; "--lib"; "--target"; Lazy.force target ]
          @ verbosity)
      in
      match status with
      | WEXITED (0 | 255) -> ()
      | _ ->
          let msg =
            Fmt.str "Couldn't compile lib at %s@.%a" path Fmt.(list string) err
          in
          raise (PluginError msg)

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
           (if !Config.current.monomorphize_old then
              "--monomorphize-conservative"
            else "--monomorphize");
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
          "--crate-type=lib";
          "-Zunstable-options";
          (* No warning *)
          "-Awarnings";
          (* include our std and rusteria crates *)
          "-Zcrate-attr=\"feature(register_tool)\"";
          "-Zcrate-attr=\"register_tool(rusteriatool)\"";
          "--extern=rusteria";
          (* include the std *)
          Fmt.str "--extern noprelude:std=%s/target/%s/debug/libstd.rlib"
            std_lib_path target;
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
      ~rustc:[ "-Zcrate-attr=\"register_tool(kanitool)\""; "--extern=kani" ]
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
      ~rustc:[ "--extern=miristd"; "--edition=2021" ]
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
  mk_cmd : input:string -> output:string -> unit -> Cmd.t;
  get_entry_point : fun_decl -> Soteria.Symex.Fuel_gauge.t entry_point option;
}

let merge_ifs (plugins : (bool * Soteria.Symex.Fuel_gauge.t option plugin) list)
    =
  let plugins =
    List.filter_map
      (fun (enabled, plugin) -> if enabled then Some plugin else None)
      plugins
  in

  let mk_cmd ~input ~output () =
    let init =
      Cmd.make
        ~charon:[ "--dest-file"; Filename.quote output ]
        ~obol:[ "--dest-file"; Filename.quote output ]
        ~rustc:[ Filename.quote input ]
        ()
    in
    List.map (fun (p : 'a plugin) -> p.mk_cmd ()) plugins
    |> List.fold_left Cmd.concat_cmd init
  in
  let get_entry_point (decl : fun_decl) =
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
          let name = Fmt.to_to_string Crate.pp_name decl.item_meta.name in
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

let compile_all_plugins () = List.iter Lib.compile [ Std; Kani; Miri ]
