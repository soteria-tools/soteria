open Charon
open Syntaxes.FunctionWrap

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

  let build_cmd ~mode { charon; obol; features; rustc } =
    let spaced = String.concat " " in
    let escape = Str.global_replace (Str.regexp {|\((\|)\)|}) {|\\\1|} in
    let with_obol = !Config.current.with_obol in
    let rustc = rustc @ !Config.current.rustc_flags in
    match mode with
    | Rustc ->
        let features = List.map (( ^ ) "--cfg=") features in
        let compiler =
          if not with_obol then "charon rustc " ^ spaced charon
          else "obol " ^ spaced obol
        in
        compiler ^ " -- " ^ spaced features ^ " " ^ escape (spaced rustc)
    | Cargo ->
        (* Cargo already specifies the edition *)
        let rustc =
          List.filter
            (Fun.negate (String.starts_with ~prefix:"--edition"))
            rustc
        in
        let features = List.map (( ^ ) "--cfg ") features in
        let rustc = rustc @ features in
        let env =
          if not (List.is_empty rustc) then
            "RUSTFLAGS=\"" ^ spaced rustc ^ "\" "
          else ""
        in
        let compiler =
          if not with_obol then "charon cargo " ^ spaced charon
          else "obol --cargo " ^ spaced obol
        in
        env ^ compiler

  let exec_cmd cmd =
    if !Config.current.log_compilation then
      L.app (fun g -> g "Running command: %s" cmd);
    Sys.command cmd

  let exec_and_read cmd =
    if !Config.current.log_compilation then
      L.app (fun g -> g "Running command: %s" cmd);
    let inp = Unix.open_process_in cmd in
    let r = In_channel.input_lines inp in
    In_channel.close inp;
    r

  let exec_in ~mode folder cmd =
    let verbosity =
      if !Config.current.log_compilation then "" else "> /dev/null 2>/dev/null"
    in
    exec_cmd @@ "cd " ^ folder ^ " && " ^ build_cmd ~mode cmd ^ verbosity
end

exception PluginError of string

type fun_decl = UllbcAst.fun_decl

type 'fuel entry_point = {
  fun_decl : fun_decl;
  expect_error : bool;
  fuel : 'fuel;
}

let mk_entry_point ?(expect_error = false) ?fuel fun_decl =
  Some { fun_decl; expect_error; fuel }

let cargo =
  "RUSTC=$(charon toolchain-path)/bin/rustc $(charon toolchain-path)/bin/cargo"

let target =
  lazy
    (match !Config.current.target with
    | Some t -> t
    | None -> (
        let info = Fmt.kstr Cmd.exec_and_read "%s -vV" cargo in
        match List.find_opt (String.starts_with ~prefix:"host") info with
        | Some s -> String.sub s 6 (String.length s - 6)
        | None -> raise (PluginError "Couldn't find target host")))

let lib_root =
  match Sys.getenv_opt "RUSTERIA_PLUGINS" with
  | Some root -> root
  | None -> List.hd Runtime_sites.Sites.plugins

type lib = Std | Kani | Miri

let lib_name = function Std -> "std" | Kani -> "kani" | Miri -> "miri"
let lib_path lib = lib_root ^ "/" ^ lib_name lib

let lib_compile ~target lib =
  if not !Config.current.no_compile_plugins then
    let path = lib_path lib in
    let verbosity =
      if !Config.current.log_compilation then "--verbose"
      else "> /dev/null 2>/dev/null"
    in
    let res =
      Fmt.kstr Cmd.exec_cmd "cd %s && %s build --lib --target %s %s" path cargo
        target verbosity
    in
    if res <> 0 && res <> 255 then
      let msg = Fmt.str "Couldn't compile lib at %s: error %d" path res in
      raise (PluginError msg)

let with_compiled_lib lib f =
  let path = lib_path lib in
  let target = Lazy.force target in
  lib_compile ~target lib;
  let config : Cmd.t = f (path, target) in
  let lib_imports =
    [
      Fmt.str "-L%s/target/%s/debug/deps" path target;
      Fmt.str "-L%s/target/debug/deps" path;
    ]
  in
  { config with rustc = config.rustc @ lib_imports }

(* List of patterns that currently cause generic errors (and thus crashes in monomorphisation).
   We tell Charon to not translate these, to avoid the crash, and we just hope the item is not
   encountered at runtime. *)
let known_generic_errors =
  [
    "alloc::borrow::ToOwned::to_owned";
    "alloc::raw_vec::_::try_allocate_in";
    "alloc::string::_::from";
    "alloc::raw_vec::finish_grow";
    "core::fmt::Display::fmt";
    "std::path::_::from";
    "core::iter::traits::iterator::Iterator::flatten";
  ]

type 'fuel plugin = {
  mk_cmd : unit -> Cmd.t;
  get_entry_point : fun_decl -> 'fuel entry_point option;
}

let default =
  let mk_cmd () =
    let@ std_lib_path, target = with_compiled_lib Std in
    let opaques = List.map (( ^ ) "--opaque ") known_generic_errors in
    let monomorphize_flag =
      if !Config.current.monomorphize_old then "--monomorphize-conservative"
      else "--monomorphize"
    in
    Cmd.make
      ~charon:
        ([
           "--ullbc";
           "--extract-opaque-bodies";
           monomorphize_flag;
           "--mir elaborated";
           "--raw-boxes";
         ]
        @ opaques)
      ~obol:[ "--entry-names main"; "--entry-attribs rusteriatool::test" ]
      ~features:[ "rusteria" ]
      ~rustc:
        [
          (* i.e. not always a binary! *)
          "--crate-type=lib";
          "-Zunstable-options";
          (* No warning *)
          "-Awarnings";
          (* include our std and rusteria crates *)
          "-Zcrate-attr=feature(register_tool)";
          "-Zcrate-attr=register_tool(rusteriatool)";
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
    let@ _ = with_compiled_lib Kani in
    Cmd.make ~features:[ "kani" ]
      ~obol:[ "--entry-attribs kanitool::proof" ]
      ~rustc:[ "-Zcrate-attr=register_tool(kanitool)"; "--extern=kani" ]
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
    let@ _ = with_compiled_lib Miri in
    Cmd.make ~features:[ "miri" ]
      ~rustc:[ "--extern=miristd"; "--edition=2021" ]
      ~obol:[ "--entry-names miri_start" ]
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
        ~charon:[ "--dest-file " ^ output ]
        ~obol:[ "--dest-file " ^ output ]
        ~rustc:[ input ] ()
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

let compile_all_plugins () =
  let target = Lazy.force target in
  List.iter (lib_compile ~target) [ Std; Kani; Miri ]
