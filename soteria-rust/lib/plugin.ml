open Charon

module Cmd = struct
  type t = {
    (* Arguments passed to Charon *)
    charon : string list; [@default []]
    (* Features to enable for compilation (as in --cfg) *)
    features : string list; [@default []]
    (* DEPRECATED: rustc flags. For Cargo we use RUSTFLAGS, but when possible it would be
       nicer to use the Cargo-specific command (as with features) *)
    rustc : string list; [@default []]
  }
  [@@deriving make]

  type mode = Cargo | Rustc | Obol

  let empty_cmd = make ()

  let concat_cmd c1 c2 =
    {
      charon = c1.charon @ c2.charon;
      features = c1.features @ c2.features;
      rustc = c1.rustc @ c2.rustc;
    }

  let build_cmd ~mode { charon; features; rustc } =
    let spaced = String.concat " " in
    match mode with
    | Rustc ->
        let escape = Str.global_replace (Str.regexp {|\((\|)\)|}) {|\\\1|} in
        let features = List.map (fun f -> "--cfg " ^ f) features in
        "charon rustc "
        ^ spaced charon
        ^ " -- "
        ^ spaced features
        ^ " "
        ^ escape (spaced rustc)
    | Obol ->
        (* almost the same as charon rustc *)
        let escape = Str.global_replace (Str.regexp {|\((\|)\)|}) {|\\\1|} in
        let features = List.map (fun f -> "--cfg=" ^ f) features in
        let obol_flags =
          List.filter (String.starts_with ~prefix:"--dest-file") charon
        in
        (* Obol currently doesn't support lib crates/files *)
        let rustc = List.filter (( <> ) "--crate-type=lib") rustc in
        "DYLD_LIBRARY_PATH=$(charon toolchain-path)/lib/ obol "
        ^ spaced obol_flags
        ^ " -- "
        ^ spaced features
        ^ " "
        ^ escape (spaced rustc)
    | Cargo ->
        let env =
          if not (List.is_empty rustc) then
            "RUSTFLAGS=\"" ^ spaced rustc ^ "\" "
          else ""
        in
        let features =
          if not (List.is_empty features) then
            "--features " ^ String.concat "," features ^ " "
          else ""
        in
        env ^ "charon cargo " ^ spaced charon ^ " -- --quiet " ^ features

  let exec_cmd cmd =
    L.debug (fun g -> g "Running command: %s" cmd);
    Sys.command cmd

  let exec_and_read cmd =
    L.debug (fun g -> g "Running command: %s" cmd);
    let inp = Unix.open_process_in cmd in
    let r = In_channel.input_lines inp in
    In_channel.close inp;
    r

  let exec_in ~mode folder cmd =
    let verbosity =
      if Soteria_logs.(Config.should_log Level.Info) then ""
      else "> /dev/null 2>/dev/null"
    in
    exec_cmd @@ "cd " ^ folder ^ " && " ^ build_cmd ~mode cmd ^ verbosity
end

exception PluginError of string

type fun_decl = UllbcAst.fun_decl

type entry_point = {
  fun_decl : fun_decl;
  expect_error : bool;
  fuel : Soteria_symex.Fuel_gauge.t option;
}

let mk_entry_point ?(expect_error = false) ?fuel fun_decl =
  Some { fun_decl; expect_error; fuel }

let cargo =
  "RUSTC=$(charon toolchain-path)/bin/rustc $(charon toolchain-path)/bin/cargo"

let get_host =
  let host = ref None in
  fun () ->
    match !host with
    | Some h -> h
    | None -> (
        let info = Fmt.kstr Cmd.exec_and_read "%s -vV" cargo in
        match List.find_opt (String.starts_with ~prefix:"host") info with
        | Some s ->
            let host_ = String.sub s 6 (String.length s - 6) in
            host := Some host_;
            host_
        | None -> raise (PluginError "Couldn't find target host"))

let lib_root =
  match Sys.getenv_opt "RUSTERIA_PLUGINS" with
  | Some root -> root
  | None -> List.hd Runtime_sites.Sites.plugins

let lib_path name = lib_root ^ "/" ^ name

let compile_lib path =
  let target = get_host () in
  let verbosity =
    if Soteria_logs.(Config.should_log Level.Trace) then "--verbose"
    else "> /dev/null 2>/dev/null"
  in
  let res =
    Fmt.kstr Cmd.exec_cmd "cd %s && %s build --lib --target %s %s" path cargo
      target verbosity
  in
  if res <> 0 && res <> 255 then
    let msg = Fmt.str "Couldn't compile lib at %s: error %d" path res in
    raise (PluginError msg)

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
    "core::ptr::null_mut";
    "std::path::_::from";
    "core::iter::traits::iterator::Iterator::flatten";
  ]

type plugin = {
  mk_cmd : unit -> Cmd.t;
  get_entry_point : fun_decl -> entry_point option;
}

let default =
  let mk_cmd () =
    let std_lib = lib_path "std" in
    let target = get_host () in
    let opaques = List.map (( ^ ) "--opaque ") known_generic_errors in
    compile_lib std_lib;
    Cmd.make
      ~charon:
        ([
           "--ullbc";
           "--extract-opaque-bodies";
           "--monomorphize";
           "--mir elaborated";
           "--raw-boxes";
         ]
        @ opaques)
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
          Fmt.str "-L%s/target/%s/debug/deps" std_lib target;
          Fmt.str "-L%s/target/debug/deps" std_lib;
          Fmt.str "--extern noprelude:std=%s/target/%s/debug/libstd.rlib"
            std_lib target;
        ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    match List.last_opt decl.item_meta.name with
    | Some (PeIdent ("main", _)) -> mk_entry_point decl
    | _ when Charon_util.decl_has_attr decl "rusteriatool::test" ->
        mk_entry_point decl
    | _ -> None
  in
  { mk_cmd; get_entry_point }

let kani =
  let mk_cmd () =
    let lib = lib_path "kani" in
    let target = get_host () in
    compile_lib lib;
    Cmd.make ~features:[ "kani " ]
      ~rustc:
        [
          "-Zcrate-attr=register_tool(kanitool)";
          "--extern=kani";
          Fmt.str "-L%s/target/%s/debug/deps" lib target;
          Fmt.str "-L%s/target/debug/deps" lib;
        ]
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
    let lib = lib_path "miri" in
    let target = get_host () in
    compile_lib lib;
    Cmd.make ~features:[ "miri" ]
      ~rustc:
        [
          "--extern=miristd";
          Fmt.str "-L%s/target/%s/debug/deps" lib target;
          Fmt.str "-L%s/target/debug/deps" lib;
        ]
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
  get_entry_point : fun_decl -> entry_point option;
}

let merge_ifs (plugins : (bool * plugin) list) =
  let plugins =
    List.filter_map
      (fun (enabled, plugin) -> if enabled then Some plugin else None)
      plugins
  in

  let mk_cmd ~input ~output () =
    let init =
      Cmd.make ~charon:[ "--dest-file " ^ output ] ~rustc:[ input ] ()
    in
    List.map (fun (p : plugin) -> p.mk_cmd ()) plugins
    |> List.fold_left Cmd.concat_cmd init
  in
  let get_entry_point (decl : fun_decl) =
    let rec aux acc rest =
      match (acc, rest) with
      | Some ep, _ ->
          let fuel : Soteria_symex.Fuel_gauge.t =
            let get_or name none =
              Charon_util.decl_get_attr decl name
              |> Option.fold ~none ~some:int_of_string
            in
            let steps = get_or "rusteriatool::step_fuel" 1000 in
            let branching = get_or "rusteriatool::branch_fuel" 4 in
            { steps; branching }
          in
          Some { ep with fuel = Some fuel }
      | None, (p : plugin) :: rest -> aux (p.get_entry_point decl) rest
      | _ -> acc
    in
    aux None plugins
  in
  { mk_cmd; get_entry_point }

let create_using_current_config () =
  merge_ifs
    [
      (true, default);
      (!Config.current.with_kani, kani);
      (!Config.current.with_miri, miri);
    ]
