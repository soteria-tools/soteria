open Charon
open Cmd

exception PluginError of string

type fun_decl = UllbcAst.fun_decl
type entry_point = { fun_decl : fun_decl; expect_error : bool }

let mk_entry_point ?(expect_error = false) fun_decl =
  Some { fun_decl; expect_error }

let cargo =
  "RUSTC=$(charon toolchain-path)/bin/rustc $(charon toolchain-path)/bin/cargo"

let get_host =
  let host = ref None in
  fun () ->
    match !host with
    | Some h -> h
    | None -> (
        let info = Fmt.kstr exec_and_read "%s -vV" cargo in
        match List.find_opt (String.starts_with ~prefix:"host") info with
        | Some s ->
            let host_ = String.sub s 6 (String.length s - 6) in
            host := Some host_;
            host_
        | None -> raise (PluginError "Couldn't find target host"))

let compile_lib path =
  let target = get_host () in
  let verbosity =
    if Soteria_logs.(Config.should_log Level.Trace) then "--verbose"
    else "> /dev/null 2>/dev/null"
  in
  let res =
    Fmt.kstr exec_cmd "cd %s && %s build --lib --target %s %s" path cargo target
      verbosity
  in
  if res <> 0 && res <> 255 then
    let msg = Fmt.str "Couldn't compile lib at %s: error %d" path res in
    raise (PluginError msg)

type plugin = {
  mk_cmd : unit -> Cmd.charon_cmd;
  get_entry_point : fun_decl -> entry_point option;
}

let default =
  let mk_cmd () =
    mk_cmd
      ~charon:
        [
          "--ullbc";
          (* We can't enable this because it removes statements we care about... *)
          (* "--mir_optimized"; *)
          "--translate-all-methods";
          "--extract-opaque-bodies";
          "--monomorphize";
        ]
      ~rustc:
        [
          (* i.e. not always a binary! *)
          "--crate-type=lib";
          "-Zunstable-options";
          (* Not sure this is needed *)
          "--extern=std";
          "--extern=core";
          (* No warning *)
          "-Awarnings";
        ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    match List.last_opt decl.item_meta.name with
    | Some (PeIdent ("main", _)) -> mk_entry_point decl
    | _ -> None
  in
  { mk_cmd; get_entry_point }

let kani =
  let mk_cmd () =
    let root = List.hd Runtime_sites.Sites.plugin_kani in
    let target = get_host () in
    let lib = root ^ "/std" in
    compile_lib lib;
    mk_cmd
      ~rustc:
        [
          "-Zcrate-attr=feature\\(register_tool\\)";
          "-Zcrate-attr=register_tool\\(kanitool\\)";
          (* Code often hides kani proofs behind a cfg *)
          "--cfg=kani";
          "--extern=kani";
          (* Manually include lib binaries *)
          Fmt.str "-L%s/target/%s/debug/deps" lib target;
          Fmt.str "-L%s/target/debug/deps" lib;
          Fmt.str "--extern noprelude:std=%s/target/%s/debug/libstd.rlib" lib
            target;
        ]
      ()
  in
  let get_entry_point (decl : fun_decl) =
    if Charon_util.decl_has_attr decl "kanitool::proof" then
      let expect_error =
        Charon_util.decl_has_attr decl "kanitool::should_panic"
      in
      mk_entry_point ~expect_error decl
    else None
  in
  { mk_cmd; get_entry_point }

let miri =
  let mk_cmd () =
    let root = List.hd Runtime_sites.Sites.plugin_miri in
    let target = get_host () in
    let lib = root ^ "/miristd" in
    compile_lib lib;
    mk_cmd
      ~rustc:
        [
          "--cfg=miri";
          (* Manually include lib binaries *)
          Fmt.str "-L%s/target/%s/debug/deps" lib target;
          Fmt.str "-L%s/target/%s/debug" lib target;
          Fmt.str "-L%s/target/debug/deps" lib;
          (* Fmt.str "--extern noprelude:std=%s/target/%s/debug/libstd.rlib" lib
            target; *)
        ]
      ()
  in
  let get_entry_point _ = None in
  { mk_cmd; get_entry_point }

type root_plugin = {
  mk_cmd : input:string -> output:string -> unit -> Cmd.charon_cmd;
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
      mk_cmd ~charon:[ "--input " ^ input; "--dest-file " ^ output ] ()
    in
    List.map (fun (p : plugin) -> p.mk_cmd ()) plugins
    |> List.fold_left concat_cmd init
  in
  let get_entry_point decl =
    let rec aux acc rest =
      match (acc, rest) with
      | None, (p : plugin) :: rest -> aux (p.get_entry_point decl) rest
      | _ -> acc
    in
    aux None plugins
  in
  { mk_cmd; get_entry_point }
