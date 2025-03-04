open Utils.Syntax
module Wpst_interp = Interp.Make (Heap)
module Compo_res = Bfa_symex.Compo_res

module ExecResult = struct
  type ('ok, 'err, 'fatal) exresult =
    | Ok of 'ok
    | Error of 'err
    | Fatal of 'fatal

  let ok x = Ok x
  let error e = Error e
  let fatal e = Fatal e

  let bind x f =
    match x with Ok x -> f x | Error e -> Error e | Fatal e -> Fatal e

  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e
    | Fatal e -> Fatal e

  let res_of_code = function
    | Ok 0 -> Ok ()
    | Ok code -> Fmt.kstr fatal "Error code: %d" code
    | Error e -> Error e
    | Fatal e -> Fatal e

  let of_result = function Result.Ok x -> Ok x | Error e -> Error e
  let of_result_fatal = function Result.Ok x -> Ok x | Error e -> Fatal e
  let ( let* ) = bind
  let ( let+ ) x f = map f x
  let ( let*> ) x f = bind (res_of_code x) f
  let ( let*! ) x f = bind (of_result_fatal x) f
end

open ExecResult

module Cleaner = struct
  let files = ref []
  let touched file = files := file :: !files
  let cleanup () = List.iter Sys.remove !files
end

let setup_console_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let pp_err ft (err, call_trace) =
  Format.open_hbox ();
  let () =
    match err with
    | `NullDereference -> Fmt.string ft "NullDereference"
    | `OutOfBounds -> Fmt.string ft "OutOfBounds"
    | `UninitializedMemoryAccess -> Fmt.string ft "UninitializedMemoryAccess"
    | `UseAfterFree -> Fmt.string ft "UseAfterFree"
    | `DivisionByZero -> Fmt.string ft "DivisionByZero"
    | `ParsingError s -> Fmt.pf ft "ParsingError: %s" s
    | `UBPointerComparison -> Fmt.string ft "UBPointerComparison"
    | `UBPointerArithmetic -> Fmt.string ft "UBPointerArithmetic"
    | `UBAbort -> Fmt.string ft "UBAbort"
    | `DoubleFree -> Fmt.string ft "DoubleFree"
    | `InvalidFree -> Fmt.string ft "InvalidFree"
    | `Memory_leak -> Fmt.string ft "Memory leak"
    | `FailedAssert -> Fmt.string ft "Failed assertion"
    | `Overflow -> Fmt.string ft "Overflow"
    | `StdErr msg -> Fmt.pf ft "Std error: %s" msg
  in
  Fmt.pf ft " with trace %a" Call_trace.pp call_trace;
  Format.close_box ()

(** Given a Rust file, parse it into LLBC, using Charon. *)
let parse_ullbc_of_file ~no_compile file_name =
  let parent_folder = Filename.dirname file_name in
  let output = Printf.sprintf "%s.llbc.json" file_name in
  let*> () =
    match no_compile with
    | true -> Ok 0
    | false -> (
        (* load from env *)
        let+ kani_lib =
          try
            let path = Sys.getenv "KANI_LIB_PATH" in
            if String.ends_with ~suffix:"/" path then Ok path
            else Ok (path ^ "/")
          with Not_found -> Fatal "KANI_LIB_PATH not set"
        in
        String.concat " "
          [
            Fmt.str "cd %s &&" parent_folder;
            "charon --ullbc";
            (* We don't care about our implementation *)
            "--opaque=kani";
            "--translate-all-methods";
            "--monomorphize";
            (* Go through rustc to allow injecting the kani deps *)
            "--no-cargo";
            (* i.e. not always a binary! *)
            "--rustc-arg=--crate-type=lib";
            "--rustc-arg=-Zunstable-options";
            "--rustc-arg=-Zcrate-attr=feature\\(register_tool\\)";
            "--rustc-arg=-Zcrate-attr=register_tool\\(kanitool\\)";
            (* Code often hides kani proofs behind a cfg *)
            "--rustc-arg=--cfg=kani";
            (* Not sure this is needed *)
            "--rustc-arg=--extern=kani";
            "--rustc-arg=--extern=std";
            (* The below is cursed and should be fixed !!! *)
            Fmt.str
              "--rustc-arg=-L%skani/target/aarch64-apple-darwin/debug/deps"
              kani_lib;
            Fmt.str "--rustc-arg=-L%skani/target/debug/deps" kani_lib;
            Fmt.str "--input %s" file_name;
            Fmt.str "--dest-file %s" output;
          ]
        |> Sys.command
        |> function
        | 0 ->
            Cleaner.touched output;
            0
        | n -> n)
  in
  let* crate =
    try
      output |> Yojson.Basic.from_file |> Charon.UllbcOfJson.crate_of_json |> ok
    with
    | Sys_error _ -> Fatal "File doesn't exist"
    | _ -> Fatal "Failed to parse ULLBC"
  in
  (* save crate to local file *)
  let () =
    match (crate, no_compile) with
    | Ok crate, false ->
        let crate_file = Printf.sprintf "%s.crate" file_name in
        let oc = open_out_bin crate_file in
        let str = Charon.PrintUllbcAst.Crate.crate_to_string crate in
        output_string oc str;
        close_out oc;
        Cleaner.touched crate_file
    | _ -> ()
  in
  of_result_fatal crate

let exec_main (crate : Charon.UllbcAst.crate) =
  let open Charon in
  Layout.Session.set_crate crate;
  let+ entry_points =
    Types.FunDeclId.Map.bindings crate.fun_decls
    |> List.filter (fun (_, (decl : UllbcAst.blocks UllbcAst.gfun_decl)) ->
           (* TODO: is the entry point the function with index 0? *)
           (match List.rev decl.item_meta.name with
           | PeIdent ("main", _) :: _ -> true
           | _ -> false)
           || List.exists
                (function
                  | Meta.AttrUnknown { path; _ } -> path = "kanitool::proof"
                  | _ -> false)
                decl.item_meta.attr_info.attributes)
    |> List.map snd
    |> function
    | [] -> Fatal "No main function found"
    | rest -> Ok rest
  in
  let exec_fun = Wpst_interp.exec_fun ~crate ~args:[] ~state:Heap.empty in
  entry_points |> List.concat_map (Rustsymex.run << exec_fun)

let exec_main_and_print log_level smt_file no_compile clean_up file_name =
  Z3solver.set_smt_file smt_file;
  setup_console_log log_level;
  let res =
    let* crate = parse_ullbc_of_file ~no_compile file_name in
    let* res =
      try exec_main crate with e -> Fmt.kstr fatal "Exn: %a" Fmt.exn e
    in
    if List.is_empty res then Fatal "Execution vanished"
    else if List.exists (Compo_res.is_missing << fst) res then
      Fatal "Miss encountered in WPST"
    else
      let errors = Compo_res.only_errors @@ List.map fst res in
      if List.is_empty errors then
        res
        |> List.filter_map (function
             | Compo_res.Ok ok, pcs -> Some (ok, pcs)
             | _ -> None)
        |> ok
      else
        let errors_parsed =
          Fmt.str "Errors: %a" Fmt.(Dump.list pp_err) errors
        in
        Error errors_parsed
  in
  if clean_up then Cleaner.cleanup ();
  match res with
  | Ok res ->
      let open Fmt in
      let n = List.length res in
      let pp_pc ft pc =
        pf ft "@[%a@]" (list ~sep:(any " /\\@, ") Typed.ppa) pc
      in
      let pp_info ft (_, pc) = pf ft "PC: @[%a@]" pp_pc pc in
      Fmt.pr "Done. - Ran %i branches\n%a\n" n
        (list ~sep:(any "@\n@\n") pp_info)
        res;
      exit 0
  | Error e ->
      L.err (fun f -> f "Error: %s" e);
      exit 1
  | Fatal e ->
      L.err (fun f -> f "Fatal: %s" e);
      exit 2
