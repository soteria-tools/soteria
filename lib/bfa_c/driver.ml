open Cerb_frontend

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

(** Copying most of the wrapper from RefinedC *)

let impl_name =
  match Sys.getenv_opt "IMPL_NAME" with
  | Some impl -> impl
  | None -> "gcc_4.9.0_x86_64-apple-darwin10.8.0"

let set_cerb_conf () =
  let open Cerb_global in
  set_cerb_conf ~backend_name:"bfa-c" ~exec:false Random ~concurrency:false
    Basic ~defacto:false ~permissive:false ~agnostic:false
    ~ignore_bitfields:false

let io : Cerb_backend.Pipeline.io_helpers =
  let open Cerb_backend.Pipeline in
  let return = Exception.except_return in
  let pass_message =
    let ref = ref 0 in
    fun str ->
      Cerb_debug.print_success (Printf.sprintf "%i. %s" !ref str);
      incr ref;
      return ()
  in
  let set_progress _ = return () in
  let run_pp opts doc =
    run_pp opts doc;
    return ()
  in
  let print_endline str =
    print_endline str;
    return ()
  in
  let print_debug n mk_str =
    Cerb_debug.print_debug n [] mk_str;
    return ()
  in
  let warn ?(always = false) mk_str =
    Cerb_debug.warn ~always [] mk_str;
    return ()
  in
  { pass_message; set_progress; run_pp; print_endline; print_debug; warn }

let load_core_stdlib_shim () =
  Exception.Result (Pmap.empty String.compare, Pmap.empty Symbol.compare_sym)

let load_core_impl_shim _stdlib _impl_name =
  Exception.Result (Pmap.empty Implementation.implementation_constant_compare)

let frontend cpp_cmd filename =
  let open Cerb_backend.Pipeline in
  let ( let* ) = Exception.except_bind in
  let conf =
    {
      debug_level = 0;
      pprints = [];
      astprints = [];
      ppflags = [];
      ppouts = [];
      typecheck_core = false;
      rewrite_core = false;
      sequentialise_core = false;
      cpp_cmd;
      cpp_stderr = true;
    }
  in
  set_cerb_conf ();
  Ocaml_implementation.(set HafniumImpl.impl);
  L.debug (fun m -> m "About to load core stdlib");
  let* stdlib = load_core_stdlib_shim () in
  L.debug (fun m -> m "Core stdlib loaded");
  let* impl = load_core_impl_shim stdlib impl_name in
  c_frontend (conf, io) (stdlib, impl) ~filename

let cpp_cmd = "cc -E -C -Werror -nostdinc -undef "

let parse_ail file =
  match frontend cpp_cmd file with
  | Result (_, (_, ast)) -> ast
  | Exception (loc, err) ->
      Fmt.failwith "Failed to parse ail with error:\n%s\n\nat %s"
        (Pp_errors.short_message err)
        (Cerb_location.location_to_string loc)

let is_main (def : Cabs.function_definition) =
  let decl = match def with FunDef (_, _, _, decl, _) -> decl in
  match decl with
  | Cabs.Declarator (_, DDecl_identifier (_, Identifier (_, name))) ->
      String.equal name "main"
  | _ -> false

let pp_err ft t =
  match t with
  | `MissingKey -> Fmt.string ft "MissingKey"
  | `MissingResource -> Fmt.string ft "MissingResource"
  | `OutOfBounds -> Fmt.string ft "OutOfBounds"
  | `UninitializedMemoryAccess -> Fmt.string ft "UninitializedMemoryAccess"
  | `UseAfterFree -> Fmt.string ft "UseAfterFree"

let exec_main log_level smt_file file_name =
  Z3solver.set_smt_file smt_file;
  setup_log log_level;
  L.debug (fun m -> m "Starting to execute");
  let entry_point, sigma = parse_ail file_name in
  L.debug (fun m ->
      m "Parsed as follows:\n=============\n%a=============" Fmt_ail.pp_program
        (entry_point, sigma));
  let entry_point =
    match entry_point with
    | None -> Fmt.failwith "No entry point function"
    | Some e -> e
  in
  L.debug (fun m -> m "Ending parse");
  let entry_point =
    sigma.function_definitions
    |> List.find (fun (id, _) -> Symbol.equal_sym id entry_point)
  in
  let symex =
    Interp.exec_fun ~prog:sigma ~args:[] ~state:Heap.empty entry_point
  in
  let () = L.debug (fun m -> m "Starting symex") in
  let result = Csymex.force symex in
  Fmt.pr "Symex terminated with the following outcomes: %a"
    Fmt.Dump.(list @@ result ~ok:(pair Svalue.pp Heap.pp) ~error:pp_err)
    result
