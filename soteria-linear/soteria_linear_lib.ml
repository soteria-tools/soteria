open Soteria
open Soteria_std
open Syntaxes.FunctionWrap
open Soteria_linear_semantic
module Lang = Soteria_linear_ast.Lang
module Parser = Soteria_linear_parser.Parse
module LSymex = Aux.Symex
open Lang

let pp_results ft v =
  let pp =
    let open Fmt.Dump in
    list
    @@ pair
         (Compo_res.pp
            ~ok:(pair Interp.S_val.ppa (Fmt.Dump.option State.pp))
            ~err:
              (Soteria.Symex.Or_gave_up.pp
                 (pair State.pp_err (Fmt.Dump.option State.pp)))
            ~miss:Fmt.nop)
         (list Interp.S_val.ppa)
  in
  pp ft v

let with_config logs_config solver_config f =
  Soteria.Logs.Config.check_set_and_lock logs_config;
  Soteria.Solvers.Config.set_and_lock solver_config;
  f ()

module Exec_interp = Interp.Make (State)

let exec file =
  let program = Parser.parse_file file in
  Fmt.pr "@[<v 2>Parsed program:@ %a@]@." Lang.Program.pp program;
  let main =
    match Lang.String_map.find_opt "main" program with
    | Some f -> f
    | None -> failwith "No main function found"
  in
  let process =
    let@@ () = Exec_interp.SM.Result.run_with_state ~state:State.empty in
    Exec_interp.eval_function main []
  in
  let results =
    let@ () = Context.with_program_inline_everything program in
    Interp.Symex.Result.run ~mode:OX process
  in
  Fmt.pr "@[<v 2>Program executed with result:@ %a@]@?" pp_results results

module Bi_interp = Interp.Make (Bi_state)

let generate_summaries file =
  let program = Parser.parse_file file in
  let context = Context.make ~program () in
  String_map.iter (Abductor.analyse_function ~context) program;
  Fmt.pr "@[<v>";
  Soteria_std.Hashtbl.Hstring.iter
    (fun fname (specs : Context.spec list) ->
      Fmt.pr "@[<v 2>Specs for %s:@ %a@]@,@," fname
        (Fmt.list ~sep:(Fmt.any "@,@,") Context.pp_spec)
        specs)
    context.specs;
  Fmt.pr "@]@."
