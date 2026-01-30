open Soteria
open Soteria_std.Syntaxes.FunctionWrap
open Soteria_linear_semantic
open Soteria.Logs
module Lang = Soteria_linear_ast.Lang
module Parser = Soteria_linear_parser.Parse
module LSymex = Aux.Symex
open Lang

let bi_abd_fuel = Symex.Fuel_gauge.{ steps = Finite 100; branching = Finite 3 }

let pp_results ft v =
  let pp =
    let open Fmt.Dump in
    list
    @@ pair
         (Symex.Compo_res.pp
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
    let@ () = Interp.with_program program in
    Interp.Symex.Result.run ~mode:OX process
  in
  Fmt.pr "@[<v 2>Program executed with result:@ %a@]@?" pp_results results

module Bi_interp = Interp.Make (Bi_state)

let generate_summaries file =
  let program = Parser.parse_file file in
  String_map.iter
    (fun fname (func_dec : Fun_def.t) ->
      let@ () = L.with_section (Fmt.str "Generating summary for %s" fname) in
      Fmt.pr "@[<v 2>Summaries for %s:@ " fname;
      let process =
        let open Bi_interp in
        let@@ () = SM.Result.run_with_state ~state:Bi_state.empty in
        let open SM.Syntax in
        let* args =
          SM.fold_list func_dec.args ~init:[] ~f:(fun acc _ ->
              let+^ v = Interp.S_val.fresh () in
              v :: acc)
        in
        let args = List.rev args in
        Bi_interp.eval_function func_dec args
      in
      let results =
        let@ () = Interp.with_program program in
        LSymex.run ~mode:UX ~fuel:bi_abd_fuel process
      in
      List.iter
        (fun (res, pc) ->
          match res with
          | Symex.Compo_res.Error (err, state) ->
              Fmt.pr "%a@,@," Bi_state.pp_spec (state, pc, Result.error err)
          | Symex.Compo_res.Ok (v, state) ->
              Fmt.pr "%a@,@," Bi_state.pp_spec (state, pc, Result.ok v)
          | Symex.Compo_res.Missing _ -> ())
        results;
      Fmt.pr "@]")
    program
