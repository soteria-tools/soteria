open Soteria
open Soteria_std.Syntaxes.FunctionWrap
open Soteria_linear_semantic
module Lang = Soteria_linear_ast.Lang
module Parser = Soteria_linear_parser.Parse

let pp_results ft v =
  let pp =
    let open Fmt.Dump in
    list
    @@ pair
         (Symex.Compo_res.pp
            ~ok:(pair Interp.S_val.ppa State.pp)
            ~err:(Soteria.Symex.Or_gave_up.pp State.pp_err)
            ~miss:Fmt.nop)
         (list Interp.S_val.ppa)
  in
  pp ft v

let with_config logs_config solver_config f =
  Soteria.Logs.Config.check_set_and_lock logs_config;
  Soteria.Solvers.Config.set solver_config;
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
  let process = Exec_interp.run_function main State.empty [] in
  let results =
    let@ () = Interp.with_program program in
    Interp.Symex.Result.run ~fuel:Symex.Fuel_gauge.infinite ~mode:OX process
  in
  Fmt.pr "@[<v 2>Program executed with result:@ %a@]@?" pp_results results
