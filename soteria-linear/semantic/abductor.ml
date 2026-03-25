open Syntaxes.FunctionWrap
open Context
module LSymex = Aux.Symex
module Lang = Soteria_linear_ast.Lang
module Bi_interp = Interp.Make (Bi_state)
open Soteria.Logs.Import

let bi_abd_fuel =
  Soteria.Symex.Fuel_gauge.{ steps = Finite 100; branching = Finite 3 }

type res =
  (Aux.S_val.t, Aux.Error.t, State.syn list) Soteria.Symex.Compo_res.t
  * Bi_state.t option

type branch = (args:Aux.S_val.t list * res:res) * LSymex.Value.(sbool t) list

let make_spec_opt (((~args, ~res), pc) : branch) : Context.spec option =
  let open Syntaxes.Option in
  let res, bstate = res in
  let pre, post = Bi_state.to_spec bstate in
  let args = List.map Aux.S_val.to_syn args in
  let pc = List.map LSymex.Value.Expr.of_value pc in
  let+ ret =
    match res with
    | Ok v ->
        let v = Aux.S_val.to_syn v in
        Some (Result.Ok v)
    | Error e -> Some (Result.Error e)
    | Missing _ -> None
  in
  Context.{ args; pre; post; pc; ret }

let analyse_function ~context fname func_dec =
  L.info (fun m -> m "Analysing function %s" fname);
  match Hashtbl.Hstring.find_opt context.specs fname with
  | Some _ ->
      (* Function was already analysed, do nothing *)
      ()
  | None ->
      let@ () = with_context ~fun_interps:(fun _ -> Inline) context in
      let process =
        let open Bi_interp in
        let open LSymex.Syntax in
        let* args =
          SM.Symex.map_list func_dec.Lang.Fun_def.args ~f:(fun _ ->
              Interp.S_val.fresh ())
        in
        let+ res = Bi_interp.eval_function func_dec args Bi_state.empty in
        (~args, ~res)
      in
      let results : branch list =
        LSymex.run ~mode:UX ~fuel:bi_abd_fuel process
      in
      let specs = List.filter_map make_spec_opt results in
      Hashtbl.Hstring.replace context.specs fname specs
