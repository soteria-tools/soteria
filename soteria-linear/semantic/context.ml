(** This module revolves around analysis contexts, which are a program, as well
    as anything that is relevant to run the analysis. *)

open Soteria_linear_ast.Lang
open Aux
module Logic = Soteria.Logic.Make (Symex)
module Expr = Symex.Value.Expr

type spec = {
  args : Expr.t list;
  pre : State.syn list;
  post : State.syn list;
  pc : Expr.t list;
  ret : (Expr.t, Aux.Error.t) result;
}
[@@deriving show { with_path = false }]

type t = {
  program : Program.t;
  specs : spec list Hashtbl.Hstring.t;
  being_analysed : Hashset.Hstring.t;
}

let make ~program () =
  {
    program;
    specs = Hashtbl.Hstring.create 0;
    being_analysed = Hashset.Hstring.with_capacity 8;
  }

type fun_interp = Use_specs of spec list | Inline

type _ Effect.t +=
  | Get_function : string -> Fun_def.t Effect.t
  | Get_interp : string -> fun_interp Effect.t

let get_function name = Effect.perform (Get_function name)
let get_interp name = Effect.perform (Get_interp name)

let with_context ~(fun_interps : context:t -> string -> fun_interp) ctx f =
  try f () with
  | effect Get_function name, k ->
      let func = String_map.find name ctx.program in
      Effect.Deep.continue k func
  | effect Get_interp name, k ->
      let interp =
        match Hashtbl.Hstring.find_opt ctx.specs name with
        | None -> (
            match fun_interps ~context:ctx name with
            | Inline -> Inline
            | Use_specs specs ->
                Hashtbl.Hstring.replace ctx.specs name specs;
                Use_specs specs)
        | Some specs -> Use_specs specs
      in
      Effect.Deep.continue k interp

let with_program_inline_everything program f =
  let context = make ~program () in
  with_context ~fun_interps:(fun ~context:_ _ -> Inline) context f
