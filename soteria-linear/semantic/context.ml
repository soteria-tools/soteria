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
  being_analysed : string Dynarray.t;
}

let make ~program () =
  {
    program;
    specs = Hashtbl.Hstring.create 0;
    being_analysed = Dynarray.create ();
  }

type fun_interp = Use_specs of spec list | Inline

type _ Effect.t +=
  | Get_function : string -> Fun_def.t Effect.t
  | Get_interp : string -> fun_interp Effect.t
  | Add_spec : string * spec list -> unit Effect.t
  | Use_specs : bool Effect.t

let get_function name = Effect.perform (Get_function name)
let get_interp name = Effect.perform (Get_interp name)
let add_spec name spec = Effect.perform (Add_spec (name, spec))
let use_specs () = Effect.perform Use_specs

let with_context ~(fun_interps : string -> fun_interp) ctx f =
  try f () with
  | effect Get_function name, k ->
      let func = String_map.find name ctx.program in
      Effect.Deep.continue k func
  | effect Get_interp name, k ->
      let interp =
        match Hashtbl.Hstring.find_opt ctx.specs name with
        | None -> (
            match fun_interps name with
            | Inline -> Inline
            | Use_specs specs ->
                Hashtbl.Hstring.replace ctx.specs name specs;
                Use_specs specs)
        | Some specs -> Use_specs specs
      in
      Effect.Deep.continue k interp
  | effect Add_spec (name, specs), k ->
      let current_specs =
        Hashtbl.Hstring.find_opt ctx.specs name |> Option.value ~default:[]
      in
      Hashtbl.Hstring.replace ctx.specs name (current_specs @ specs);
      Effect.Deep.continue k ()

let with_program_inline_everything program f =
  let context = make ~program () in
  with_context ~fun_interps:(fun _ -> Inline) context f
