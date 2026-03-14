(** This module revolves around analysis contexts, which are a program, as well
    as anything that is relevant to run the analysis. *)

open Soteria_linear_ast.Lang
open Aux
module Logic = Soteria.Logic.Make (Symex)

type spec = {
  args : Expr.t list;
  pre : State.syn list;
  post : State.syn list;
  pc : Expr.t list;
  ret : Expr.t list;
}

type t = {
  program : Program.t;
  specs : spec list Hashtbl.Hstring.t;
  use_specs : bool;
  being_analysed : string Dynarray.t;
}

let make ~use_specs ~program () =
  {
    program;
    specs = Hashtbl.Hstring.create 0;
    use_specs;
    being_analysed = Dynarray.create ();
  }

type _ Effect.t +=
  | Get_function : string -> Fun_def.t Effect.t
  | Get_specs : string -> spec list Effect.t
  | Add_spec : string * spec list -> unit Effect.t
  | Use_specs : bool Effect.t

let get_function name = Effect.perform (Get_function name)
let get_specs name = Effect.perform (Get_specs name)
let add_spec name spec = Effect.perform (Add_spec (name, spec))
let use_specs () = Effect.perform Use_specs

let with_context ~(compute_specs : string -> spec list) ctx f =
  try f () with
  | effect Get_function name, k ->
      let func = String_map.find name ctx.program in
      Effect.Deep.continue k func
  | effect Get_specs name, k ->
      let specs =
        match Hashtbl.Hstring.find_opt ctx.specs name with
        | None ->
            let specs = compute_specs name in
            Hashtbl.Hstring.replace ctx.specs name specs;
            specs
        | Some specs -> specs
      in
      Effect.Deep.continue k specs
  | effect Add_spec (name, specs), k ->
      let current_specs =
        Hashtbl.Hstring.find_opt ctx.specs name |> Option.value ~default:[]
      in
      Hashtbl.Hstring.replace ctx.specs name (current_specs @ specs);
      Effect.Deep.continue k ()
  | effect Use_specs, k -> Effect.Deep.continue k ctx.use_specs
