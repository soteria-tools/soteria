(** This module revolves around analysis contexts, which are a program, as well
    as anything that is relevant to run the analysis. *)

open Soteria_linear_ast.Lang
open Aux
module Logic = Soteria.Logic.Make (Symex)

module Spatial_atom : Logic.Asrt.S with type t = State.syn = struct
  type t = State.syn [@@deriving show]

  let ins_outs = State.ins_outs
end

module With_pure = Logic.Asrt.With_pure (Spatial_atom)
module Atom = With_pure.Atom

type spec = (Atom.t list, State.err) Logic.Spec.t

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
  | Get_specs : string -> spec list option Effect.t
  | Add_spec : string * spec list -> unit Effect.t
  | Use_specs : bool Effect.t

let get_function name = Effect.perform (Get_function name)
let get_specs name = Effect.perform (Get_specs name)
let add_spec name spec = Effect.perform (Add_spec (name, spec))
let use_specs () = Effect.perform Use_specs

let with_context ctx f =
  try f () with
  | effect Get_function name, k ->
      let func = String_map.find name ctx.program in
      Effect.Deep.continue k func
  | effect Get_specs name, k ->
      let res = Hashtbl.Hstring.find_opt ctx.specs name in
      Effect.Deep.continue k res
  | effect Add_spec (name, specs), k ->
      let current_specs =
        Hashtbl.Hstring.find_opt ctx.specs name |> Option.value ~default:[]
      in
      Hashtbl.Hstring.replace ctx.specs name (current_specs @ specs);
      Effect.Deep.continue k ()
  | effect Use_specs, k -> Effect.Deep.continue k ctx.use_specs

module Asrt_model (State : State_intf.S) = struct
  module Spatial_atom_model :
    Logic.Asrt.Model(Spatial_atom).S
      with type state = State.t
       and type fix = State.syn = struct
    type state = State.t
    type fix = State.syn

    let produce = State.produce
    let consume = State.consume
  end

  module Model_with_pure = With_pure.Model (Spatial_atom_model)
  include Logic.Asrt.Model_with_star (With_pure.Atom) (Model_with_pure)

  let exec_spec spec args state =
    Logic.Spec.execute ~consume ~produce spec args state
end
