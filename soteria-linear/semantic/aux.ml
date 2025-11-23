open Soteria_linear_ast.Lang
module S_val = Soteria.Tiny_values.Typed

module Symex =
  Soteria.Symex.Make
    (Soteria.Symex.Meta.Dummy)
    (Soteria.Tiny_values.Tiny_solver.Z3_solver)

module S_int = struct
  module Symex = Symex
  include S_val

  type t = S_val.T.sint S_val.t

  let simplify = Symex.simplify
  let fresh () = Symex.nondet S_val.t_int
  let pp = S_val.ppa
end

type _ Effect.t += Resolve_function : string -> Fun_def.t Effect.t

type subst = S_val.T.any S_val.t String_map.t
[@@deriving show { with_path = false }]

let get_function fname = Effect.perform (Resolve_function fname)

let with_program (program : Program.t) f =
  try f ()
  with effect Resolve_function name, k -> (
    match String_map.find_opt name program with
    | Some func_def -> Effect.Deep.continue k func_def
    | None -> failwith ("Function not found: " ^ name))
