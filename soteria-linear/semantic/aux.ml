open Soteria_linear_ast.Lang
module Typed = Soteria.Tiny_values.Typed

module Symex =
  Soteria.Symex.Make
    (Soteria.Symex.Meta.Dummy)
    (Soteria.Tiny_values.Tiny_solver.Z3_solver)

module S_int = struct
  module Symex = Symex
  include Typed

  type t = Typed.T.sint Typed.t

  let simplify = Symex.simplify
  let fresh () = Symex.nondet Typed.t_int
  let pp = Typed.ppa
end

module S_val = struct
  module Symex = Symex
  include Typed

  type t = T.any Typed.t [@@deriving show { with_path = false }]

  let sem_eq = sem_eq_untyped

  let fresh () : t Symex.t =
    let open Symex.Syntax in
    let* v = Symex.nondet Typed.t_int in
    Symex.return v
end

type _ Effect.t += Resolve_function : string -> Fun_def.t Effect.t
type subst = S_val.t String_map.t [@@deriving show { with_path = false }]

let get_function fname = Effect.perform (Resolve_function fname)

let with_program (program : Program.t) f =
  try f ()
  with effect Resolve_function name, k -> (
    match String_map.find_opt name program with
    | Some func_def -> Effect.Deep.continue k func_def
    | None -> failwith ("Function not found: " ^ name))
