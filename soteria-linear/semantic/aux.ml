open Soteria_linear_ast.Lang
module Typed = Soteria.Tiny_values.Typed
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)

module Error = struct
  type t = [ `Interp of string | `UseAfterFree ]
end

module S_int = struct
  include Typed

  type t = Typed.T.sint Typed.t

  let simplify = Symex.simplify
  let fresh () = Symex.nondet Typed.t_int
  let pp = Typed.ppa
end

module S_val = struct
  include Typed

  type t = T.any Typed.t [@@deriving show { with_path = false }]

  let sem_eq = sem_eq_untyped

  let fresh () : t Symex.t =
    Symex.branches
      [
        (fun () -> Symex.nondet Typed.t_int);
        (fun () -> Symex.nondet Typed.t_bool);
      ]

  let check_nonzero (v : T.sint Typed.t) :
      (T.nonzero Typed.t, string, 'a) Symex.Result.t =
    let open Symex.Syntax in
    let open Typed.Infix in
    let open Typed.Syntax in
    if%sat v ==@ 0s then Symex.Result.error "ZeroException"
    else Symex.Result.ok (Typed.cast v)
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
