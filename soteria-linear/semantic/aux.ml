open Soteria_linear_ast.Lang
module Typed = Soteria.Tiny_values.Typed
module Syn = Soteria.Tiny_values.Typed.Syn

module Symex =
  Soteria.Symex.Make
    (Soteria.Symex.Meta.Dummy)
    (Soteria.Tiny_values.Tiny_solver.Z3_solver)

module S_int = struct
  include Typed

  type t = Typed.T.sint Typed.t
  type syn = Syn.t [@@deriving show { with_path = false }]

  let simplify = Symex.simplify
  let fresh () = Symex.nondet Typed.t_int
  let pp = Typed.ppa

  let subst (f : syn -> 'a Typed.t) (e : syn) : t =
    (* FIXME: I don't know how to do without `Typed.cast`... *)
    Typed.cast (f e)

  let to_syn = Syn.of_value
  let exprs_syn (s : syn) : Symex.Value.Syn.t list = [ s ]
end

module S_val = struct
  include Typed

  type t = T.any Typed.t [@@deriving show { with_path = false }]
  type syn = Syn.t [@@deriving show { with_path = false }]

  let subst (f : syn -> 'a Typed.t) (e : syn) : t = Typed.cast (f e)
  let to_syn = Syn.of_value
  let exprs_syn (s : syn) : Symex.Value.Syn.t list = [ s ]

  let learn_eq (_ : syn) (_ : t) : (unit, 'a) Symex.Consumer.t =
    failwith "Not implemented"

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
