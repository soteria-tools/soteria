open Soteria.Bv_values
open Soteria.Symex.Make (Soteria.Symex.Meta.Dummy) (Bv_solver.Z3_solver)
open Syntax

let test =
  let v = Svalue.Var.of_int 42 in
  let ty = Svalue.t_bool in
  let sv = Svalue.mk_var v ty in
  let exists = Svalue.Bool.exists [ (v, ty) ] sv in
  if%sat Typed.type_ exists then return 42 else return (-1)

let () =
  let pp = Fmt.(Dump.list int) in
  Fmt.pr "exists: %a@\n" pp (List.map fst @@ run ~mode:OX test)
