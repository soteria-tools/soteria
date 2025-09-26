open
  Soteria.Symex.Make (Soteria.Symex.Meta.Dummy) (Soteria.Symex.Mut.Dummy)
    (Soteria.C_values.C_solver.Z3_solver)

open Syntax
open Soteria.C_values

let pp_branch pp_err =
  let pp_pc = Fmt.Dump.list Typed.ppa in
  let pp_res =
    Soteria.Symex.Compo_res.pp ~ok:Typed.ppa ~err:pp_err ~miss:Fmt.nop
  in
  Fmt.Dump.pair pp_res pp_pc

let complex_process =
  let open Typed.Infix in
  let* x = nondet Typed.t_int in
  let* y = nondet Typed.t_int in
  if%sat x >@ y then
    if%sat x >@ Typed.zero then Result.ok (x +@ y) else Result.ok (x -@ y)
  else if%sat x ==@ y then give_up ~loc:() "x == y" else Result.error "okkk"

let _give_up_in_ux =
  let results = run ~mode:UX complex_process in
  Fmt.pr "@[<v 2>Csymex.run ~mode:UX complex_process:@ %a@]@\n@\n"
    (Fmt.Dump.list (pp_branch Fmt.string))
    results

let _give_up_in_ux_res =
  let results = Result.run ~mode:UX complex_process in
  Fmt.pr "@[<v 2>Csymex.Result.run ~mode:UX complex_process:@ %a@]@\n@\n"
    (Fmt.Dump.list (pp_branch (Soteria.Symex.Or_gave_up.pp Fmt.string)))
    results

let _give_up_in_ox_exn =
  Fmt.pr "Csymex.run ~mode:OX complex_process;; EXPECTING EXCEPTION -- ";
  try
    let results = run ~mode:OX complex_process in
    Fmt.pr "In OX with Csymex.run: %a@\n"
      (Fmt.Dump.list (pp_branch Fmt.string))
      results
  with Soteria.Symex.Gave_up reason ->
    Fmt.pr "Caught Gave_up in OX: %s@\n@\n" reason

let _give_up_in_ox_res =
  let results = Result.run ~mode:OX complex_process in
  Fmt.pr "@[<v 2>Csymex.Result.run ~mode:OX complex_process:@ %a@]@\n@\n"
    (Fmt.Dump.list (pp_branch (Soteria.Symex.Or_gave_up.pp Fmt.string)))
    results
