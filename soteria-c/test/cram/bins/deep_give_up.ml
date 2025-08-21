open Csymex
open Syntax
open Soteria_c_values

let pp_branch pp_err =
  let pp_pc = Fmt.Dump.list Typed.ppa in
  let pp_res =
    Soteria_symex.Compo_res.pp ~ok:Typed.ppa ~err:pp_err ~miss:Fmt.nop
  in
  Fmt.Dump.pair pp_res pp_pc

let complex_process =
  let open Typed.Infix in
  let* x = nondet Typed.t_int in
  let* y = nondet Typed.t_int in
  if%sat x >@ y then
    if%sat x >@ Typed.zero then Result.ok (x +@ y) else Result.ok (x -@ y)
  else
    if%sat x ==@ y then give_up ~loc:Cerb_location.unknown "x == y"
    else Result.error "okkk"

let give_up_in_ux =
  let results = Csymex.run ~mode:UX complex_process in
  Fmt.pr "In UX with Csymex.run: %a@\n@\n"
    (Fmt.Dump.list (pp_branch Fmt.string))
    results

let give_up_in_ux_res =
  let results = Csymex.Result.run ~mode:UX complex_process in
  Fmt.pr "In UX with Csymex.Result.run: %a@\n@\n"
    (Fmt.Dump.list (pp_branch (Soteria_symex.Symex.Or_gave_up.pp Fmt.string)))
    results

let give_up_in_ox_exn =
  Fmt.pr
    "Trying to run in OX with Csymex.run, expecting to catch an exception -- ";
  try
    let results = Csymex.run ~mode:OX complex_process in
    Fmt.pr "In OX with Csymex.run: %a@\n"
      (Fmt.Dump.list (pp_branch Fmt.string))
      results
  with Soteria_symex.Symex.Gave_up reason ->
    Fmt.pr "Caught Gave_up in OX: %s@\n@\n" reason

let give_up_in_ox_res =
  let results = Csymex.Result.run ~mode:OX complex_process in
  Fmt.pr "In UX with Csymex.Result.run: %a@\n@\n"
    (Fmt.Dump.list (pp_branch (Soteria_symex.Symex.Or_gave_up.pp Fmt.string)))
    results
