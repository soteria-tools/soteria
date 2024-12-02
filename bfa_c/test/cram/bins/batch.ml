open Bfa_c_lib
open Csymex.Syntax

let ( let@ ) = ( @@ )

let process =
  let open Svalue.Infix in
  let open Svalue.Syntax in
  let@ () = Csymex.batched in
  let* n = Csymex.nondet Svalue.TInt in
  let* () = Csymex.return ~learned:[ n #>= 0s ] () in
  let* () = Csymex.return ~learned:[ n #< 10s ] () in
  Csymex.return ~learned:[ n #< 0s ] ()

let () =
  let file = Some "batch.smt" in
  Z3solver.set_smt_file file;
  Initialize_analysis.init_once ();
  let l = Csymex.run process in
  assert (List.length l = 0);
  Z3solver.close_smt_log_file ();
  let f = open_in "batch.smt" in
  let () =
    try
      while true do
        Printf.printf "%s" (input_line f);
        print_newline ()
      done
    with End_of_file -> close_in f
  in
  ()
