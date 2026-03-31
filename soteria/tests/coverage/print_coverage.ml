open Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values.Typed

let file = "my.file"

let branch_span line id : Soteria.Coverage.branch_span =
  { file; line; branch_id = "b_" ^ string_of_int line ^ "_" ^ string_of_int id }

let process () : (unit, 'e, 'f) Result.t =
  Soteria.Coverage.As_ctx.register_function ~file ~name:"my_fun" ~line:1 ();
  let* b1 = nondet t_bool in
  Soteria.Coverage.As_ctx.mark_lines_reachable ~file Iter.(1 -- 7);
  Soteria.Coverage.As_ctx.mark_line ~file ~line:1;
  let* () = if%sat[@span branch_span 2 1] b1 then return () else return () in
  Soteria.Coverage.As_ctx.mark_line ~file ~line:3;
  let* () =
    if%sat[@span branch_span 4 1] v_false then return ()
    else if%sat[@span branch_span 4 2] v_true then return ()
    else return ()
  in
  Soteria.Coverage.As_ctx.mark_line ~file ~line:5;
  Result.ok ()

let () =
  Soteria.Logs.Config.(set_and_lock (make ~hide_unstable:true ()));
  let { coverage; _ } : 'a Soteria.Coverage.with_coverage =
    Soteria.Coverage.As_ctx.with_coverage () @@ fun () ->
    run ~coverage:Handled ~mode:UX process
  in
  Soteria.Coverage.JsonWriter.to_formatter Fmt.stdout coverage;
  Fmt.pr "@.@.";
  Soteria.Coverage.CoberturaWriter.to_formatter Fmt.stdout coverage;
  Fmt.pr "@.@.";
  Soteria.Coverage.LcovWriter.to_formatter Fmt.stdout coverage;
  Fmt.pr "@."
