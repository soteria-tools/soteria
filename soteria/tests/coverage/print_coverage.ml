open Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values.Typed

let file = "my.file"

let branch_span line : Soteria.Coverage.source_span =
  { file; line; branch_id = "b_" ^ string_of_int line }

let process : (unit, 'e, 'f) Result.t =
  let* b1 = nondet t_bool in
  Soteria.Coverage.As_ctx.mark_line ~file ~line:1;
  let* () = if%sat[@span branch_span 2] b1 then return () else return () in
  Soteria.Coverage.As_ctx.mark_line ~file ~line:3;
  let* () =
    if%sat[@span branch_span 4] v_false then return ()
    else if%sat[@span branch_span 4] v_true then return ()
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
  let report = Soteria.Coverage.to_report coverage in
  Soteria.Coverage.JsonWriter.to_formatter Fmt.stdout report;
  Fmt.pr "@.@.";
  Soteria.Coverage.CoberturaWriter.to_formatter Fmt.stdout
    (Soteria.Coverage.to_report coverage);
  Fmt.pr "@."
