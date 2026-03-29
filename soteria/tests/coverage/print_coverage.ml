open Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values.Typed

let process : (int, int, unit) Result.t =
  let* b1 = nondet t_bool in
  let* b2 = nondet t_bool in
  let branch_id = "tests/sample-branch" in
  let branch_span =
    { Soteria.Coverage.file = "sample.c"; line = 7; branch_id }
  in
  let* x = if%sat[@span branch_span] b1 then return 1 else return 2 in
  let* y = if%sat[@span branch_span] b2 then return 3 else return 4 in
  Result.ok (x + y)

let () =
  Soteria.Logs.Config.(set_and_lock (make ~hide_unstable:true ()));
  let { coverage; _ } : 'a Soteria.Coverage.with_coverage =
    Soteria.Coverage.As_ctx.with_coverage () @@ fun () ->
    Soteria.Coverage.As_ctx.mark_line ~file:"sample.c" ~line:3;
    Soteria.Coverage.As_ctx.mark_line ~file:"sample.c" ~line:3;
    Soteria.Coverage.As_ctx.mark_line ~file:"sample.c" ~line:4;
    run ~coverage:Handled ~mode:UX process
  in
  let report = Soteria.Coverage.to_report coverage in
  Soteria.Coverage.JsonWriter.to_formatter Fmt.stdout report;
  Fmt.pr "@.@.";
  Soteria.Coverage.CoberturaWriter.to_formatter Fmt.stdout
    (Soteria.Coverage.to_report coverage);
  Fmt.pr "@."
