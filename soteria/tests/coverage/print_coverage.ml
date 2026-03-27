open Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values.Typed

let process : (int, int, unit) Result.t =
  let* b1 = nondet t_bool in
  let* b2 = nondet t_bool in
  let branch_id = "tests/sample-branch" in
  let branch_span =
    Some { Soteria.Coverage.file = "sample.c"; line = 7; branch_id }
  in
  let* x =
    branch_on ?branch_span b1
      ~then_:(fun () -> return 1)
      ~else_:(fun () -> return 2)
  in
  let* y =
    branch_on ?branch_span b2
      ~then_:(fun () -> return 3)
      ~else_:(fun () -> return 4)
  in
  Result.ok (x + y)

let () =
  Soteria.Logs.Config.(set_and_lock (make ~hide_unstable:true ()));
  let with_coverage =
    Soteria.Coverage.As_ctx.with_coverage () @@ fun () ->
    Soteria.Stats.As_ctx.with_stats () @@ fun () ->
    Soteria.Coverage.As_ctx.mark_line ~file:"sample.c" ~line:3;
    Soteria.Coverage.As_ctx.mark_line ~file:"sample.c" ~line:3;
    Soteria.Coverage.As_ctx.mark_line ~file:"sample.c" ~line:4;
    ignore (run_needs_stats ~mode:UX process)
  in
  let coverage = with_coverage.Soteria.Coverage.coverage in
  let report_json = Soteria.Coverage.to_yojson coverage in
  Yojson.Safe.pretty_to_channel stdout report_json;
  Fmt.pr "@.@.";
  Soteria.Coverage.Writers.cobertura.write_to_formatter Fmt.stdout
    (Soteria.Coverage.to_report coverage);
  Fmt.pr "@.";
  Soteria.Coverage.Writers.json.write_to_formatter Fmt.stdout
    (Soteria.Coverage.to_report coverage);
  Fmt.pr "@."
