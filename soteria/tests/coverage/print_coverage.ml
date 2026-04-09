open Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values.Typed

let file = "my.file"

let branch_span fn line id : Soteria.Coverage.branch_span =
  {
    loc = Function fn;
    line;
    branch_id = "b_" ^ string_of_int line ^ "_" ^ string_of_int id;
  }

let process () : (unit, 'e, 'f) Result.t =
  let open Soteria.Coverage in
  let open Soteria.Coverage.As_ctx in
  let my_fun = { file; name = "my_fun"; line = Some 1; end_line = None } in
  let other_fun =
    { file; name = "other_fun"; line = Some 15; end_line = None }
  in
  register_function other_fun;
  register_function my_fun;
  register_bulk
    (Iter.singleton
       (Function my_fun, Iter.map (fun line -> Line line) Iter.(1 -- 7)));
  register_bulk
    (Iter.singleton
       (Function other_fun, Iter.map (fun line -> Line line) Iter.(15 -- 18)));
  mark_function my_fun;
  let* b1 = nondet t_bool in
  mark (Function my_fun) (Line 1);
  let* () =
    if%sat[@span branch_span my_fun 2 1] b1 then return () else return ()
  in
  mark (Function my_fun) (Line 3);
  let* () =
    if%sat[@span branch_span my_fun 4 1] v_false then return ()
    else if%sat[@span branch_span my_fun 4 2] v_true then return ()
    else return ()
  in
  mark (Function my_fun) (Line 5);
  let* () = if%sat[@span_opt None] b1 then return () else return () in
  let* () =
    if%sat[@span_opt Some (branch_span my_fun 6 1)] b1 then return ()
    else return ()
  in
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
