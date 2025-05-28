open Simple_smt
module L = Soteria_logs.Logs.L

let solver_log =
  let debug_str ~prefix s = L.smt (fun m -> m "%s: %s" prefix s) in
  {
    send = debug_str ~prefix:"-> ";
    receive = debug_str ~prefix:"<- ";
    stop = Fun.id;
  }

module Dump = struct
  let current_channel = ref None

  let close_channel () =
    match !current_channel with
    | None -> ()
    | Some (oc, _) ->
        close_out oc;
        current_channel := None

  let () = at_exit close_channel

  let open_channel f =
    let oc = open_out f in
    current_channel := Some (oc, f);
    Some oc

  let channel () =
    (* We only open if current file is not None and its different from current config *)
    match (!Solver_config.current.dump_smt_file, !current_channel) with
    | None, None -> None
    | Some f, None -> open_channel f
    | Some f, Some (oc, f') ->
        if f == f' then Some oc
        else (
          close_channel ();
          open_channel f)
    | None, Some _ ->
        close_channel ();
        None

  let log_sexp sexp =
    match channel () with
    | None -> ()
    | Some oc ->
        Sexplib.Sexp.output_hum oc sexp;
        flush oc

  let log_response response elapsed =
    match channel () with
    | None -> ()
    | Some oc ->
        output_string oc " ; -> ";
        Sexplib.Sexp.output_hum oc response;
        if elapsed > 0 && not !Solver_config.current.hide_response_times then (
          output_string oc " (";
          output_string oc (string_of_int elapsed);
          output_string oc "ms)");
        output_char oc '\n';
        flush oc
end

type t = Simple_smt.solver

let solver_config () =
  { z3 with log = solver_log; exe = !Solver_config.current.z3_path }

let create () =
  let solver = new_solver (solver_config ()) in
  let command sexp =
    Dump.log_sexp sexp;
    let now = Unix.gettimeofday () in
    let res = solver.command sexp in
    let elapsed = Int.of_float ((Unix.gettimeofday () -. now) *. 1000.) in
    Dump.log_response res elapsed;
    res
  in
  { solver with command }

let initialize_solver : (Simple_smt.solver -> unit) ref = ref (fun _ -> ())

let register_solver_init f =
  let old = !initialize_solver in
  let f' solver =
    old solver;
    f solver
  in
  initialize_solver := f'

let () =
  register_solver_init (fun solver ->
      match !Solver_config.current.solver_timeout with
      | None -> ()
      | Some timeout ->
          ack_command solver (set_option ":timeout" (string_of_int timeout)))

let execute_init exe = !initialize_solver exe
