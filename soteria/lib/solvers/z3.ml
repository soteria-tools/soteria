open Smt
open Logs.Import

module StatKeys = struct
  let check_sats = "solvers.z3.check_sats"

  let () =
    Stats.register_int_printer ~name:"Z3 check-sat calls" check_sats (fun _ ->
        Fmt.int)
end

(** Create a Z3 solver module from a value type. It can be configured, see
    {!Config}. *)
module Make (Value : Value.S) :
  Solver_interface.S with type value = Value.t and type ty = Value.ty = struct
  let initialize_solver : (Smt.solver -> unit) ref =
    ref (fun solver ->
        command solver (set_option ":produce-models" "true");
        List.iter (command solver) Value.init_commands)

  let register_solver_init f =
    let old = !initialize_solver in
    let f' solver =
      old solver;
      f solver
    in
    initialize_solver := f'

  let () =
    register_solver_init (fun solver ->
        match (Config.get ()).solver_timeout with
        | None -> ()
        | Some timeout ->
            command solver (set_option ":timeout" (string_of_int timeout)))

  let solver_log =
    let debug ~prefix thunk = [%l.smt "%s: %s" prefix (thunk ())] in
    { send = debug ~prefix:"-> "; receive = debug ~prefix:"<- "; stop = Fun.id }

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
      (* We only open if current file is not None and its different from current
         config *)
      match ((Config.get ()).dump_smt_file, !current_channel) with
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
          Smt.output_sexp oc sexp;
          flush oc

    let log_response response elapsed =
      match channel () with
      | None -> ()
      | Some oc ->
          let fmt = Format.formatter_of_out_channel oc in
          Fmt.pf fmt " ; -> %a (%a)\n@?" Smt.pp_sexp response
            Logs.Printers.pp_time elapsed
  end

  type t = Smt.solver
  type value = Value.t
  type ty = Value.ty

  let solver_config () =
    { z3 with log = solver_log; exe = (Config.get ()).z3_path }

  let init () =
    let solver = new_solver (solver_config ()) in
    let ack_command sexp =
      Dump.log_sexp sexp;
      let now = Unix.gettimeofday () in
      let res = solver.ack_command sexp in
      let elapsed = Unix.gettimeofday () -. now in
      Dump.log_response res elapsed;
      res
    in
    let command sexp =
      Dump.log_sexp sexp;
      solver.command sexp
    in
    let solver = { solver with ack_command; command } in
    !initialize_solver solver;
    solver

  let declare_var solver name ty =
    let name = Symex.Var.to_string name in
    let ty = Value.sort_of_ty ty in
    let sexp = declare name ty in
    command solver sexp

  let add_constraint solver v =
    let v = Value.encode_value v in
    let sexp = Smt.assume v in
    command solver sexp

  let check_sat solver : Symex.Solver_result.t =
    Stats.As_ctx.incr StatKeys.check_sats;
    let smt_res =
      try check solver
      with Smt.UnexpectedSolverResponse s ->
        [%l.error "Unexpected solver response: %s" (Smt.to_string s)];
        Unknown
    in
    match smt_res with
    | Sat -> Sat
    | Unsat -> Unsat
    | Unknown ->
        [%l.info "Solver returned unknown"];
        Unknown

  let push solver n = command solver (Smt.push n)
  let pop solver n = command solver (Smt.pop n)
  let save solver = push solver 1
  let backtrack_n solver n = pop solver n

  let reset solver =
    command solver reset;
    !initialize_solver solver

  let get_model solver = try Some (Smt.get_model solver) with _ -> None
end
