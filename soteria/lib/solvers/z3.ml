open Simple_smt
open Logs.Import

(** The type of values the solver operators on. *)
module type Value = sig
  type t
  type ty

  (** A list of commands that need to be aknowledged on start up of the solver.
      These are also sent everytime [Solver_interface.S.reset] is called. *)
  val init_commands : sexp list

  (** Encode a type into a SMTLib sort *)
  val sort_of_ty : ty -> sexp

  (** Encode a value into a SMTLib value *)
  val encode_value : t -> sexp
end

module StatKeys = struct
  let check_sats = "solvers.z3.check_sats"

  let () =
    Stats.register_int_printer ~name:"Z3 check-sat calls" check_sats (fun _ ->
        Fmt.int)
end

module Make (Value : Value) :
  Solver_interface.S with type value = Value.t and type ty = Value.ty = struct
  let initialize_solver : (Simple_smt.solver -> unit) ref =
    ref (fun solver -> List.iter (ack_command solver) Value.init_commands)

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
            ack_command solver (set_option ":timeout" (string_of_int timeout)))

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
          Sexplib.Sexp.output_hum oc sexp;
          flush oc

    let log_response response elapsed =
      match channel () with
      | None -> ()
      | Some oc ->
          let fmt = Format.formatter_of_out_channel oc in
          Fmt.pf fmt " ; -> %a (%a)\n@?" Sexplib.Sexp.pp_hum response
            Logs.Printers.pp_time elapsed
  end

  type t = Simple_smt.solver
  type value = Value.t
  type ty = Value.ty

  let solver_config () =
    { z3 with log = solver_log; exe = (Config.get ()).z3_path }

  let init () =
    let solver = new_solver (solver_config ()) in
    let command sexp =
      Dump.log_sexp sexp;
      let now = Unix.gettimeofday () in
      let res = solver.command sexp in
      let elapsed = Unix.gettimeofday () -. now in
      Dump.log_response res elapsed;
      res
    in
    let solver = { solver with command } in
    !initialize_solver solver;
    solver

  let declare_var solver name ty =
    let name = Symex.Var.to_string name in
    let ty = Value.sort_of_ty ty in
    let sexp = declare name ty in
    ack_command solver sexp

  let add_constraint solver v =
    let v = Value.encode_value v in
    let sexp = Simple_smt.assume v in
    ack_command solver sexp

  let check_sat solver : Symex.Solver_result.t =
    Stats.As_ctx.incr StatKeys.check_sats;
    let smt_res =
      try check solver
      with Simple_smt.UnexpectedSolverResponse s ->
        L.error (fun m ->
            m "Unexpected solver response: %s" (Sexplib.Sexp.to_string_hum s));
        Unknown
    in
    match smt_res with
    | Sat -> Sat
    | Unsat -> Unsat
    | Unknown ->
        L.info (fun m -> m "Solver returned unknown");
        Unknown

  let push solver n = ack_command solver (Simple_smt.push n)
  let pop solver n = ack_command solver (Simple_smt.pop n)

  let reset solver =
    ack_command solver Smt_utils.reset;
    !initialize_solver solver

  let get_model solver = try Some (Simple_smt.get_model solver) with _ -> None
end
