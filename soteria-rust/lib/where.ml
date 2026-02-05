module Terminal = Soteria.Terminal

(** Represents the current position in analysis. *)
type t = {
  op : string option;
      (** Current operation being performed, only if relevant *)
  loc : Charon.Meta.span_data option;  (** Current code location if known *)
  stack : Charon.Meta.span_data Soteria.Terminal.Call_trace.t;
      (** Current call stack *)
}

let pp ft t =
  let pp_loc =
    Soteria.Logs.Printers.pp_unstable ~name:"loc" Charon.Meta.pp_span_data
  in
  let pp_loc_opt : Charon.Meta.span_data option Fmt.t =
    Fmt.option ~none:(Fmt.any "unknown location") pp_loc
  in
  let pp_op ft = function Some op -> Fmt.pf ft " during %s" op | None -> () in
  let pp_stack = Soteria.Terminal.Call_trace.pp pp_loc in
  Fmt.pf ft "%a%a with trace@[<hov 1>@ %a@]" pp_loc_opt t.loc pp_op t.op
    pp_stack t.stack

let move_to loc where = { where with loc = Some loc }

let move_to_opt loc where =
  match loc with Some loc -> move_to loc where | None -> where

let set_op op where = { where with op = Some op }

let add_to_stack ~loc ~msg where =
  let call = Soteria.Terminal.Call_trace.mk_element ~loc ~msg () in
  { where with stack = call :: where.stack }

let loc_or_default where =
  match where.loc with Some loc -> loc | None -> Charon_util.empty_span_data

let nowhere =
  { op = None; loc = None; stack = Soteria.Terminal.Call_trace.empty }
