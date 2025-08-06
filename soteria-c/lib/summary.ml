open Soteria_terminal
open Syntaxes.FunctionWrap
module T = Typed.T
module Var = Soteria_symex.Var

type after_exec = [ `After_exec ]
type pruned = [ `Pruned ]
type analysed = [ `Analysed ]

let leaks_to_error ~(loc : Cerb_location.t) leaks =
  let elements =
    List.map
      (fun loc -> Call_trace.mk_element ~loc ~msg:"This allocation leaked" ())
      leaks
  in
  let leak_msg = Call_trace.singleton ~loc () in
  (`Memory_leak, elements @ leak_msg)

type raw = {
  args : Aggregate_val.t list;
      (** List of arguments values, corresponding to the formal arguments in
          order. Really a form of [(x == a0) * (y == a1)] *)
  pre : State.serialized list;  (** Pre-condition as a list of fixes *)
  pc : Typed.T.sbool Typed.t list;
      (** Path condition. Whether it is in the post or in the pre, it doesn't
          matter for UX. *)
  post : State.serialized;  (** Post condition as a serialized heap *)
  ret :
    ( Aggregate_val.t,
      Error.t * (Cerb_location.t[@printer Fmt_ail.pp_loc]) Call_trace.t )
    result;
      (** Return value. If `ok` then it is the C value that the function
          returned, if `err` then it is a description of the bug exhibitied by
          the code *)
}
[@@deriving show { with_path = false }]

type _ t =
  | After_exec : raw -> after_exec t
  | Pruned : {
      raw : raw;
      memory_leaks : Cerb_location.t list option;
          (** If [None], no memory leak was detected. If [Some l], then some
              memory leak was detected and, in addition, we return a list of all
              *known* code location where something that leaked was allocated.
              It is possible for this field to be [Some []], which means that
              there was a leak, but we did not track any allocation site for it.
          *)
    }
      -> pruned t
  | Analysed : {
      raw : raw;
      manifest_bugs : (Error.t * Cerb_location.t Call_trace.t) list;
    }
      -> analysed t
(* [@@deriving show { with_path = false }] *)

let pp : type a. Format.formatter -> a t -> unit =
 fun ft summary ->
  match summary with
  | After_exec raw -> Fmt.pf ft "@[<2>After_exec@ %a@]" (Fmt.parens pp_raw) raw
  | Pruned { raw; memory_leaks } ->
      Fmt.pf ft "@[<v 2>Pruned {@ @[raw =@ %a@];@ @[memory_leak =@ %a@]}@]"
        pp_raw raw
        (Fmt.Dump.option @@ Fmt.Dump.list Fmt_ail.pp_loc)
        memory_leaks
  | Analysed { raw; manifest_bugs } ->
      Fmt.pf ft "@[<v 2>Analysed {@ @[raw =@ %a@];@ @[manifest_bugs =@ %a@]}@]"
        pp_raw raw
        (Fmt.Dump.list (Fmt.Dump.pair Error.pp (Call_trace.pp Fmt_ail.pp_loc)))
        manifest_bugs

module Var_graph = Graph.Make_in_place (Var)
module Var_hashset = Var_graph.Node_set

let filter_pc relevant_vars pc =
  ListLabels.filter pc ~f:(fun v ->
      Iter.exists
        (fun (var, _) -> Var_hashset.mem relevant_vars var)
        (Typed.iter_vars v))

(** Removes any bit of the state that does not any "relevant variables". i.e.,
    bits that are not reachable from the precondition. *)
let filter_serialized_state relevant_vars (state : State.serialized) =
  (* leak_origins tracks the source code location of allocation for each heap location that was detected to leak.
     If empty, no leak is detected. *)
  let leak_origins = ref [] in
  let resulting_heap =
    ListLabels.filter state.heap ~f:(fun (loc, b) ->
        let relevant =
          Iter.exists
            (fun (var, _) -> Var_hashset.mem relevant_vars var)
            (Typed.iter_vars loc)
        in
        if relevant then true
        else
          (* If the block is not freed, we record where the object was allocated *)
          let leaked = not (Block.is_freed b) in
          if leaked then leak_origins := b.info :: !leak_origins;
          L.trace (fun m ->
              m "Filtering out unreachable location: %a which %a." Typed.ppa loc
                (fun ft b ->
                  if b then Fmt.pf ft "leaked" else Fmt.pf ft "did not leak")
                leaked);
          false)
  in
  (* Globals are not filtered: if they are in the spec, they were bi-abduced and necessary *)
  let resulting_state = { state with heap = resulting_heap } in
  (resulting_state, !leak_origins)

let init_reachable_vars summary =
  let init_reachable = Var_hashset.with_capacity 0 in
  let mark_reachable x = Var_hashset.add init_reachable x in
  let mark_cval_reachable cval =
    Aggregate_val.iter_vars cval (fun (x, _) -> mark_reachable x)
  in
  (* We mark all variables from the arguments and return value as reachable *)
  let () = List.iter mark_cval_reachable summary.args in
  let () = Result.iter mark_cval_reachable summary.ret in
  let () =
    (* We mark all accessed globals as reachable. *)
    Globs.iter_vars_serialized summary.post.State_intf.Template.globs
      (fun (x, _) -> mark_reachable x)
  in
  init_reachable

let make ~args ~pre ~pc ~post ~ret () = After_exec { args; pre; pc; post; ret }

(* TODO: for below: do we only need to compute reachability of variables of type Loc, since
   they are the only one that cannot be created out of the blue, and need provenance? e*)

(** This function prunes the summary by removing anything that isn't reachable
    from the arguments or the return value. It returns the updated summary, as
    well as a boolean capturing whether a memory leak was detected. A memory
    leak is detected if there was an unreachable block that was not freed. *)
let prune (summary : after_exec t) : pruned t =
  let (After_exec summary) = summary in
  L.trace (fun m -> m "Pruning summary %a" pp_raw summary);
  let module Var_graph = Graph.Make_in_place (Var) in
  let graph = Var_graph.with_node_capacity 0 in
  let init_reachable = init_reachable_vars summary in
  (* For each equality [e1 = e2] in the path condition,
     we add a double edge from all variables of [e1] to all variables of [e2] *)
  ListLabels.iter summary.pc ~f:(fun v ->
      match Typed.kind v with
      | Binop (Eq, el, er) ->
          (* We make the second iterator peristent to avoid going over the structure too many times if there are many *)
          let r_iter = Iter.persistent_lazy (Svalue.iter_vars er) in
          let product = Iter.product (Svalue.iter_vars el) r_iter in
          product (fun ((x, _), (y, _)) -> Var_graph.add_double_edge graph x y)
      | _ -> ());
  (* For each block $l -> B in the pre and post state, we add a single-sided arrow
     from all variables in $l to all variables contained in B. *)
  ListLabels.iter
    (List.concat (List.map (fun (x : State.serialized) -> x.heap) summary.pre)
    @ summary.post.heap)
    ~f:(fun (l, b) ->
      let b_iter = Block.iter_vars_serialized b in
      Iter.product (Typed.iter_vars l) (Iter.persistent_lazy b_iter)
        (fun ((x, _), (y, _)) -> Var_graph.add_edge graph x y));
  (* [init_reachable] is the set of initially-reachable variables, and we have a reachability [graph].
     We can compute all reachable values. *)
  let reachable = Var_graph.reachable_from graph init_reachable in
  (* We can now filter the summary to keep only the reachable values *)
  let new_pc = filter_pc reachable summary.pc in
  let new_post, memory_leaks = filter_serialized_state reachable summary.post in
  let memory_leaks =
    (* Memory leaks only make sense for functions that terminate successfully *)
    match (summary.ret, memory_leaks) with
    | Ok _, (_ :: _ as l) -> Some (List.filter_map Fun.id l)
    | _ -> None
  in
  Pruned { raw = { summary with pc = new_pc; post = new_post }; memory_leaks }

(** Current criterion: a bug is manifest if its path condition is a consequence
    of the heap's and function arguments well-formedness conditions *)
let rec analyse : type a. fid:Ail_tys.sym -> a t -> analysed t =
 fun ~fid summary ->
  match summary with
  | After_exec _ -> analyse ~fid (prune summary)
  | Analysed _ -> summary
  | Pruned { raw = summary; memory_leaks } -> (
      let@ () =
        Soteria_logs.Logs.with_section
          ("Analysing a summary for " ^ Cerb_frontend.Symbol.show_symbol fid)
      in
      L.debug (fun m ->
          m "Analysing a summary for %s@\n%a"
            (Cerb_frontend.Symbol.show_symbol fid)
            pp_raw summary);
      let arg_tys = Option.get (Ail_helpers.get_param_tys fid) in
      match summary.ret with
      | Ok _ ->
          let loc =
            Ail_helpers.find_fun_loc fid
            |> Option.value ~default:Cerb_location.unknown
          in
          let manifest_leak =
            Option.map (leaks_to_error ~loc) memory_leaks |> Option.to_list
          in
          Analysed { raw = summary; manifest_bugs = manifest_leak }
      | Error error ->
          let module Subst = Soteria_symex.Substs.Subst in
          let module From_iter = Subst.From_iter (Csymex) in
          let iter_pc f = List.iter (fun v -> Typed.iter_vars v f) summary.pc in
          let iter_post = State.iter_vars_serialized summary.post in
          let iter_args f =
            List.iter (fun cval -> Aggregate_val.iter_vars cval f) summary.args
          in
          let process =
            let open Csymex.Syntax in
            let* subst =
              From_iter.from_iter
                (Iter.append iter_pc (Iter.append iter_post iter_args))
            in
            let subst = Subst.to_fn subst in
            let args = List.map (Aggregate_val.subst subst) summary.args in
            let constrs =
              List.map2
                (fun arg ty -> Option.get (Layout.constraints ~ty arg))
                args arg_tys
            in
            let constrs = List.concat constrs in
            let* () = Csymex.assume constrs in
            let serialized_heap = State.subst_serialized subst summary.post in
            (* We don't need the produced heap, just its wf condition *)
            (* We might want to use another symex monad, à grisette,
           that produces the condition as a writer monad in an \/ or something *)
            let* _heap = State.produce serialized_heap State.empty in
            let pc = List.map (Typed.subst subst) summary.pc in
            L.trace (fun m ->
                m
                  "Produced heap, about to check if path condition holds in \
                   every branch");
            Csymex.assert_ox (Typed.conj pc)
          in
          let result =
            Csymex.run ~fuel:Soteria_symex.Fuel_gauge.infinite process
          in
          L.trace (fun m ->
              m "Results: %a" (Fmt.Dump.list (Fmt.pair Fmt.bool Fmt.nop)) result);
          (* The bug is manifest if the assert passed in every branch. *)
          (* FIXME: the non-empty check is a bit of a hack.
             We need to somehow be able to run production and assert in OX mode.
             Right now, if production vanishes in one case but not all, we get a false positive. *)
          let is_manifest =
            (not (List.is_empty result)) && List.for_all fst result
          in
          let manifest_bugs = if is_manifest then [ error ] else [] in
          Analysed { raw = summary; manifest_bugs })

let manifest_bugs (type a) ~fid (summary : a t) =
  let (Analysed { manifest_bugs; _ }) = analyse ~fid summary in
  manifest_bugs
