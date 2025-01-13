module Bi_interp = Interp.Make (Bi_heap)
open Ail_tys
module T = Typed.T

module Summary = struct
  type 'err t = {
    args : T.cval Typed.t list;
        (** List of arguments values, corresponding to the formal arguments in
            order. Really a form of [(x == a0) * (y == a1)] *)
    pre : Heap.serialized list;  (** Pre-condition as a list of fixes *)
    pc : Svalue.t list;
        (** Path condition. Whether it is in the post or in the pre, it doesn't
            matter for UX. *)
    post : Heap.serialized;  (** Post condition as a serialized heap *)
    ret : (T.cval Typed.t, 'err) result;
        (** Return value. If `ok` then it is the C value that the function
            returned, if `err` then it is a description of the bug exhibitied by
            the code *)
  }
  [@@deriving show { with_path = false }]
end

module Summaries = struct
  module H = Hashtbl.Make (struct
    type t = Cerb_frontend.Symbol.sym

    let equal = Cerb_frontend.Symbol.equal_sym
    let hash = Hashtbl.hash
  end)
end

let generate_summaries_for ~prog (fundef : fundef) =
  let open Syntaxes.List in
  let fid, (floc, _, _, _, _) = fundef in
  let* arg_tys =
    match Ail_helpers.get_param_tys ~prog fid with
    | None ->
        L.info (fun m ->
            m "No argument types found for %a at loc %a" Fmt_ail.pp_sym fid
              Fmt_ail.pp_loc floc);
        []
    | Some arg_tys -> [ arg_tys ]
  in
  let process =
    let open Csymex.Syntax in
    let* args = Csymex.all (List.map Layout.nondet_c_ty arg_tys) in
    let* result = Bi_interp.exec_fun ~prog ~args ~state:Bi_heap.empty fundef in
    match result with
    | Ok (ret, bi_heap) -> Csymex.return (args, Ok ret, bi_heap)
    | Error (err, bi_heap) -> Csymex.return (args, Error err, bi_heap)
    | Missing _ -> Csymex.vanish ()
  in
  let+ (args, ret, bi_heap), pc = Csymex.run process in
  let pre, post = Bi_heap.to_spec bi_heap in
  Summary.{ args; pre; pc; post; ret }

let generate_all_summaries prog =
  let order = Call_graph.weak_topological_order (Call_graph.of_prog prog) in
  ListLabels.filter_map order ~f:(fun fid ->
      let open Syntaxes.Option in
      let+ fundef = Ail_helpers.find_fun_sym ~prog fid in
      let summaries = generate_summaries_for ~prog fundef in
      (fid, summaries))
