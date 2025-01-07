open Bfa_symex
module Bi_interp = Interp.Make (Bi_heap)
open Csymex.Syntax
open Ail_tys
open Cerb_frontend
open Typed.Syntax
module T = Typed.T

module Summary = struct
  (** A summary is a quadruple compose of:
      - A list of arguments, corresponding to the formal arguments in order
      - A precondition
      - A postcondition
      - A path condition
      - A return value *)

  type t = {
    args : T.cval Typed.t list;
    pre : Heap.serialized list;
    pc : Svalue.t list;
    post : Heap.serialized;
    ret : T.cval Typed.t;
  }
  [@@deriving show]
end

module Summaries = struct
  module H = Hashtbl.Make (struct
    type t = Cerb_frontend.Symbol.sym

    let equal = Cerb_frontend.Symbol.equal_sym
    let hash = Hashtbl.hash
  end)
end

let nondet_c_ty (ty : Ctype.ctype) : T.cval Typed.t Csymex.t =
  match Ctype.proj_ctype_ ty with
  | Void -> Csymex.return 0s
  | Pointer _ ->
      let* loc = Typed.nondet Typed.t_loc in
      let* ofs = Typed.nondet Typed.t_int in
      Csymex.return (Typed.Ptr.mk loc ofs)
  | Basic (Integer ity) ->
      let constrs = Layout.int_constraints ity |> Option.get in
      let+ res = Typed.nondet ~constrs Typed.t_int in
      (res :> Typed.T.cval Typed.t)
  | Basic (Floating _) -> Csymex.not_impl "nondet_c_ty: floating"
  | Array _ | Function _ | FunctionNoParams _ | Struct _ | Union _ | Atomic _ ->
      Csymex.not_impl "nondet_c_ty: unsupported type"

let generate_summaries ~prog (fundef : fundef) =
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
    let* args = Csymex.all (List.map nondet_c_ty arg_tys) in
    let++ ret, bi_heap =
      Bi_interp.exec_fun ~prog ~args ~state:Bi_heap.empty fundef
    in
    (args, ret, bi_heap)
  in
  let+ res, pc = Csymex.run process in
  Compo_res.map res (fun (args, ret, bi_heap) ->
      let pre, post = Bi_heap.to_spec bi_heap in
      Summary.{ args; pre; pc; post; ret })

(* let serialized_state, path_condition, args =
        if Bi_subst.is_empty bi_subst then
          (serialized_state, path_condition, args)
        else
          let subst_var = Bi_subst.forward bi_subst in
          ( Heap.subst_serialized subst_var serialized_state,
            List.map (Svalue.subst subst_var) path_condition,
            List.map (Typed.subst subst_var) args )
      in
      let* () = Csymex.assume path_condition in
      let* state = Bi_heap.produce serialized_state Bi_heap.empty in
      let++ res, _final_state = Bi_interp.exec_fun ~prog ~args ~state fundef in
      let backward_subst = Bi_subst.backward bi_subst in
      (Typed.subst backward_subst res, 0) *)
