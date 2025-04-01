(** The state of globals is a map from identifier to address. We model it as an
    immutable total map from identifier to address *)

open Csymex
module Sym_map = Concrete_map (Ail_helpers.Symbol_std)

module Loc :
  Bfa_symex.Pure_fun.Codom
    with type t = Typed.T.sloc Typed.t
     and module Symex = Csymex = struct
  module Symex = Csymex

  type t = Typed.T.sloc Typed.t

  let pp = Typed.ppa

  let fresh () =
    let open Typed.Infix in
    Csymex.nondet
      ~constrs:(fun x -> [ Typed.not (x ==@ Typed.Ptr.null_loc) ])
      Typed.t_loc

  let sem_eq = Typed.sem_eq
  let subst = Typed.subst
  let iter_vars = Typed.iter_vars
end

module GlobFn = Bfa_symex.Pure_fun.Make (Loc)

type t = GlobFn.t Sym_map.t option [@@deriving show { with_path = false }]

type serialized = GlobFn.serialized Sym_map.serialized
[@@deriving show { with_path = false }]

let serialize st =
  match st with None -> [] | Some st -> Sym_map.serialize GlobFn.serialize st

let subst_serialized subst_var serialized =
  Sym_map.subst_serialized GlobFn.subst_serialized subst_var serialized

let iter_vars_serialized s =
  (Sym_map.iter_vars_serialized GlobFn.iter_vars_serialized) s

let get sym st =
  (* TODO: This is correct, but a tiny bit of a hack.
           We need to enforce the invariant that all symbols are different.
           It'd be nice to enforce this modularly, but right now we do it as a wrapper.
           Unless one has hundreds of globals, the cost is negligeable, but still. *)
  let open Csymex.Syntax in
  let existed = Option.fold ~none:false ~some:(Sym_map.M.mem sym) st in
  let* res = (Sym_map.wrap GlobFn.load) sym st in
  let loc, st = Bfa_symex.Compo_res.get_ok res in
  let+ () =
    if existed then (* We haven't created a new location *) return ()
    else
      (* We learn that the new location is distinct from all other global locations *)
      let to_assume =
        Sym_map.M.to_seq (Option.value ~default:Sym_map.M.empty st)
        |> Seq.filter_map (fun (k, v) ->
               let open Typed.Infix in
               if Cerb_frontend.Symbol.equal_sym k sym then None
               else
                 let neq = Typed.not (v ==@ loc) in
                 Some neq)
        |> List.of_seq
      in
      assume to_assume
  in
  (loc, st)

let produce serialized st =
  let open Csymex.Syntax in
  let* st' = (Sym_map.produce GlobFn.produce) serialized st in
  let+ () =
    (* Bit heavy-handed but we just massively assume the well-formedness *)
    let to_assume =
      Sym_map.M.to_seq (Option.value ~default:Sym_map.M.empty st')
      |> Bfa_std.Utils.Seq_ex.self_cross_product
      |> Seq.map (fun ((_, loca), (_, locb)) ->
             let open Typed.Infix in
             Typed.not (loca ==@ locb))
      |> List.of_seq
    in
    assume to_assume
  in
  st'

(* For pure predicates in UX, consume = produce *)
let consume serialized st = (Sym_map.consume GlobFn.consume) serialized st
let empty = None
