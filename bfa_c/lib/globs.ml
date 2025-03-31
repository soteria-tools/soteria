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
  let fresh () = Csymex.nondet Typed.t_loc
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

let get loc st =
  let open Csymex.Syntax in
  let+ res = (Sym_map.wrap GlobFn.load) loc st in
  Bfa_symex.Compo_res.get_ok res

let produce serialized st = (Sym_map.produce GlobFn.produce) serialized st

(* For pure predicates in UX, consume = produce *)
let consume serialized st = (Sym_map.consume GlobFn.consume) serialized st
let empty = None
