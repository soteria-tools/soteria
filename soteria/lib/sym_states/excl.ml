(** This module contains constructor to build a state composed of a single
    exclusively-owned value (in the Separation Logic sense). *)

open Symex

module Elem (Symex : Logic.Symex_with_syntax.Base) = struct
  open Symex

  module type With_syntax = sig
    type t
    type syn

    val fresh : unit -> t Symex.t
    val to_syn : t -> syn
    val iter_vars : t -> (Var.t * 'a Value.ty -> unit) -> unit
  end
end

module Make_raw (Symex : Symex.Base) = struct
  (** This module describes exclusive ownership of a value. Additional inputs
      are required, to describe what happens in case of [miss] error. It is
      recommended to use [Make] or [Make_with_logic] directly. *)

  open Symex
  open Symex.Syntax

  type 'a t = 'a

  let unwrap ~fresh ~to_syn st =
    match st with
    | Some x -> Symex.Result.ok x
    | None ->
        let* v = fresh () in
        Result.miss [ to_syn v ]

  let assert_exclusively_owned ~fresh ~to_syn (st : 'a t option) =
    let** _ = unwrap ~fresh ~to_syn st in
    Result.ok ()

  let load ~fresh ~to_syn (st : 'a t option) =
    let++ x = unwrap ~fresh ~to_syn st in
    (x, st)

  let store ~fresh ~to_syn x (st : 'a t option) =
    let++ _ = unwrap ~fresh ~to_syn st in
    ((), Some x)
end

module Make (Symex : Symex.Base) = struct
  (** This module describes exclusive ownership of a value. It has no notion of
      logic and syntax, it is purely semantics. This means it cannot be used to
      perform, e.g. bi-abduction or any creation/manipulation of summaries. *)

  open Symex
  open Symex.Syntax
  module Raw = Make_raw (Symex)

  let fresh () = Symex.return ()
  let to_syn () = ()
  let unwrap st = Raw.unwrap ~fresh ~to_syn st

  let assert_exclusively_owned st =
    Raw.assert_exclusively_owned ~fresh ~to_syn st

  let load st = Raw.load ~fresh ~to_syn st
  let store v st = Raw.store ~fresh ~to_syn v st
end

module Make_with_logic
    (Symex : Logic.Symex_with_syntax.Base)
    (E : Elem(Symex).With_syntax) =
struct
  open Symex

  type t = E.t
  type syn = E.syn

  module Syntax = Logic.Syntax.M (Symex)
  module Subst = Syntax.Subst (Symex.Expr)

  (* TODO: right now the type of semantic (Value.t) and syntactic are the same.
      Ideally, we should have some type-level protection of `x`
      so that it cannot be used before it has been substituted.
      
      I'm thinking something à la EarlyBinders type in Rustc.
      I spent some time trying to figure out something elegant but I'm really trying to stop bike-shedding. *)
  let produce (x : syn) (st : t option) (subst : Subst.t) :
      (t option * Subst.t) Symex.t =
    let open Symex.Syntax in
    match st with
    | Some _ -> Symex.vanish ()
    | None ->
        let+ x, subst = E.subst subst x in
        (Some x, subst)

  let consume (x : serialized) (st : t option) (subst : Subst.t) :
      (t option * Subst.t, Symex.lfail, serialized) Symex.Result.t =
    let open Symex.Syntax in
    match st with
    | None -> Symex.Result.miss [ x ]
    | Some y ->
        let+ subst = E.learn_eq subst (E.to_syn y) x 

  let subst_serialized subst_var x = E.subst subst_var x
end
