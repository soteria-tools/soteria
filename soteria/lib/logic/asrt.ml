open Soteria_std
open Logs.Import
module Var = Symex.Var

module M (Symex : Symex.Base) = struct
  (** Atoms of separation logic assertions are SL assertions that do not contain
      separating conjunctions.

      This module contain the signature required of such atoms, and small
      combinators to build atoms. An assertion (described in [Asrt]) is a list
      of atoms. *)

  open Symex

  module type Base = Sym_states.Base.M(Symex).S

  type 'a atom = Spatial of 'a | Pure of Value.Expr.t
  [@@deriving show { with_path = false }]

  type 'a t = 'a atom list [@@deriving show { with_path = false }]

  let make ~spatial ~pure =
    List.map (fun x -> Spatial x) spatial @ List.map (fun x -> Pure x) pure

  (** [Execute] contains the utilities to perform {e production} and
      {e consumption} of a given assertion, using the consumer and producer of
      the parameter [B]. *)
  module Execute (B : Base) = struct
    let produce_atom atom st =
      let open Producer.Syntax in
      match atom with
      | Spatial p -> B.produce p st
      | Pure f ->
          let+ () = Symex.Producer.produce_pure f in
          st

    let produce (asrt : B.syn t) (st : B.t option) : B.t option Producer.t =
      Producer.fold_list ~init:st ~f:(Fun.flip produce_atom) asrt

    let subst_covers subst expr =
      let exception Not_covered in
      try
        let () =
          ignore
          @@ Value.Expr.Subst.apply
               ~missing_var:(fun _ _ -> raise_notrace Not_covered)
               subst expr
        in
        true
      with Not_covered -> false

    let ins_outs (a : B.syn atom) =
      match a with Spatial p -> B.ins_outs p | Pure f -> ([ f ], [])

    (** Returns whether the atom can be consumed. This is true only if
        - The substitution covers all in-parameters
        - All out-parameters are either known or can be learned *)
    let is_consumable (subst : Value.Expr.Subst.t) (atom : B.syn atom) : bool =
      let ins, outs = ins_outs atom in
      let learnable_pairs =
        List.mapi
          (fun i syn ->
            let dummy_value =
              (* These values are toxic, do not use them in the symex! *)
              Value.mk_var (Var.of_int i) (Value.Expr.ty syn)
            in
            (syn, dummy_value))
          outs
      in
      List.for_all (subst_covers subst) ins
      && List.for_all
           (fun (syn, v) ->
             (* Syn.learn returns [Some _] if we can learn everything *)
             Option.is_some (Value.Expr.Subst.learn subst syn v))
           learnable_pairs

    let consume_atom atom st =
      let open Consumer.Syntax in
      [%l.debug "@[Consuming atom:@ %a@]" (pp_atom B.pp_syn) atom];
      match atom with
      | Spatial pred -> B.consume pred st
      | Pure f ->
          let+ () = Symex.Consumer.consume_pure f in
          st

    let consume (asrt : B.syn t) (st : B.t option) :
        (B.t option, B.syn list) Consumer.t =
      let open Consumer.Syntax in
      let* subst = Consumer.expose_subst () in
      [%l.debug
        "@[<v>@[About to consume asrt:@ %a@]@ @[in subst:@ %a@]@ @[and current \
         state:@ %a@]@]"
        (pp B.pp_syn) asrt Value.Expr.Subst.pp subst (Fmt.Dump.option B.pp) st];
      let rec aux (remaining : B.syn t) (st : B.t option) :
          (B.t option, B.syn list) Consumer.t =
        if List.is_empty remaining then Consumer.ok st
        else
          let* subst = Consumer.expose_subst () in
          match List.find_with_rest (is_consumable subst) remaining with
          | None ->
              [%l.info
                "@[<v>Failed to consume assertion because I can't find any \
                 consumable atom left given my current substitution.@.@[<v \
                 2>Substitution:@ %a@]@.@[<v 2>Atoms left:@ %a@]@]"
                Value.Expr.Subst.pp subst (pp B.pp_syn) remaining];
              Consumer.lfail @@ Value.of_bool false
          | Some (atom, rest) ->
              let* st' = consume_atom atom st in
              aux rest st'
      in
      aux asrt st
  end
end
