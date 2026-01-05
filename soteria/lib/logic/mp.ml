(** Matching plans in Soteria are an extended version of the work presented in
    {{:https://doi.org/10.4230/LIPIcs.ECOOP.2024.26}this paper}. *)

open Soteria_std
module Var = Symex.Var

module M (Symex : Symex.S) = struct
  open Symex

  (** An atom is a basic unit of a separation logic assertion. A separation
      logic assertion is a separation conjunction of atoms (represented by a
      list of atoms) *)
  module type Atom = sig
    type syn := Value.Syn.t
    type t [@@deriving show]
    type state

    val ins_outs : t -> syn list * syn list
    val produce : t -> state -> state Producer.t
    val consume : t -> state -> (state, t) Consumer.t
  end

  module Asrt (Atom : Atom) = struct
    type t = Atom.t list

    let produce (asrt : t) (st : Atom.state) : Atom.state Producer.t =
      Producer.fold_list ~init:st ~f:(Fun.flip Atom.produce) asrt

    let subst_covers subst expr =
      let exception Not_covered in
      try
        let () =
          ignore
          @@ Value.Syn.subst
               ~missing_var:(fun _ _ -> raise Not_covered)
               subst expr
        in
        true
      with Not_covered -> false

    (** Returns whether the atom can be consumed. This is true only if
        - The substitution covers all in-parameters
        - All out-parameters are either known or can be learned *)
    let is_consumable (subst : Value.Syn.Subst.t) (atom : Atom.t) : bool =
      let ins, outs = Atom.ins_outs atom in
      let learnable_pairs =
        List.mapi
          (fun i syn ->
            let dummy_value =
              (* These values are toxic, do not use them in the symex! *)
              Value.mk_var (Var.of_int i) (Value.Syn.ty syn)
            in
            (syn, dummy_value))
          outs
      in
      List.for_all (subst_covers subst) ins
      && List.for_all
           (fun (syn, v) ->
             (* Syn.learn returns [Some _] if we can learn everything *)
             Option.is_some (Value.Syn.learn subst syn v))
           learnable_pairs

    let consume (asrt : t) (st : Atom.state) : (Atom.state, Atom.t) Consumer.t =
      let open Consumer.Syntax in
      let rec aux (remaining : t) (st : Atom.state) :
          (Atom.state, Atom.t) Consumer.t =
        if List.is_empty remaining then Consumer.ok st
        else
          let* subst = Consumer.expose_subst () in
          match List.find_with_rest (is_consumable subst) remaining with
          | None ->
              failwith
                "No consumable atom found, let's figure out what to do with \
                 this corner case"
          | Some (atom, rest) ->
              let* st' = Atom.consume atom st in
              aux rest st'
      in
      aux asrt st
  end
end
