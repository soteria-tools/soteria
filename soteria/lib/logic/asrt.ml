open Soteria_std
module Var = Symex.Var

module M (Symex : Symex.S) = struct
  (** Atoms of separation logic assertions are SL assertions that do not contain
      separating conjunctions.

      This module contain the signature required of such atoms, and small
      combinators to build atoms. An assertion (described in [Asrt]) is a list
      of atoms. *)

  open Symex

  type ('spatial, 'pure) spatial_or_pure = Spatial of 'spatial | Pure of 'pure
  [@@deriving show]

  module type S = sig
    type syn := Value.Syn.t
    type t [@@deriving show]

    val ins_outs : t -> syn list * syn list
  end

  (* FIXME: I don't like the name "Model" here. *)
  module Model (A : S) = struct
    module type S = sig
      type state
      type fix

      val produce : A.t -> state option -> state option Producer.t
      val consume : A.t -> state option -> (state option, fix list) Consumer.t
    end
  end

  module With_pure (A : S) = struct
    module Atom : S with type t = (A.t, Value.Syn.t) spatial_or_pure = struct
      type t = (A.t, Value.Syn.t) spatial_or_pure [@@deriving show]

      let ins_outs = function
        | Spatial atom -> A.ins_outs atom
        | Pure expr -> ([ expr ], [])
    end

    module Model (M : Model(A).S) :
      Model(Atom).S with type state = M.state and type fix = M.fix = struct
      type state = M.state
      type fix = M.fix

      let produce atom st =
        let open Producer.Syntax in
        match atom with
        | Spatial pred -> M.produce pred st
        | Pure f ->
            let+ () = Symex.Producer.produce_pure f in
            st

      let consume atom st =
        let open Consumer.Syntax in
        match atom with
        | Spatial pred -> M.consume pred st
        | Pure f ->
            let+ () = Symex.Consumer.consume_pure f in
            st
    end
  end

  module Model_with_star (A : S) (M : Model(A).S) = struct
    type t = A.t list
    type state = M.state
    type fix = M.fix

    let produce (asrt : t) (st : state option) : state option Producer.t =
      Producer.fold_list ~init:st ~f:(Fun.flip M.produce) asrt

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
    let is_consumable (subst : Value.Syn.Subst.t) (atom : A.t) : bool =
      let ins, outs = A.ins_outs atom in
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

    let consume (asrt : t) (st : M.state option) :
        (M.state option, M.fix list) Consumer.t =
      let open Consumer.Syntax in
      let rec aux (remaining : t) (st : M.state option) :
          (M.state option, M.fix list) Consumer.t =
        if List.is_empty remaining then Consumer.ok st
        else
          let* subst = Consumer.expose_subst () in
          match List.find_with_rest (is_consumable subst) remaining with
          | None ->
              failwith
                "No consumable atom found, let's figure out what to do with \
                 this corner case"
          | Some (atom, rest) ->
              let* st' = M.consume atom st in
              aux rest st'
      in
      aux asrt st
  end
end
