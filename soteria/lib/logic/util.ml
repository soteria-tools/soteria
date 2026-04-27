module M (Symex : Symex.Base) = struct
  (** Utilities to handle common types with Soteria's logic modules. *)

  module Abstr = Data.Abstr.M (Symex)

  module Option = struct
    (** Option type lifted to the value world. Implements
        {{!Soteria.Abstr.M.S_with_syn}[S_with_syn]}. *)

    type 'a t = 'a option [@@deriving show { with_path = false }]
    type 'a syn = 'a option [@@deriving show { with_path = false }]

    let fresh fresh () =
      let open Symex in
      branches
        [ (fun () -> return None); (fun () -> map Option.some (fresh ())) ]

    let to_syn to_syn = Option.map to_syn
    let subst subst s = Option.map (subst s)

    let learn_eq learn_eq syn opt =
      match (syn, opt) with
      | None, None -> Symex.Consumer.ok ()
      | Some syn, Some x -> learn_eq syn x
      | Some _, None | None, Some _ ->
          Symex.Consumer.lfail (Symex.Value.of_bool false)

    let exprs_syn exprs_syn = Option.fold ~none:[] ~some:exprs_syn
  end
end
