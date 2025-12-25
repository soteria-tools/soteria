open Soteria_std
module Var = Symex.Var

module From_iter (Symex : Symex.Base) = struct
  module type S = sig
    type t

    val from_iter : 'a Symex.Value.ty Var.iter_vars -> t Symex.t
  end
end

module Subst = struct
  include Map.Make (Var)

  let pp = Fmt.Dump.iter_bindings iter Fmt.nop Var.pp Var.pp

  let substitute_extensible ~f ~subst x =
    let next =
      ref
        (match max_binding_opt subst with
        | None -> 0
        | Some (x, _) -> Var.to_int x)
    in
    let subst = ref subst in
    let subst_var =
      match find_opt x !subst with
      | Some x' -> x'
      | None ->
          let x' = Var.of_int !next in
          incr next;
          subst := add x x' !subst;
          x'
    in
    let res = f subst_var x in
    (res, !subst)

  let to_fn subst x = find x subst

  module From_iter (Symex : Symex.Base) :
    From_iter(Symex).S with type t := Var.t t = struct
    let from_iter iter_vars =
      let open Symex.Syntax in
      Symex.fold_iter iter_vars ~init:empty ~f:(fun subst (var, ty) ->
          if mem var subst then Symex.return subst
          else
            let+ var' = Symex.fresh_var ty in
            add var var' subst)
  end
end

module Subst_mut = struct
  include Hashtbl.Make (Var)

  let add = replace

  let substitute_extensible ~f ~subst x =
    let next =
      ref (to_seq_keys subst |> Seq.map Var.to_int |> Seq.fold_left max 0)
    in
    let subst_var =
      match find_opt subst x with
      | Some x' -> x'
      | None ->
          let x' = Var.of_int !next in
          incr next;
          add subst x x';
          x'
    in
    let res = f subst_var x in
    (res, subst)

  module From_iter (Symex : Symex.Base) :
    From_iter(Symex).S with type t := Var.t t = struct
    let from_iter iter_vars =
      let open Symex.Syntax in
      let subst = create 0 in
      let+ () =
        Symex.fold_iter iter_vars ~init:() ~f:(fun () (var, ty) ->
            if mem subst var then Symex.return ()
            else
              let+ var' = Symex.fresh_var ty in
              add subst var var')
      in
      subst
  end
end
