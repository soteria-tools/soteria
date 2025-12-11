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

module Bi_subst = struct
  type t = {
    forward : Var.t Subst.t;
    backward : Var.t Subst_mut.t;
    mutable next_backward : int;
  }

  let empty () =
    { forward = Subst.empty; backward = Subst_mut.create 0; next_backward = 0 }

  module From_iter (Symex : Symex.Base) : From_iter(Symex).S with type t := t =
  struct
    open Symex.Syntax

    let from_iter iter_vars =
      Symex.fold_iter iter_vars ~init:(empty ()) ~f:(fun bi_subst (var, ty) ->
          if Subst.mem var bi_subst.forward then Symex.return bi_subst
          else
            let+ var' = Symex.fresh_var ty in
            let forward = Subst.add var var' bi_subst.forward in
            Subst_mut.replace bi_subst.backward var' var;
            let next_backward =
              max bi_subst.next_backward (Var.to_int var + 1)
            in
            { forward; backward = bi_subst.backward; next_backward })
  end

  let is_empty bi_subst = Subst.is_empty bi_subst.forward
  let forward bi_subst v_id = Subst.find v_id bi_subst.forward

  let backward bi_subst v_id =
    match Subst_mut.find_opt bi_subst.backward v_id with
    | Some v -> v
    | None ->
        let v = bi_subst.next_backward in
        bi_subst.next_backward <- v + 1;
        let v = Var.of_int v in
        Subst_mut.add bi_subst.backward v_id v;
        v
end
