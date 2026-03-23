open Soteria.Symex.Compo_res

module Make (Symex : Soteria.Symex.Base) : Tree_borrows_intf.M(Symex).S = struct
  open Symex
  include Raw

  module SM =
    Soteria.Sym_states.State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  module SM_St =
    Soteria.Sym_states.State_monad.Make
      (Symex)
      (struct
        type nonrec t = tb_state option
      end)

  (* Lift operations symbolically *)

  let unwrap = Option.get ~msg:"missing state in concrete TB"

  let add_child ~parent ?protector ~state st =
    let st = unwrap st in
    let st', tag = add_child ~parent ?protector ~state st in
    return (Ok tag, Some st')

  let unprotect tag st = return (Ok (), Some (unwrap st |> unprotect tag))

  let access accessed e root st =
    match access accessed e (unwrap root) st with
    | Ok st' -> Symex.Result.ok st'
    | Error e -> Symex.Result.error e

  let set_protector ~protected tag st t =
    Result.ok (set_protector ~protected tag (unwrap st) t)

  let strong_protector_exists st = strong_protector_exists (unwrap st)

  (* Compositionality *)

  type serialized = | [@@deriving show]

  let serialize _ = []
  let subst_serialized _ : serialized -> serialized = function _ -> .
  let iter_vars_serialized _ _ = ()
  let consume _ st = Result.ok st
  let produce _ st = return ((), st)

  type serialized_state = | [@@deriving show]

  let serialize_state _ = []

  let subst_serialized_state _ : serialized_state -> serialized_state = function
    | _ -> .

  let iter_vars_serialized_state _ _ = ()
  let consume_state _ st = Result.ok st
  let produce_state _ st = return st
end
