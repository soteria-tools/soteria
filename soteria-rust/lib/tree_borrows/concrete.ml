(** Concrete implementation of tree borrows, for testing and debugging. Does not
    support compositionality, just lifts {!Raw} to fit the state model shape. *)

open Soteria.Symex.Compo_res

module Make (Symex : Tree_borrows_intf.Rust_symex) :
  Tree_borrows_intf.M(Symex).S = struct
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

  let nondet_tag () = return None
  let init () = return (init ())
  let init_st _ = return (Some empty_state)
  let unwrap x = Option.get ~msg:"missing state in concrete TB" x

  let borrow ?protector parent ~state st =
    let st = unwrap st in
    let st', tag = borrow ?protector parent ~state st in
    return (Ok tag, Some st')

  let unprotect tag st = return (Ok (), Some (unwrap st |> unprotect tag))

  let access accessed e root st =
    match access accessed e (unwrap root) (unwrap st) with
    | Ok st' -> Symex.Result.ok (Some st')
    | Error e -> Symex.Result.error e

  let set_protector ~protected tag t st =
    Result.ok (Some (set_protector ~protected tag (unwrap t) (unwrap st)))

  let strong_protector_exists st = strong_protector_exists (unwrap st)
  let merge l r = return (Some (merge (unwrap l) (unwrap r)))
  let equal_state = Option.equal equal_state

  (* Compositionality *)

  type serialized = | [@@deriving show]

  let serialize _ = []
  let subst_serialized _ : serialized -> serialized = function _ -> .
  let iter_vars_serialized _ _ = ()
  let consume _ st = Result.ok st
  let produce _ st = return ((), st)

  type serialized_state = | [@@deriving show]
  type full_serialized = Structure of serialized | State of serialized_state

  let serialize_state _ = []
  let fix_empty_state () = []

  let subst_serialized_state _ : serialized_state -> serialized_state = function
    | _ -> .

  let iter_vars_serialized_state _ _ = ()
  let consume_state _ st = Result.ok st
  let produce_state _ st = return st
end
