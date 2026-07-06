(** Concrete implementation of tree borrows, for testing and debugging. Does not
    support compositionality, just lifts {!Raw} to fit the state model shape. *)

open Compo_res

module Make (Symex : Tree_borrows_intf.Rust_symex) :
  Tree_borrows_intf.M(Symex).S = struct
  open Symex
  open Raw

  let[@inline] unwrap x = Option.get ~msg:"missing state in concrete TB" x

  module Tree = struct
    type nonrec t = t

    let pp = pp
    let show = Fmt.to_to_string pp

    module SM =
      Soteria.Sym_states.State_monad.Make
        (Symex)
        (struct
          type nonrec t = t option
        end)

    let init () = return (init ())

    type syn = | [@@deriving show]

    let to_syn _ = []
    let ins_outs (s : syn) = match s with _ -> .
    let consume (s : syn) _ = match s with _ -> .
    let produce (s : syn) _ = match s with _ -> .
    let assert_exclusively_owned () = SM.Result.ok ()

    let borrow ?protector parent ~state st =
      let st = unwrap st in
      let tag, st' = borrow ?protector parent ~state st in
      return (Ok tag, Some st')

    let unprotect tag st = return (Ok (), Some (unwrap st |> unprotect tag))
    let strong_protector_exists st = strong_protector_exists (unwrap st)
  end

  module State = struct
    type nonrec t = tb_state

    module SM =
      Soteria.Sym_states.State_monad.Make
        (Symex)
        (struct
          type nonrec t = t option
        end)

    let pp = pp_tb_state
    let show = Fmt.to_to_string pp
    let init _ = return empty_state
    let merge l r = return (merge l r)
    let equal = Option.equal equal_state

    type syn = | [@@deriving show]

    let to_syn _ = []
    let ins_outs (s : syn) = match s with _ -> .
    let consume (s : syn) _ = match s with _ -> .
    let produce (s : syn) _ = match s with _ -> .
    let fix_empty () = []

    let[@inline] access accessed e root st =
      match access accessed e (unwrap root) (unwrap st) with
      | Ok st' -> Symex.Result.ok (Some st')
      | Error e -> Symex.Result.error e

    let set_protector ~protected tag t st =
      Result.ok (Some (set_protector ~protected tag (unwrap t) (unwrap st)))

    let assert_exclusively_owned _ = Result.ok ()
  end
end
