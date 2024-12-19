open Compo_res

module Make (Symex : Symex.S) = struct
  open Symex
  open Symex.Syntax

  type ('a, 'fix) t = 'a * 'fix list

  let pp pp_inner pp_fix fmt (st, fixes) =
    Format.fprintf fmt "@[<v 2>STATE: %a;@ FIXES: %a@]" pp_inner st
      (Fmt.Dump.list pp_fix) fixes

  let wrap ~produce (f : 'a -> ('v * 'a, 'err, 'fix) Symex.Result.t)
      (st : ('a, 'fix) t) : ('v * ('a, 'fix) t, 'err, 'fix) Result.t =
    let st, fixes = st in
    let* res = f st in
    match res with
    | Ok (v, st) -> Result.ok (v, (st, fixes))
    | Missing fix ->
        let* st = produce fix st in
        let++ v, st = f st in
        (v, (st, fix :: fixes))
    | Error e -> Result.error e

  let produce prod inner_ser st =
    let st, fixes = st in
    let+ st = prod inner_ser st in
    (st, fixes)

  let consume ~produce cons inner_ser st =
    let st, fixes = st in
    let* res = cons inner_ser st in
    match res with
    | Ok st -> Result.ok (st, fixes)
    | Missing fix ->
        let* st = produce fix st in
        let++ st = cons inner_ser st in
        (st, fix :: fixes)
    | Error e -> Result.error e
end
