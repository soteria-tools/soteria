open Compo_res

module Make (Symex : Symex.S) = struct
  open Symex
  open Symex.Syntax

  type ('a, 'fix) t = 'a * 'fix list

  let expose (st, fixes) = (st, fixes)

  let pp pp_inner pp_fix fmt (st, fixes) =
    Format.fprintf fmt "@[<v 2>STATE: %a;@ FIXES: %a@]" pp_inner st
      (Fmt.Dump.list pp_fix) fixes

  let wrap ?(fuel = 1) ~produce (f : 'a -> ('v * 'a, 'err, 'fix) Symex.Result.t)
      (bi_st : ('a, 'fix) t) :
      ('v * ('a, 'fix) t, 'err * ('a, 'fix) t, 'fix) Result.t =
    let () = if fuel <= 0 then failwith "Bi_abd.wrap: fuel must be positive" in
    let rec with_fuel fuel bi_st =
      let st, fixes = bi_st in
      let* res = f st in
      match res with
      | Ok (v, st) -> Result.ok (v, (st, fixes))
      | Error e -> Result.error (e, bi_st)
      | Missing fix ->
          if fuel <= 0 then Symex.vanish ()
          else
            let* st = produce fix st in
            with_fuel (fuel - 1) (st, fix :: fixes)
    in
    with_fuel fuel bi_st

  let produce prod inner_ser st =
    let st, fixes = st in
    let+ st = prod inner_ser st in
    (st, fixes)

  let consume ?(fuel = 1) ~(produce : 'ser -> 't -> 't Symex.t)
      (cons : 'ser -> 't -> ('t, 'err, 'ser) Symex.Result.t) (inner_ser : 'ser)
      (bi_st : ('t, 'ser) t) :
      (('t, 'ser) t, 'err * ('t, 'ser) t, 'ser) Symex.Result.t =
    let () = if fuel <= 0 then failwith "Bi_abd.wrap: fuel must be positive" in
    let rec with_fuel fuel bi_st =
      let st, fixes = bi_st in
      let* res = cons inner_ser st in
      match res with
      | Ok st -> Result.ok (st, fixes)
      | Error _e ->
          Logs.info (fun m -> m "Bi_abd.consume: vanishing an error");
          Symex.vanish ()
      | Missing fix ->
          if fuel <= 0 then Symex.vanish ()
          else
            let* st = produce fix st in
            with_fuel (fuel - 1) (st, fix :: fixes)
    in
    with_fuel fuel bi_st
end
