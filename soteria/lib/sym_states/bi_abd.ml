open Logs.Import
open Symex.Compo_res

module Make (Symex : Symex.Base) = struct
  (** This is unsound in {!Approx.OX}-mode, use only in {!Approx.UX}-mode. *)

  open Symex
  open Symex.Syntax

  type ('a, 'fix) t = 'a * 'fix list

  let expose (st, fixes) = (st, fixes)

  let pp pp_inner pp_fix fmt (st, fixes) =
    Format.fprintf fmt "@[<v 2>STATE: %a;@ FIXES: %a@]" pp_inner st
      (Fmt.Dump.list pp_fix) fixes

  let wrap ?(fuel = 1) ~(produce : 'syn -> 'a -> 'a Symex.Producer.t)
      (f : 'a -> ('v * 'a, 'err, 'syn list) Symex.Result.t)
      (bi_st : ('a, 'syn) t) :
      ('v * ('a, 'syn) t, 'err * ('a, 'syn) t, 'syn list) Result.t =
    let () = if fuel <= 0 then failwith "Bi_abd.wrap: fuel must be positive" in
    let rec with_fuel fuel bi_st =
      let st, fixes = bi_st in
      let* res = f st in
      match res with
      | Ok (v, st) -> Result.ok (v, (st, fixes))
      | Error e -> Result.error (e, bi_st)
      | Missing fix_choices ->
          if fuel <= 0 then Symex.vanish ()
          else
            Symex.branches
              (List.map
                 (fun fix ->
                   fun () ->
                    let* st =
                      Symex.Producer.run_identity_producer
                        (Producer.fold_list ~init:st
                           ~f:(fun st s -> produce s st)
                           fix)
                    in
                    with_fuel (fuel - 1) (st, fix @ fixes))
                 fix_choices)
    in
    with_fuel fuel bi_st

  let wrap_error (f : 'a -> ('ok, 'err, 'fix) Symex.Result.t)
      (bi_st : ('a, 'fix) t) : ('ok, 'err * ('a, 'fix) t, 'fix) Result.t =
    let st, _ = bi_st in
    let* res = f st in
    match res with
    | Error e -> Result.error (e, bi_st)
    | _ -> failwith "Bi_abd.wrap_error: expected error result"

  let wrap_no_fail f bi_st =
    let st, fixes = bi_st in
    let+ v, st = f st in
    (v, (st, fixes))

  let produce prod inner_ser st =
    let open Symex.Producer.Syntax in
    let st, fixes = st in
    let+ st = prod inner_ser st in
    (st, fixes)

  let consume ?(fuel = 1) ~(produce : 'syn -> 'a -> 'a Symex.Producer.t)
      (cons : 'syn -> 't -> ('t, 'syn list) Symex.Consumer.t) (inner_ser : 'syn)
      (bi_st : ('t, 'syn) t) : (('t, 'syn) t, 'syn list) Symex.Consumer.t =
    let open Symex.Consumer.Syntax in
    let () = if fuel <= 0 then failwith "Bi_abd.wrap: fuel must be positive" in
    let rec with_fuel fuel bi_st =
      let st, fixes = bi_st in
      let*! res = cons inner_ser st in
      match res with
      | Ok st -> Consumer.ok (st, fixes)
      | Error _e ->
          L.info (fun m -> m "Bi_abd.consume: vanishing an error");
          Consumer.lfail (Value.bool false)
      | Missing fix_choices ->
          if fuel <= 0 then Consumer.lift_symex (Symex.vanish ())
          else
            Consumer.branches
              (List.map
                 (fun fix ->
                   fun () ->
                    let* st =
                      Consumer.lift_symex
                        (Symex.Producer.run_identity_producer
                           (Producer.fold_list ~init:st
                              ~f:(fun st s -> produce s st)
                              fix))
                    in
                    with_fuel (fuel - 1) (st, fix @ fixes))
                 fix_choices)
    in
    with_fuel fuel bi_st
end
