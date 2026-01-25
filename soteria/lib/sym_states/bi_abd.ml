open Symex.Compo_res

module Make (Symex : Symex.Base) (B : Base.M(Symex).S) = struct
  (** This is unsound in {!Approx.OX}-mode, use only in {!Approx.UX}-mode. *)

  type t = B.t option * B.serialized list
  type serialized = B.serialized

  module BSM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = B.t option
      end)

  module SM =
    State_monad.Make
      (Symex)
      (struct
        type nonrec t = t option
      end)

  open SM.Syntax

  let of_opt : t option -> t = function
    | None -> (None, [])
    | Some (st, v) -> (st, v)

  let to_opt : t -> t option = function None, [] -> None | other -> Some other
  let expose (st, fixes) = (st, fixes)

  let pp fmt ((st, fixes) : t) =
    Format.fprintf fmt "@[<v 2>STATE: %a;@ FIXES: %a@]" (Fmt.Dump.option B.pp)
      st
      (Fmt.Dump.list B.pp_serialized)
      fixes

  let show = Fmt.to_to_string pp

  let wrap ?(fuel = 1) (f : ('v, 'err, B.serialized list) BSM.Result.t) :
      ('v, 'err, serialized list) SM.Result.t =
    let () = if fuel <= 0 then failwith "Bi_abd.wrap: fuel must be positive" in
    let rec with_fuel fuel : ('v, 'err, serialized list) SM.Result.t =
      let* bi_st = SM.get_state () in
      let st, fixes = of_opt bi_st in
      let* res, st' = SM.lift @@ f st in
      match res with
      | Ok v ->
          let* () = SM.set_state (to_opt (st', fixes)) in
          SM.Result.ok v
      | Error e ->
          let* () = SM.set_state (to_opt (st', fixes)) in
          SM.Result.error e
      | Missing fix_choices ->
          if fuel <= 0 then SM.vanish ()
          else
            SM.branches
              (List.map
                 (fun fix ->
                   fun () ->
                    let* (), st'' =
                      let fn =
                        BSM.fold_list fix ~init:() ~f:(fun () fix ->
                            B.produce fix)
                      in
                      SM.lift @@ fn st
                    in
                    let* () = SM.set_state (to_opt (st'', fix @ fixes)) in
                    with_fuel (fuel - 1))
                 fix_choices)
    in
    with_fuel fuel

  let wrap_no_fail (f : 'a BSM.t) : 'a SM.t =
   fun (bi_st : t option) ->
    let open Symex.Syntax in
    let st, fixes = of_opt bi_st in
    let+ v, st = f st in
    (v, to_opt (st, fixes))

  let produce inner_ser : unit SM.t =
    let* st = SM.get_state () in
    let st, fixes = of_opt st in
    let* (), st = SM.lift @@ B.produce inner_ser st in
    SM.set_state (to_opt (st, fixes))

  (* let consume ?(fuel = 1) ~(produce : 'ser -> 't -> 't Symex.t)
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
          L.info (fun m -> m "Bi_abd.consume: vanishing an error");
          Symex.vanish ()
      | Missing fix_choices ->
          if fuel <= 0 then Symex.vanish ()
          else
            Symex.branches
              (List.map
                 (fun fix ->
                   fun () ->
                    let* st = produce fix st in
                    with_fuel (fuel - 1) (st, fix :: fixes))
                 fix_choices)
    in
    with_fuel fuel bi_st *)
end
