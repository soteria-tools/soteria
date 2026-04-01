open Symex.Compo_res

let pp_bi_state pp_st pp_fixes fmt (st, fixes) =
  Format.fprintf fmt "@[<v 2>STATE: %a;@ FIXES: %a@]" (Fmt.Dump.option pp_st) st
    (Fmt.Dump.list pp_fixes) fixes

module Make (Symex : Symex.Base) (B : Base.M(Symex).S) = struct
  (** This is unsound in {!Symex.Approx.OX}-mode, use only in
      {!Symex.Approx.UX}-mode. *)

  type t = B.t option * B.syn list
  type syn = B.syn [@@deriving show { with_path = false }]

  let to_syn _ = failwith "Bi_abd.to_syn: not implemented"
  let ins_outs syn = B.ins_outs syn

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

  let to_spec topt : pre:syn list * post:syn list =
    let post, pre = of_opt topt in
    let post = Option.fold ~none:[] ~some:B.to_syn post in
    (~pre, ~post)

  let to_opt : t -> t option = function None, [] -> None | other -> Some other
  let expose (st, fixes) = (st, fixes)

  let pp' ?(inner = B.pp) ?(syn = B.pp_syn) fmt (st, fixes) =
    Format.fprintf fmt "@[<v 2>STATE: %a;@ FIXES: %a@]" (Fmt.Dump.option inner)
      st (Fmt.Dump.list syn) fixes

  let pp fmt t = pp' fmt t
  let show = Fmt.to_to_string pp

  let wrap ?(fuel = 1) (f : ('v, 'err, B.syn list) B.SM.Result.t) :
      ('v, 'err, syn list) SM.Result.t =
    let () = if fuel <= 0 then failwith "Bi_abd.wrap: fuel must be positive" in
    let rec with_fuel fuel : ('v, 'err, syn list) SM.Result.t =
      let* bi_st = SM.get_state () in
      let st, fixes = of_opt bi_st in
      let*^ res, st' = f st in
      match res with
      | Ok _ | Error _ ->
          let+ () = SM.set_state (to_opt (st', fixes)) in
          res
      | Missing fix_choices ->
          if fuel <= 0 then SM.vanish ()
          else
            SM.branches
              (List.map
                 (fun fix ->
                   fun () ->
                    let*^ (), st'' =
                      B.SM.iter_list fix
                        ~f:(fun fix st ->
                          let open Symex.Syntax in
                          let+ st' =
                            Symex.Producer.run_identity (B.produce fix st)
                          in
                          ((), st'))
                        st
                    in
                    let* () = SM.set_state (to_opt (st'', fix @ fixes)) in
                    with_fuel (fuel - 1))
                 fix_choices)
    in
    with_fuel fuel

  let wrap_no_fail (f : 'a B.SM.t) : 'a SM.t =
   fun (bi_st : t option) ->
    let open Symex.Syntax in
    let st, fixes = of_opt bi_st in
    let+ v, st = f st in
    (v, to_opt (st, fixes))

  let produce syn st : t option Symex.Producer.t =
    let open Symex.Producer.Syntax in
    let st, fixes = of_opt st in
    let+ st = B.produce syn st in
    to_opt (st, fixes)

  let consume (syn : syn) (bi_st : t option) :
      (t option, syn list) Symex.Consumer.t =
    let open Symex.Consumer in
    let open Syntax in
    let max_fuel = 1 in
    let rec with_fuel fuel (bi_st : SM.st) : (SM.st, syn list) Symex.Consumer.t
        =
      let st, fixes = of_opt bi_st in
      let*! res = B.consume syn st in
      match res with
      | Ok st' -> ok (to_opt (st', fixes))
      | Error _ -> lift_symex (Symex.vanish ())
      | Missing fix_choices ->
          if fuel <= 0 then lift_symex (Symex.vanish ())
          else
            branches
              (List.map
                 (fun fix () ->
                   let*^ st' =
                     let open Symex.Syntax in
                     let* st =
                       Symex.fold_list fix ~init:st ~f:(fun st syn ->
                           Symex.Producer.run_identity (B.produce syn st))
                     in
                     Symex.return st
                   in
                   with_fuel (fuel - 1) (to_opt (st', fix @ fixes)))
                 fix_choices)
    in
    with_fuel max_fuel bi_st
end
