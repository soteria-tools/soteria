type 'a eff = ('a -> unit) * (('a -> unit) -> unit)

(* The current code (specificaly, the solver implementation) relies
   on the fact that initialisers are performed in the order they were registered. *)

let mk_eff () =
  let eff = ref (fun _ -> ()) in
  let register f =
    let old = !eff in
    eff :=
      fun x ->
        old x;
        f x
  in
  ((fun x -> !eff x), register)

let (reset, register_resetter) : unit eff = mk_eff ()

let (init_before_each, register_before_each_initialiser) :
    'a Cerb_frontend.AilSyntax.sigma eff =
  mk_eff ()

let (init_once, register_once_initialiser) : unit eff = mk_eff ()

let reinit sigma =
  reset ();
  init_before_each sigma
