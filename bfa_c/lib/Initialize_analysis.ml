type 'a eff = ('a -> unit) * (('a -> unit) -> unit)

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

let (init, register_initializer) : 'a Cerb_frontend.AilSyntax.sigma eff =
  mk_eff ()

let reinit sigma =
  reset ();
  init sigma
