(** Fuzz test: bitvector smart constructor equivalence. *)

open Soteria.Bv_values

let print_bv_params (depth, bv_size, seed) =
  Printf.sprintf "depth=%d, bv_size=%d, seed=%d" depth bv_size seed

let test_bv_equivalence =
  QCheck.Test.make ~count:10000 ~name:"bv_smart_eq_direct"
    QCheck.(make ~print:print_bv_params Fuzz_common.gen_params)
    (fun (depth, bv_size, seed) ->
      let rs = Random.State.make [| seed |] in
      let smart, direct = Gen.gen_bv_pair ~depth ~bv_size rs in
      if Svalue.equal smart direct then true
      else
        let check = Fuzz_common.check_equivalence smart direct in
        if not check then
          Format.eprintf "COUNTEREXAMPLE:@.%a@." Fuzz_common.pp_pair
            (smart, direct);
        check)

let () =
  Fuzz_common.setup ();
  let suite = List.map QCheck_alcotest.to_alcotest [ test_bv_equivalence ] in
  Alcotest.run "fuzz_bv" [ ("bv_smart_constructor_equivalence", suite) ]
