(** Fuzz test: boolean smart constructor equivalence. *)

let test_bool_equivalence count =
  Fuzz_common.mk_test ~count ~name:"bool_smart_eq_direct" Gen.gen_bool_pair

let () =
  Fuzz_common.setup ();
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ test_bool_equivalence (Lazy.force Fuzz_common.test_count) ]
  in
  Alcotest.run "fuzz_bool" [ ("bool_smart_constructor_equivalence", suite) ]
