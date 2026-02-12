(** Fuzz test: boolean smart constructor equivalence. *)

let test_bool_equivalence =
  Fuzz_common.mk_test ~count:20000 ~name:"bool_smart_eq_direct"
    Gen.gen_bool_pair

let () =
  Fuzz_common.setup ();
  let suite = List.map QCheck_alcotest.to_alcotest [ test_bool_equivalence ] in
  Alcotest.run "fuzz_bool" [ ("bool_smart_constructor_equivalence", suite) ]
