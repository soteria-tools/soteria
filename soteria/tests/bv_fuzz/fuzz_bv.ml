(** Fuzz test: bitvector smart constructor equivalence. *)

let test_bv_equivalence =
  Fuzz_common.mk_test ~count:10000 ~name:"bv_smart_eq_direct" Gen.gen_bv_pair

let () =
  Fuzz_common.setup ();
  let suite = List.map QCheck_alcotest.to_alcotest [ test_bv_equivalence ] in
  Alcotest.run "fuzz_bv" [ ("bv_smart_constructor_equivalence", suite) ]
