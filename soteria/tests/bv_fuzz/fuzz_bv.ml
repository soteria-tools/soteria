(** Fuzz test: bitvector smart constructor equivalence. *)

let () =
  Fuzz_common.setup ();
  let suite =
    List.map
      (QCheck_alcotest.to_alcotest ~speed_level:`Slow)
      [ Fuzz_common.mk_test ~name:"bv_smart_eq_direct" Gen.gen_bv ]
  in
  Alcotest.run "fuzz_bv" [ ("bv_smart_constructor_equivalence", suite) ]
