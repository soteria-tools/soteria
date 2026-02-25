(** Fuzz test: boolean smart constructor equivalence. *)

let () =
  Fuzz_common.setup ();
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ Fuzz_common.mk_test ~name:"bool_smart_eq_direct" Gen.gen_bool ]
  in
  Alcotest.run "fuzz_bool" [ ("bool_smart_constructor_equivalence", suite) ]
