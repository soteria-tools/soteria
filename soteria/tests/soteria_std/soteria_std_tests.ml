(* Make sure modules are loaded and tests are registered *)
module _ = List_tests
module _ = Graph_tests
module _ = Iter_tests

let () = Test_register.run_all ()
