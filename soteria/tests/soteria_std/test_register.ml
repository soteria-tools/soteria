open Alcotest

(* An OCaml hashtable can have several bindings for the same key. *)
let test_set : (string, unit test_case) Hashtbl.t = Hashtbl.create 0

let register (suite : string) (test_name : string) (test : unit -> unit) : unit
    =
  Hashtbl.add test_set suite (test_case test_name `Quick test)

let ( let@ ) = ( @@ )

let run_all () =
  let suites =
    Hashtbl.to_seq_keys test_set |> List.of_seq |> List.sort_uniq String.compare
  in
  let tests =
    List.map (fun suite -> (suite, Hashtbl.find_all test_set suite)) suites
  in
  Alcotest.run "Soteria_std" tests
