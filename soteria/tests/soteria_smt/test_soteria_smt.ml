open Soteria.Smt

let sexp = Alcotest.testable pp_sexp ( = )

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  go 0

(* {1 Reader helpers} *)

let with_reader s f =
  let tmp = Filename.temp_file "soteria_smt" ".in" in
  let oc = open_out tmp in
  output_string oc s;
  close_out oc;
  let ic = open_in tmp in
  let err = open_in tmp in
  Fun.protect
    ~finally:(fun () ->
      close_in ic;
      close_in err;
      Sys.remove tmp)
    (fun () -> f (Reader.create ic err))

let parse_one s = with_reader s Reader.read_sexp

let parse_with_err ~stdin ~err =
  let f1 = Filename.temp_file "soteria_smt" ".in" in
  let f2 = Filename.temp_file "soteria_smt" ".err" in
  let w p c =
    let oc = open_out p in
    output_string oc c;
    close_out oc
  in
  w f1 stdin;
  w f2 err;
  let ic = open_in f1 in
  let ec = open_in f2 in
  Fun.protect
    ~finally:(fun () ->
      close_in ic;
      close_in ec;
      Sys.remove f1;
      Sys.remove f2)
    (fun () -> Reader.read_sexp (Reader.create ic ec))

let roundtrips s = parse_one (to_string s) = s
let big_list n = list (List.init n (fun i -> atom (Printf.sprintf "a%d" i)))

(* {1 Fake solver} *)

let fake_cfg exts resp =
  {
    ack_command = (fun _ -> resp);
    command = (fun _ -> ());
    stop = (fun () -> ());
    force_stop = (fun () -> ());
    config = { z3 with exts };
  }

let fake resp = fake_cfg Z3 resp

(* {1 Serializer (original) } *)

let test_serialize () =
  Alcotest.(check string) "atom" "foo" (to_string (atom "foo"));
  Alcotest.(check string) "empty list" "()" (to_string (list []));
  Alcotest.(check string)
    "flat list" "(a b c)"
    (to_string (list [ atom "a"; atom "b"; atom "c" ]));
  Alcotest.(check string)
    "nested" "(= (bvadd x y) #x0f)"
    (to_string (eq (bv_add (atom "x") (atom "y")) (bv_k 8 (Z.of_int 15))));
  Alcotest.(check string)
    "bv binary (non-mult-of-4)" "#b101"
    (to_string (bv_k 3 (Z.of_int 5)));
  Alcotest.(check string) "indexed sort" "(_ BitVec 32)" (to_string (t_bits 32));
  Alcotest.(check string)
    "assert" "(assert (and p q))"
    (to_string (assume (bool_and (atom "p") (atom "q"))))

(* {1 Serializer corner cases} *)

let test_structure_edges () =
  Alcotest.(check string)
    "single-elt list" "(a)"
    (to_string (list [ atom "a" ]));
  Alcotest.(check string)
    "deep nesting" "(((x)))"
    (to_string (list [ list [ list [ atom "x" ] ] ]));
  Alcotest.(check string)
    "app no args is identity" "f"
    (to_string (app (atom "f") []));
  Alcotest.(check string)
    "app_ no args is identity" "f"
    (to_string (app_ "f" []));
  Alcotest.(check string)
    "non-atom head" "((f x) y)"
    (to_string (app (list [ atom "f"; atom "x" ]) [ atom "y" ]));
  Alcotest.(check string)
    "verbatim quoted atom" "(|a b| \"c d\")"
    (to_string (list [ atom "|a b|"; atom "\"c d\"" ]));
  Alcotest.(check bool) "is_atom atom" true (is_atom (atom "x"));
  Alcotest.(check bool) "is_atom list" false (is_atom (list []));
  Alcotest.(check bool)
    "to_list some" true
    (to_list (list [ atom "a" ]) = Some [ atom "a" ]);
  Alcotest.(check bool) "to_list none" true (to_list (atom "x") = None)

let test_numerals () =
  Alcotest.(check string) "zero" "0" (to_string (int_zk Z.zero));
  Alcotest.(check string) "pos" "5" (to_string (int_zk (Z.of_int 5)));
  Alcotest.(check string) "neg" "(- 5)" (to_string (int_zk (Z.of_int (-5))));
  Alcotest.(check string) "int_k neg" "(- 3)" (to_string (int_k (-3)));
  Alcotest.(check string) "int_k pos" "7" (to_string (int_k 7));
  let big = Z.pow (Z.of_int 2) 70 in
  Alcotest.(check string)
    "very large" (Z.to_string big)
    (to_string (int_zk big))

let test_bitvectors () =
  Alcotest.(check string) "hex byte" "#xff" (to_string (bv_k 8 (Z.of_int 255)));
  Alcotest.(check string) "hex zero-pad" "#x0001" (to_string (bv_k 16 Z.one));
  Alcotest.(check string)
    "hex 32 zero" "#x00000000"
    (to_string (bv_k 32 Z.zero));
  Alcotest.(check string) "width-1 binary" "#b1" (to_string (bv_k 1 Z.one));
  Alcotest.(check string)
    "non-mult-of-4" "#b101"
    (to_string (bv_k 3 (Z.of_int 5)));
  Alcotest.(check string)
    "negative hex" "(bvneg #x01)"
    (to_string (bv_k 8 (Z.of_int (-1))));
  Alcotest.(check string)
    "bv_nat_bin pad" "#b00001"
    (to_string (bv_nat_bin 5 Z.one));
  Alcotest.(check string)
    "bv_nat_hex pad" "#x00ff"
    (to_string (bv_nat_hex 16 (Z.of_int 255)));
  Alcotest.(check string)
    "bv_bin negative" "(bvneg #b0001)"
    (to_string (bv_bin 4 (Z.of_int (-1))))

let test_logic_helpers () =
  Alcotest.(check string) "distinct []" "true" (to_string (distinct []));
  Alcotest.(check string)
    "distinct [x]" "(distinct x)"
    (to_string (distinct [ atom "x" ]));
  Alcotest.(check string)
    "distinct xyz" "(distinct x y z)"
    (to_string (distinct [ atom "x"; atom "y"; atom "z" ]));
  Alcotest.(check string) "bool_ands []" "true" (to_string (bool_ands []));
  Alcotest.(check string)
    "bool_ands [p]" "(and p)"
    (to_string (bool_ands [ atom "p" ]));
  Alcotest.(check string) "bool_ors []" "false" (to_string (bool_ors []));
  Alcotest.(check string) "bool_k true" "true" (to_string (bool_k true));
  Alcotest.(check string) "bool_k false" "false" (to_string (bool_k false));
  Alcotest.(check bool) "bool_k true shared" true (bool_k true == s_true);
  Alcotest.(check bool) "bool_k false shared" true (bool_k false == s_false)

let test_commands () =
  Alcotest.(check string) "reset" "(reset)" (to_string reset);
  Alcotest.(check string)
    "set_option" "(set-option :x y)"
    (to_string (set_option ":x" "y"));
  Alcotest.(check string) "push" "(push 1)" (to_string (push 1));
  Alcotest.(check string) "pop" "(pop 2)" (to_string (pop 2));
  Alcotest.(check string)
    "declare const" "(declare-fun v () Int)"
    (to_string (declare "v" t_int));
  Alcotest.(check string)
    "declare_fun" "(declare-fun f (Int Bool) Bool)"
    (to_string (declare_fun "f" [ t_int; t_bool ] t_bool));
  Alcotest.(check string)
    "declare_datatype" "(declare-datatype Ptr ((mk-ptr (loc Int) (ofs Int))))"
    (to_string
       (declare_datatype "Ptr" []
          [ ("mk-ptr", [ ("loc", t_int); ("ofs", t_int) ]) ]));
  Alcotest.(check string) "assume" "(assert p)" (to_string (assume (atom "p")));
  Alcotest.(check string)
    "exists" "(exists (x) b)"
    (to_string (exists [ atom "x" ] (atom "b")))

let test_indexed () =
  Alcotest.(check string)
    "extract" "((_ extract 7 0) x)"
    (to_string (bv_extract 7 0 (atom "x")));
  Alcotest.(check string)
    "sign_extend" "((_ sign_extend 8) x)"
    (to_string (bv_sign_extend 8 (atom "x")));
  Alcotest.(check string)
    "zero_extend" "((_ zero_extend 4) x)"
    (to_string (bv_zero_extend 4 (atom "x")));
  Alcotest.(check string) "t_bits" "(_ BitVec 32)" (to_string (t_bits 32));
  Alcotest.(check string)
    "fam" "(_ foo a b)"
    (to_string (fam "foo" [ atom "a"; atom "b" ]));
  Alcotest.(check string) "ifam" "(_ bar 1 2)" (to_string (ifam "bar" [ 1; 2 ]))

let test_printers () =
  let s = eq (bv_add (atom "x") (atom "y")) (bv_k 8 (Z.of_int 15)) in
  Alcotest.(check string)
    "pp_sexp = to_string" (to_string s)
    (Format.asprintf "%a" pp_sexp s);
  let tmp = Filename.temp_file "soteria_smt" ".out" in
  let oc = open_out tmp in
  output_sexp oc s;
  close_out oc;
  let ic = open_in tmp in
  let got = In_channel.input_all ic in
  close_in ic;
  Sys.remove tmp;
  Alcotest.(check string) "output_sexp = to_string" (to_string s) got

let test_roundtrip () =
  List.iter
    (fun s -> Alcotest.(check sexp) "roundtrip" s (parse_one (to_string s)))
    [
      atom "x";
      list [];
      list [ atom "a" ];
      eq (bv_add (atom "x") (atom "y")) (bv_k 8 (Z.of_int 15));
      declare_datatype "Ptr" []
        [ ("mk-ptr", [ ("loc", t_int); ("ofs", t_int) ]) ];
      list [ list [ list [ atom "deep" ] ]; atom "#b101" ];
    ]

(* {1 Floating point} *)

let is_fp = function List (Atom "fp" :: _) -> true | _ -> false

let test_fp_constants () =
  Alcotest.(check string)
    "f32 1.0" "(fp #b0 #b01111111 #b00000000000000000000000)"
    (to_string (f32_k 1.0));
  Alcotest.(check string)
    "f32 -1.0" "(fp #b1 #b01111111 #b00000000000000000000000)"
    (to_string (f32_k (-1.0)));
  Alcotest.(check string)
    "f64 0.0"
    "(fp #b0 #b00000000000 \
     #b0000000000000000000000000000000000000000000000000000)"
    (to_string (f64_k 0.0));
  Alcotest.(check string)
    "f64 -0.0"
    "(fp #b1 #b00000000000 \
     #b0000000000000000000000000000000000000000000000000000)"
    (to_string (f64_k (-0.0)));
  Alcotest.(check string)
    "f16 wraps f32"
    (Printf.sprintf "((_ to_fp 5 11) RNA %s)" (to_string (f32_k 1.0)))
    (to_string (f16_k 1.0));
  Alcotest.(check string)
    "f128 wraps f64"
    (Printf.sprintf "((_ to_fp 15 113) RNA %s)" (to_string (f64_k 1.0)))
    (to_string (f128_k 1.0));
  Alcotest.(check bool) "f32 nan well-formed" true (is_fp (f32_k Float.nan));
  Alcotest.(check bool)
    "f64 inf well-formed" true
    (is_fp (f64_k Float.infinity))

let test_fp_shape_failure () =
  Alcotest.check_raises "bad float size" (Failure "Unsupported float size: 7")
    (fun () -> ignore (float_shape 7))

let test_rounding_modes () =
  let open RoundingMode in
  Alcotest.(check string) "RNE" "RNE" (to_string (to_sexp NearestTiesToEven));
  Alcotest.(check string) "RNA" "RNA" (to_string (to_sexp NearestTiesToAway));
  Alcotest.(check string) "RTP" "RTP" (to_string (to_sexp Ceil));
  Alcotest.(check string) "RTN" "RTN" (to_string (to_sexp Floor));
  Alcotest.(check string) "RTZ" "RTZ" (to_string (to_sexp Truncate));
  Alcotest.(check string) "default RNA" "RNA" (to_string default)

let test_rounding_deriving () =
  let open RoundingMode in
  Alcotest.(check bool) "equal refl" true (equal Ceil Ceil);
  Alcotest.(check bool) "equal diff" false (equal Ceil Floor);
  Alcotest.(check bool) "compare diff <> 0" true (compare Ceil Floor <> 0);
  Alcotest.(check int) "compare refl" 0 (compare Truncate Truncate);
  Alcotest.(check string) "show no path" "Ceil" (show Ceil);
  Alcotest.(check string) "pp no path" "Floor" (Format.asprintf "%a" pp Floor)

let test_fp_ops () =
  Alcotest.(check string)
    "fp_add carries rm" "(fp.add RNA a b)"
    (to_string (fp_add (atom "a") (atom "b")));
  Alcotest.(check string)
    "fp_rem no rm" "(fp.rem a b)"
    (to_string (fp_rem (atom "a") (atom "b")));
  Alcotest.(check string) "fp_abs" "(fp.abs f)" (to_string (fp_abs (atom "f")));
  Alcotest.(check string)
    "fp_eq" "(fp.eq a b)"
    (to_string (fp_eq (atom "a") (atom "b")));
  Alcotest.(check string)
    "fp_leq" "(fp.leq a b)"
    (to_string (fp_leq (atom "a") (atom "b")));
  Alcotest.(check string)
    "fp_lt" "(fp.lt a b)"
    (to_string (fp_lt (atom "a") (atom "b")));
  Alcotest.(check string)
    "fp_round RTZ" "(fp.roundToIntegral RTZ x)"
    (to_string (fp_round RoundingMode.Truncate (atom "x")))

let test_fp_is_class () =
  Alcotest.(check string)
    "normal" "(fp.isNormal x)"
    (to_string (fp_is FP_normal (atom "x")));
  Alcotest.(check string)
    "subnormal" "(fp.isSubnormal x)"
    (to_string (fp_is FP_subnormal (atom "x")));
  Alcotest.(check string)
    "zero" "(fp.isZero x)"
    (to_string (fp_is FP_zero (atom "x")));
  Alcotest.(check string)
    "infinite" "(fp.isInfinite x)"
    (to_string (fp_is FP_infinite (atom "x")));
  Alcotest.(check string)
    "nan" "(fp.isNaN x)"
    (to_string (fp_is FP_nan (atom "x")))

let test_fp_conversions () =
  Alcotest.(check string)
    "float_of_bv" "((_ to_fp 8 24) x)"
    (to_string (float_of_bv 32 (atom "x")));
  Alcotest.(check string)
    "float_of_ubv" "((_ to_fp_unsigned 5 11) RNE x)"
    (to_string (float_of_ubv RoundingMode.NearestTiesToEven 16 (atom "x")));
  Alcotest.(check string)
    "float_of_sbv" "((_ to_fp 11 53) RTN x)"
    (to_string (float_of_sbv RoundingMode.Floor 64 (atom "x")));
  Alcotest.(check string)
    "ubv_of_float" "((_ fp.to_ubv 8) RTP f)"
    (to_string (ubv_of_float RoundingMode.Ceil 8 (atom "f")));
  Alcotest.(check string)
    "sbv_of_float" "((_ fp.to_sbv 16) RTZ f)"
    (to_string (sbv_of_float RoundingMode.Truncate 16 (atom "f")));
  Alcotest.(check string)
    "bv_of_int" "((_ int_to_bv 32) n)"
    (to_string (bv_of_int 32 (atom "n")));
  Alcotest.(check string)
    "int_of_bv signed" "(sbv_to_int x)"
    (to_string (int_of_bv true (atom "x")));
  Alcotest.(check string)
    "int_of_bv unsigned" "(ubv_to_int x)"
    (to_string (int_of_bv false (atom "x")))

let test_bv_overflow () =
  Alcotest.(check string) "nego" "(bvnego x)" (to_string (bv_nego (atom "x")));
  Alcotest.(check string)
    "uaddo" "(bvuaddo l r)"
    (to_string (bv_uaddo (atom "l") (atom "r")));
  Alcotest.(check string)
    "saddo" "(bvsaddo l r)"
    (to_string (bv_saddo (atom "l") (atom "r")));
  Alcotest.(check string)
    "usubo" "(bvusubo l r)"
    (to_string (bv_usubo (atom "l") (atom "r")));
  Alcotest.(check string)
    "ssubo" "(bvssubo l r)"
    (to_string (bv_ssubo (atom "l") (atom "r")));
  Alcotest.(check string)
    "umulo" "(bvumulo l r)"
    (to_string (bv_umulo (atom "l") (atom "r")));
  Alcotest.(check string)
    "smulo" "(bvsmulo l r)"
    (to_string (bv_smulo (atom "l") (atom "r")))

(* {1 Reader (original) } *)

let test_reader () =
  Alcotest.check sexp "atom" (atom "success") (parse_one "success\n");
  Alcotest.check sexp "sat" (atom "sat") (parse_one "  sat ");
  Alcotest.check sexp "bv hex" (atom "#xff") (parse_one "#xff");
  Alcotest.check sexp "bv bin" (atom "#b1010") (parse_one "#b1010");
  Alcotest.check sexp "nested list"
    (list [ atom "a"; list [ atom "b"; atom "c" ] ])
    (parse_one "(a (b c))");
  Alcotest.check sexp "empty list" (list []) (parse_one "()");
  Alcotest.check sexp "quoted symbol" (atom "|a b|") (parse_one "|a b|");
  Alcotest.check sexp "string literal" (atom "\"hi\"") (parse_one "\"hi\"");
  Alcotest.check sexp "comment skipped" (atom "x")
    (parse_one "; a comment\n  x");
  Alcotest.check sexp "model-ish"
    (list
       [ list [ atom "define-fun"; atom "x"; list []; atom "Int"; atom "3" ] ])
    (parse_one "((define-fun x () Int 3))")

let test_reader_interns () =
  Alcotest.(check bool)
    "success interned" true
    (parse_one "success" == Reader.a_success);
  Alcotest.(check bool) "sat interned" true (parse_one "sat" == Reader.a_sat)

(* {1 Reader corner cases} *)

let test_reader_whitespace_comments () =
  Alcotest.check sexp "tabs/crlf" (atom "foo") (parse_one "\t\r\n  foo \r\n");
  Alcotest.check sexp "comment between elements"
    (list [ atom "a"; atom "b" ])
    (parse_one "(a ; comment here\n b)");
  Alcotest.check sexp "trailing comment no newline" (atom "x")
    (parse_one "x ; trailing no newline");
  Alcotest.check sexp "full-line comment before list"
    (list [ atom "a" ])
    (parse_one "; hello\n(a)");
  Alcotest.check sexp "extra spaces in list"
    (list [ atom "a"; atom "b" ])
    (parse_one "(   a    b   )")

let test_reader_adjacency_quoting () =
  Alcotest.check sexp "tight list" (list [ atom "a" ]) (parse_one "(a)");
  Alcotest.check sexp "tight bv" (list [ atom "#xff" ]) (parse_one "(#xff)");
  Alcotest.check sexp "triple nest"
    (list [ list [ list [] ] ])
    (parse_one "((()))");
  Alcotest.check sexp "quoted keeps parens/spaces" (atom "|a (b) c|")
    (parse_one "|a (b) c|");
  Alcotest.check sexp "string escaped quote" (atom "\"a\"\"b\"")
    (parse_one "\"a\"\"b\"");
  Alcotest.check sexp "quoted adjacent to paren"
    (list [ atom "|x|"; atom "y" ])
    (parse_one "(|x| y)")

let test_reader_sequential_interning () =
  let a, b =
    with_reader "sat unsat" (fun r ->
        let a = Reader.read_sexp r in
        let b = Reader.read_sexp r in
        (a, b))
  in
  Alcotest.(check bool) "1st is interned sat" true (a == Reader.a_sat);
  Alcotest.(check bool) "2nd is interned unsat" true (b == Reader.a_unsat);
  Alcotest.(check bool)
    "unknown interned" true
    (parse_one "unknown" == Reader.a_unknown);
  let l1, l2 =
    with_reader "(a) (b)" (fun r ->
        let a = Reader.read_sexp r in
        let b = Reader.read_sexp r in
        (a, b))
  in
  Alcotest.check sexp "seq list 1" (list [ atom "a" ]) l1;
  Alcotest.check sexp "seq list 2" (list [ atom "b" ]) l2;
  let x, y =
    with_reader "a(b)" (fun r ->
        let a = Reader.read_sexp r in
        let b = Reader.read_sexp r in
        (a, b))
  in
  Alcotest.check sexp "adj atom" (atom "a") x;
  Alcotest.check sexp "adj list" (list [ atom "b" ]) y

let test_reader_refill_boundary () =
  let l = big_list 30000 in
  let s = to_string l in
  Alcotest.(check bool) "exceeds 64KiB buffer" true (String.length s > 65536);
  (match parse_one s with
  | List xs -> Alcotest.(check int) "list length" 30000 (List.length xs)
  | Atom _ -> Alcotest.fail "expected a list");
  Alcotest.(check bool) "big list roundtrips" true (roundtrips l);
  let big_atom = atom (String.make 100000 'a') in
  Alcotest.(check bool) "big atom roundtrips" true (roundtrips big_atom)

let test_reader_eof_fallback () =
  Alcotest.check sexp "eof -> stderr atom" (atom "boom")
    (parse_with_err ~stdin:"   " ~err:"boom");
  Alcotest.check sexp "eof empty stderr" (atom "")
    (parse_with_err ~stdin:"" ~err:"")

let test_reader_error_response () =
  Alcotest.check sexp "(error ...)"
    (list [ atom "error"; atom "\"line 1\"" ])
    (parse_one "(error \"line 1\")")

(* {1 Protocol (original)} *)

let test_ack () =
  ack_command (fake (atom "success")) (atom "anything");
  Alcotest.check_raises "non-success raises"
    (UnexpectedSolverResponse (atom "boom"))
    (fun () -> ack_command (fake (atom "boom")) (atom "x"))

let test_check () =
  Alcotest.(check string) "sat" "Sat" (show_result (check (fake (atom "sat"))));
  Alcotest.(check string)
    "unsat" "Unsat"
    (show_result (check (fake (atom "unsat"))));
  Alcotest.(check string)
    "unknown" "Unknown"
    (show_result (check (fake (atom "unknown"))));
  Alcotest.check_raises "garbage raises"
    (UnexpectedSolverResponse (atom "weird"))
    (fun () -> ignore (check (fake (atom "weird"))))

(* {1 Protocol corner cases} *)

let test_protocol_list_responses () =
  let err = list [ atom "error"; atom "\"oops\"" ] in
  Alcotest.check_raises "ack list resp raises" (UnexpectedSolverResponse err)
    (fun () -> ack_command (fake err) (atom "x"));
  Alcotest.check_raises "check list resp raises" (UnexpectedSolverResponse err)
    (fun () -> ignore (check (fake err)))

let test_protocol_printers () =
  let e = UnexpectedSolverResponse (eq (atom "a") (atom "b")) in
  let s = Printexc.to_string e in
  Alcotest.(check bool)
    "exn name" true
    (contains ~needle:"UnexpectedSolverResponse" s);
  Alcotest.(check bool) "exn payload" true (contains ~needle:"(= a b)" s);
  Alcotest.(check string) "show Sat" "Sat" (show_result Sat);
  Alcotest.(check string) "show Unsat" "Unsat" (show_result Unsat);
  Alcotest.(check string) "show Unknown" "Unknown" (show_result Unknown);
  Alcotest.(check string)
    "pp_result" "Unsat"
    (Format.asprintf "%a" pp_result Unsat)

(* {1 get_model workarounds} *)

let dfun name args ret def =
  list [ atom "define-fun"; atom name; list (List.map atom args); ret; def ]

let test_model_passthrough () =
  let m = list [ dfun "a" [] t_int (int_k 1) ] in
  Alcotest.check sexp "Other passthrough" m (get_model (fake_cfg Other m));
  Alcotest.check sexp "CVC5 passthrough" m (get_model (fake_cfg CVC5 m));
  Alcotest.check sexp "Other atom passthrough" (atom "weird")
    (get_model (fake_cfg Other (atom "weird")))

let test_model_z3_reorder () =
  let def_b = dfun "b" [] t_int (atom "a") in
  let def_a = dfun "a" [] t_int (int_k 1) in
  Alcotest.check sexp "dependency-ordered"
    (list [ def_a; def_b ])
    (get_model (fake_cfg Z3 (list [ def_b; def_a ])))

let test_model_z3_drop_as_array () =
  let body = list [ atom "_"; atom "as-array"; atom "k" ] in
  let def_f = dfun "f" [] (list [ atom "Array"; t_int; t_int ]) body in
  let expected = dfun "f" [] (list [ atom "Array"; t_int; t_int ]) (atom "k") in
  Alcotest.check sexp "as-array stripped" (list [ expected ])
    (get_model (fake_cfg Z3 (list [ def_f ])))

let test_model_z3_error_cases () =
  Alcotest.check_raises "atom response raises"
    (UnexpectedSolverResponse (atom "model"))
    (fun () -> ignore (get_model (fake_cfg Z3 (atom "model"))));
  let cyclic =
    list [ dfun "a" [] t_int (atom "b"); dfun "b" [] t_int (atom "a") ]
  in
  Alcotest.check_raises "cyclic raises" (UnexpectedSolverResponse cyclic)
    (fun () -> ignore (get_model (fake_cfg Z3 cyclic)));
  let malformed = list [ atom "oops" ] in
  Alcotest.check_raises "malformed def raises"
    (UnexpectedSolverResponse malformed) (fun () ->
      ignore (get_model (fake_cfg Z3 malformed)))

(* {1 Live z3 (skipped if absent)} *)

let z3_available () = Sys.command "command -v z3 >/dev/null 2>&1" = 0
let skip_unless_z3 () = if not (z3_available ()) then Alcotest.skip ()

let test_z3_smoke () =
  skip_unless_z3 ();
  let s = new_solver z3 in
  ack_command s (declare "x" (t_bits 8));
  ack_command s (assume (bv_ult (atom "x") (bv_k 8 (Z.of_int 5))));
  Alcotest.(check string) "sat" "Sat" (show_result (check s));
  let m = get_model s in
  Alcotest.(check bool)
    "model is a list" true
    (match m with List _ -> true | Atom _ -> false);
  ack_command s (assume (bv_ult (bv_k 8 (Z.of_int 5)) (atom "x")));
  Alcotest.(check string) "unsat" "Unsat" (show_result (check s));
  s.stop ()

let test_z3_push_pop () =
  skip_unless_z3 ();
  let s = new_solver z3 in
  ack_command s (declare "x" t_int);
  ack_command s (assume (num_lt (int_k 10) (atom "x")));
  Alcotest.(check string) "x>10 sat" "Sat" (show_result (check s));
  ack_command s (push 1);
  ack_command s (assume (num_lt (atom "x") (int_k 5)));
  Alcotest.(check string) "x>10 & x<5 unsat" "Unsat" (show_result (check s));
  ack_command s (pop 1);
  Alcotest.(check string) "after pop sat" "Sat" (show_result (check s));
  s.stop ()

let test_z3_float () =
  skip_unless_z3 ();
  let s = new_solver z3 in
  ack_command s (declare "v" t_f32);
  ack_command s (assume (fp_eq (atom "v") (f32_k 1.0)));
  Alcotest.(check string) "v=1.0 sat" "Sat" (show_result (check s));
  ack_command s (push 1);
  ack_command s (assume (fp_eq (atom "v") (f32_k 2.0)));
  Alcotest.(check string) "v=1 & v=2 unsat" "Unsat" (show_result (check s));
  ack_command s (pop 1);
  s.stop ()

let test_z3_datatype_and_overflow () =
  skip_unless_z3 ();
  let s = new_solver z3 in
  ack_command s
    (declare_datatype "Ptr" []
       [ ("mk-ptr", [ ("loc", t_int); ("ofs", t_int) ]) ]);
  ack_command s (declare "p" (atom "Ptr"));
  ack_command s (assume (eq (app_ "loc" [ atom "p" ]) (int_k 3)));
  ack_command s (assume (eq (app_ "ofs" [ atom "p" ]) (int_k 4)));
  Alcotest.(check string) "datatype sat" "Sat" (show_result (check s));
  ack_command s (declare "a" (t_bits 8));
  ack_command s (declare "b" (t_bits 8));
  ack_command s (assume (bv_uaddo (atom "a") (atom "b")));
  ack_command s (assume (eq (atom "a") (bv_k 8 (Z.of_int 200))));
  Alcotest.(check string) "overflow possible sat" "Sat" (show_result (check s));
  ack_command s (push 1);
  ack_command s (assume (eq (atom "b") (bv_k 8 Z.zero)));
  Alcotest.(check string)
    "no overflow + overflow asserted unsat" "Unsat"
    (show_result (check s));
  ack_command s (pop 1);
  s.stop ()

let () =
  Alcotest.run "soteria_smt"
    [
      ( "serialize",
        [
          Alcotest.test_case "builders" `Quick test_serialize;
          Alcotest.test_case "structure-edges" `Quick test_structure_edges;
          Alcotest.test_case "numerals" `Quick test_numerals;
          Alcotest.test_case "bitvectors" `Quick test_bitvectors;
          Alcotest.test_case "logic-helpers" `Quick test_logic_helpers;
          Alcotest.test_case "commands" `Quick test_commands;
          Alcotest.test_case "indexed" `Quick test_indexed;
          Alcotest.test_case "printers" `Quick test_printers;
          Alcotest.test_case "roundtrip" `Quick test_roundtrip;
        ] );
      ( "fp",
        [
          Alcotest.test_case "constants" `Quick test_fp_constants;
          Alcotest.test_case "shape-failure" `Quick test_fp_shape_failure;
          Alcotest.test_case "rounding-modes" `Quick test_rounding_modes;
          Alcotest.test_case "rounding-deriving" `Quick test_rounding_deriving;
          Alcotest.test_case "ops" `Quick test_fp_ops;
          Alcotest.test_case "is-class" `Quick test_fp_is_class;
          Alcotest.test_case "conversions" `Quick test_fp_conversions;
          Alcotest.test_case "bv-overflow" `Quick test_bv_overflow;
        ] );
      ( "reader",
        [
          Alcotest.test_case "round-trip" `Quick test_reader;
          Alcotest.test_case "interning" `Quick test_reader_interns;
          Alcotest.test_case "whitespace-comments" `Quick
            test_reader_whitespace_comments;
          Alcotest.test_case "adjacency-quoting" `Quick
            test_reader_adjacency_quoting;
          Alcotest.test_case "sequential-interning" `Quick
            test_reader_sequential_interning;
          Alcotest.test_case "refill-boundary" `Quick
            test_reader_refill_boundary;
          Alcotest.test_case "eof-fallback" `Quick test_reader_eof_fallback;
          Alcotest.test_case "error-response" `Quick test_reader_error_response;
        ] );
      ( "protocol",
        [
          Alcotest.test_case "ack_command" `Quick test_ack;
          Alcotest.test_case "check" `Quick test_check;
          Alcotest.test_case "list-responses" `Quick
            test_protocol_list_responses;
          Alcotest.test_case "exn-and-result-printers" `Quick
            test_protocol_printers;
        ] );
      ( "model",
        [
          Alcotest.test_case "passthrough" `Quick test_model_passthrough;
          Alcotest.test_case "z3-reorder" `Quick test_model_z3_reorder;
          Alcotest.test_case "z3-drop-as-array" `Quick
            test_model_z3_drop_as_array;
          Alcotest.test_case "z3-error-cases" `Quick test_model_z3_error_cases;
        ] );
      ( "z3",
        [
          Alcotest.test_case "smoke" `Slow test_z3_smoke;
          Alcotest.test_case "push-pop" `Slow test_z3_push_pop;
          Alcotest.test_case "float" `Slow test_z3_float;
          Alcotest.test_case "datatype-and-overflow" `Slow
            test_z3_datatype_and_overflow;
        ] );
    ]
