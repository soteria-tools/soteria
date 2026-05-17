(** A small, performance-focused SMT-LIB s-expression and solver-process
    library, specialised to Soteria's needs.

    This is an in-tree replacement for the (unpublished) [simple_smt] library.
    The public surface mirrors the subset of [Simple_smt] that Soteria uses, so
    that consumers only need to swap [open Simple_smt] for [open Soteria_smt].

    Performance notes:
    - Serialization writes directly into a reused {!Buffer.t} (no
      pretty-printing, no whole-tree intermediate string).
    - Solver responses are read with a hand-written buffered reader and a
      recursive-descent parser specialised to the small response grammar,
      rather than a generic s-expression lexer.
    - The handful of hot response tokens ([success], [sat], [unsat],
      [unknown]) are interned, so the [check] / [ack_command] round-trips do
      not allocate a fresh atom. *)

include Fast_smt

(** {1 SMT term and command builders} *)

(* Apply a function to some arguments. *)
let app f args = match args with [] -> f | _ -> List (f :: args)
let app_ f (args : sexp list) : sexp = app (Atom f) args

(* Type annotation *)
let as_type x t = app_ "as" [ x; t ]

let nat_k x = Atom (string_of_int x)
let nat_zk x = Atom (Z.to_string x)

(* Indexed family *)
let fam f is = List (Atom "_" :: Atom f :: is)
let ifam f is = fam f (List.map nat_k is)

(** {2 Booleans} *)

let t_bool = Atom "Bool"
let s_true = Atom "true"
let s_false = Atom "false"
let bool_k b = if b then s_true else s_false

let a_ite = Atom "ite"
let a_eq = Atom "="
let a_distinct = Atom "distinct"
let a_not = Atom "not"
let a_and = Atom "and"
let a_or = Atom "or"

let ite x y z = List [ a_ite; x; y; z ]
let eq x y = List [ a_eq; x; y ]
let distinct xs = match xs with [] -> s_true | _ -> List (a_distinct :: xs)
let bool_not p = List [ a_not; p ]
let bool_and p q = List [ a_and; p; q ]
let bool_ands ps = match ps with [] -> s_true | _ -> List (a_and :: ps)
let bool_or p q = List [ a_or; p; q ]
let bool_ors ps = match ps with [] -> s_false | _ -> List (a_or :: ps)

(** {2 Integers} *)

let t_int = Atom "Int"
let a_neg = Atom "-"
let num_neg x = List [ a_neg; x ]
let int_k x = if x < 0 then num_neg (nat_k (-x)) else nat_k x
let int_zk x = if Z.lt x Z.zero then num_neg (nat_zk (Z.neg x)) else nat_zk x

let a_lt = Atom "<"
let a_leq = Atom "<="
let a_add = Atom "+"
let a_sub = Atom "-"
let a_mul = Atom "*"
let a_div = Atom "div"
let a_mod = Atom "mod"
let a_rem = Atom "rem"

let num_lt x y = List [ a_lt; x; y ]
let num_leq x y = List [ a_leq; x; y ]
let num_add x y = List [ a_add; x; y ]
let num_sub x y = List [ a_sub; x; y ]
let num_mul x y = List [ a_mul; x; y ]
let num_div x y = List [ a_div; x; y ]
let num_mod x y = List [ a_mod; x; y ]
let num_rem x y = List [ a_rem; x; y ]

(** {2 Bit-vectors} *)

(* [t_bits w] is the type of bit vectors of width [w]. *)
let t_bits w = ifam "BitVec" [ w ]

(* A non-negative bit-vector literal in binary. *)
let bv_nat_bin w v = Atom ("#b" ^ Z.format ("0" ^ string_of_int w ^ "b") v)

(* A non-negative bit-vector literal in hex (width must be a multiple of 4). *)
let bv_nat_hex w v = Atom ("#x" ^ Z.format ("0" ^ string_of_int (w / 4) ^ "x") v)

let a_bvneg = Atom "bvneg"
let bv_neg x = List [ a_bvneg; x ]

let bv_bin w v =
  if Z.geq v Z.zero then bv_nat_bin w v else bv_neg (bv_nat_bin w (Z.neg v))

let bv_hex w v =
  if Z.geq v Z.zero then bv_nat_hex w v else bv_neg (bv_nat_hex w (Z.neg v))

(* Bit-vector literal, hex if width is a multiple of 4, binary otherwise. *)
let bv_k w v = if w mod 4 = 0 then bv_hex w v else bv_bin w v

let a_bvult = Atom "bvult"
let a_bvule = Atom "bvule"
let a_bvslt = Atom "bvslt"
let a_bvsle = Atom "bvsle"
let a_concat = Atom "concat"
let a_bvnot = Atom "bvnot"
let a_bvand = Atom "bvand"
let a_bvor = Atom "bvor"
let a_bvxor = Atom "bvxor"
let a_bvadd = Atom "bvadd"
let a_bvsub = Atom "bvsub"
let a_bvmul = Atom "bvmul"
let a_bvudiv = Atom "bvudiv"
let a_bvurem = Atom "bvurem"
let a_bvsdiv = Atom "bvsdiv"
let a_bvsrem = Atom "bvsrem"
let a_bvsmod = Atom "bvsmod"
let a_bvshl = Atom "bvshl"
let a_bvlshr = Atom "bvlshr"
let a_bvashr = Atom "bvashr"

let bv_ult x y = List [ a_bvult; x; y ]
let bv_uleq x y = List [ a_bvule; x; y ]
let bv_slt x y = List [ a_bvslt; x; y ]
let bv_sleq x y = List [ a_bvsle; x; y ]
let bv_concat x y = List [ a_concat; x; y ]
let bv_sign_extend i x = app (ifam "sign_extend" [ i ]) [ x ]
let bv_zero_extend i x = app (ifam "zero_extend" [ i ]) [ x ]
let bv_extract last_ix first_ix x = app (ifam "extract" [ last_ix; first_ix ]) [ x ]
let bv_not x = List [ a_bvnot; x ]
let bv_and x y = List [ a_bvand; x; y ]
let bv_or x y = List [ a_bvor; x; y ]
let bv_xor x y = List [ a_bvxor; x; y ]
let bv_add x y = List [ a_bvadd; x; y ]
let bv_sub x y = List [ a_bvsub; x; y ]
let bv_mul x y = List [ a_bvmul; x; y ]
let bv_udiv x y = List [ a_bvudiv; x; y ]
let bv_urem x y = List [ a_bvurem; x; y ]
let bv_sdiv x y = List [ a_bvsdiv; x; y ]
let bv_srem x y = List [ a_bvsrem; x; y ]
let bv_smod x y = List [ a_bvsmod; x; y ]
let bv_shl x y = List [ a_bvshl; x; y ]
let bv_lshr x y = List [ a_bvlshr; x; y ]
let bv_ashr x y = List [ a_bvashr; x; y ]

(** {2 Commands} *)

let simple_command xs = List (List.map atom xs)
let set_option x y = simple_command [ "set-option"; x; y ]
let push n = simple_command [ "push"; string_of_int n ]
let pop n = simple_command [ "pop"; string_of_int n ]
let declare_fun f ps r = app_ "declare-fun" [ Atom f; List ps; r ]
let declare f t = declare_fun f [] t

type con_field = string * sexp

let declare_datatype name type_params cons =
  let mk_field ((f, arg_ty) : con_field) = List [ Atom f; arg_ty ] in
  let mk_con (c, fs) = List (Atom c :: List.map mk_field fs) in
  let mk_cons = List (List.map mk_con cons) in
  let def =
    match type_params with
    | [] -> mk_cons
    | _ -> app_ "par" [ List (List.map atom type_params); mk_cons ]
  in
  app_ "declare-datatype" [ Atom name; def ]

let assume e = app_ "assert" [ e ]

(** {1 Solver} *)

type solver_extensions = Z3 | CVC5 | Other

type solver_log = {
  send : (unit -> string) -> unit;  (** We sent this to the solver. *)
  receive : (unit -> string) -> unit;  (** We got this from the solver. *)
  stop : unit -> unit;  (** Cleanup when done. *)
}

type solver_config = {
  exe : string;
  opts : string list;
  exts : solver_extensions;
  log : solver_log;
}

type solver = {
  command : sexp -> sexp;
  stop : unit -> unit;
  force_stop : unit -> unit;
  config : solver_config;
}

(** {2 Acknowledged commands and checking} *)

let ack_command (s : solver) cmd =
  match s.command cmd with
  | Atom "success" -> ()
  | ans -> raise (UnexpectedSolverResponse ans)

type result = Unsat | Unknown | Sat

let pp_result fmt = function
  | Unsat -> Format.pp_print_string fmt "Unsat"
  | Unknown -> Format.pp_print_string fmt "Unknown"
  | Sat -> Format.pp_print_string fmt "Sat"

let show_result = function
  | Unsat -> "Unsat"
  | Unknown -> "Unknown"
  | Sat -> "Sat"

let s_check_sat = List [ Atom "check-sat" ]

let check s =
  match s.command s_check_sat with
  | Atom "unsat" -> Unsat
  | Atom "sat" -> Sat
  | Atom "unknown" -> Unknown
  | ans -> raise (UnexpectedSolverResponse ans)

(** {2 Models} *)

let s_get_model = List [ Atom "get-model" ]

let get_model s =
  let ans = s.command s_get_model in
  match s.config.exts with
  | CVC5 | Other -> ans
  | Z3 ->
      (* Workaround z3 bug #7270: remove `as-array`. *)
      let rec drop_as_array x =
        match x with
        | List [ _; Atom "as-array"; f ] -> f
        | List xs -> List (List.map drop_as_array xs)
        | _ -> x
      in
      (* Workaround z3 bug #7268: emit defs in dependency order. *)
      let add_binder bound x =
        match x with
        | List [ Atom v; _ ] -> StrSet.add v bound
        | _ -> raise (UnexpectedSolverResponse x)
      in
      let add_bound = List.fold_left add_binder in
      let rec free bound vars x =
        match x with
        | Atom a -> if StrSet.mem a bound then vars else a :: vars
        | List [ Atom q; List vs; body ]
          when String.equal q "forall" || String.equal q "exist"
               || String.equal q "let" ->
            free (add_bound bound vs) vars body
        | List xs -> List.fold_left (free bound) vars xs
      in
      let check_def x =
        match x with
        | List [ _def_fun; Atom name; List args; _ret; def ] ->
            let bound = add_bound StrSet.empty args in
            (name, free bound [] def, x)
        | _ -> raise (UnexpectedSolverResponse ans)
      in
      (match ans with
      | Atom _ -> raise (UnexpectedSolverResponse ans)
      | List xs ->
          let defs = List.map check_def xs in
          let add_dep mp (x, xs, e) = StrMap.add x (xs, e) mp in
          let deps = List.fold_left add_dep StrMap.empty defs in
          let processing = ref StrSet.empty in
          let processed = ref StrSet.empty in
          let decls = ref [] in
          let rec arrange todo =
            match todo with
            | x :: xs ->
                if StrSet.mem x !processed then arrange xs
                else if StrSet.mem x !processing then
                  raise (UnexpectedSolverResponse ans) (* recursive *)
                else (
                  match StrMap.find_opt x deps with
                  | None -> arrange xs
                  | Some (ds, e) ->
                      processing := StrSet.add x !processing;
                      arrange ds;
                      processing := StrSet.remove x !processing;
                      processed := StrSet.add x !processed;
                      decls := drop_as_array e :: !decls;
                      arrange xs)
            | [] -> ()
          in
          arrange (List.map (fun (x, _, _) -> x) defs);
          List (List.rev !decls))

(** {2 Creating solvers} *)

let new_solver (cfg : solver_config) : solver =
  let args = Array.of_list (cfg.exe :: cfg.opts) in
  let proc = Unix.open_process_args_full cfg.exe args [||] in
  let pid = Unix.process_full_pid proc in
  let in_chan, out_chan, in_err_chan = proc in
  let reader = Reader.create in_chan in_err_chan in
  let cmd_buf = Buffer.create 4096 in

  let send_command c =
    Buffer.clear cmd_buf;
    write_buf cmd_buf c;
    Buffer.output_buffer out_chan cmd_buf;
    output_char out_chan '\n';
    flush out_chan;
    cfg.log.send (fun () -> Buffer.contents cmd_buf);
    let ans = Reader.read_sexp reader in
    cfg.log.receive (fun () -> to_string ans);
    ans
  in
  let stop_command () =
    (try
       output_string out_chan "(exit)\n";
       flush out_chan
     with Sys_error _ -> ());
    let _ = Unix.close_process_full proc in
    cfg.log.stop ()
  in
  let force_stop_command () =
    (try Unix.kill pid 9 with Unix.Unix_error _ -> ());
    let _ = Unix.close_process_full proc in
    cfg.log.stop ()
  in
  let s =
    {
      command = send_command;
      stop = stop_command;
      force_stop = force_stop_command;
      config = cfg;
    }
  in
  ack_command s (set_option ":print-success" "true");
  ack_command s (set_option ":produce-models" "true");
  Gc.finalise (fun me -> me.stop ()) s;
  s

(** {2 Solver configurations} *)

let quiet_log =
  { send = (fun _ -> ()); receive = (fun _ -> ()); stop = (fun () -> ()) }

let printf_log =
  {
    send = (fun f -> Printf.printf "[->] %s\n%!" (f ()));
    receive = (fun f -> Printf.printf "[<-] %s\n%!" (f ()));
    stop = (fun () -> ());
  }

let cvc5 : solver_config =
  {
    exe = "cvc5";
    opts = [ "--incremental"; "--sets-ext" ];
    exts = CVC5;
    log = quiet_log;
  }

let z3 : solver_config =
  { exe = "z3"; opts = [ "-in"; "-smt2" ]; exts = Z3; log = quiet_log }

(** {1 SMT-LIB utilities}

    Utilities for SMT-LIB's
    {{:https://smt-lib.org/theories-FloatingPoint.shtml} FloatingPoint theory}
    and
    {{:https://smt-lib.org/theories-FixedSizeBitVecs.shtml} FixedSizeBitVecs
     theory}, as well as some extra solver commands. *)

(** {2 Boolean Stuff} *)

let exists qs body = app_ "exists" [ list qs; body ]

(** {2 Rounding Modes} *)

module RoundingMode = struct
  type t =
    | NearestTiesToEven  (** Round to nearest, ties to even (RNE) *)
    | NearestTiesToAway  (** Round to nearest, ties away from zero (RNA) *)
    | Ceil  (** Round toward positive infinity (RTP) *)
    | Floor  (** Round toward negative infinity (RTN) *)
    | Truncate  (** Round toward zero (RTZ) *)
  [@@deriving eq, show { with_path = false }, ord]

  let to_sexp = function
    | NearestTiesToEven -> atom "RNE"
    | NearestTiesToAway -> atom "RNA"
    | Ceil -> atom "RTP"
    | Floor -> atom "RTN"
    | Truncate -> atom "RTZ"

  (** Equivalent to NearestTiesToAway; default mode for FloatingPoint
      operations. *)
  let default = to_sexp NearestTiesToAway
end

(** {2 FloatingPoint} *)

(** [float_shape n] is the shape of a IEEE float of a given size in bits.
    Returns a two element list [[exp, mant]], where [exp] is the number of
    exponent bits, and [mant] is the number of mantissa/significand bits. [mant]
    {b includes the hidden bit} which is always [1], as per SMT-lib's
    expectations. Always holds that [n = mant + exp]. Only implemented for
    [n = 16, 32, 64, 128]. *)
let float_shape = function
  | 16 -> [ 5; 11 ]
  | 32 -> [ 8; 24 ]
  | 64 -> [ 11; 53 ]
  | 128 -> [ 15; 113 ]
  | n -> failwith (Printf.sprintf "Unsupported float size: %d" n)

(** SMT-LIB Float16 type. *)
let t_f16 = atom "Float16"

(** SMT-LIB Float32 type. *)
let t_f32 = atom "Float32"

(** SMT-LIB Float64 type. *)
let t_f64 = atom "Float64"

(** SMT-LIB Float128 type. *)
let t_f128 = atom "Float128"

(** [f32_k f] creates a Float32 constant from an OCaml float [f].

    Directly encodes the IEEE 754 binary32 representation. This may incur some
    loss of precision. *)
let f32_k f =
  let bin = Int32.bits_of_float f in
  (* a Float32 has 8 exponent bits, 23 explicit mantissa bits *)
  app_ "fp"
    [
      bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
      bv_nat_bin 8
        (Z.of_int32 @@ Int32.logand 0xffl @@ Int32.shift_right_logical bin 23);
      bv_nat_bin 23 (Z.of_int32 @@ Int32.logand bin 0x7fffffl);
    ]

(** [f64_k f] creates a Float64 constant from an OCaml float [f].

    Directly encodes the IEEE 754 binary64 representation, so no precision is
    lost. *)
let f64_k f =
  let bin = Int64.bits_of_float f in
  (* a Float64 has 11 exponent bits, 52 mantissa bits, with a 53rd implicit 1 *)
  app_ "fp"
    [
      bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
      bv_nat_bin 11
        (Z.of_int64 @@ Int64.logand 0x7ffL @@ Int64.shift_right_logical bin 52);
      bv_nat_bin 52 (Z.of_int64 @@ Int64.logand bin 0xfffffffffffffL);
    ]

(** [f128_k f] creates a Float128 constant from OCaml float [f].

    The value is first converted into a Float64, and we then use the [to_fp]
    SMT-LIB function to convert it to a Float128, using [RoundingMode.default].
    Necessarily implies a loss of precision. *)
let f128_k f =
  let f64 = f64_k f in
  let fam = ifam "to_fp" (float_shape 128) in
  app fam [ RoundingMode.default; f64 ]

(** [f16_k f] creates a Float16 constant from OCaml float [f].

    The value is first converted into a Float32, and we then use the [to_fp]
    SMT-LIB function to convert it to a Float16, using [RoundingMode.default].
    Necessarily implies a loss of precision. *)
let f16_k f =
  let f32 = f32_k f in
  let fam = ifam "to_fp" (float_shape 16) in
  app fam [ RoundingMode.default; f32 ]

(** [fp_abs f] returns the absolute value of [f]. *)
let fp_abs f = app_ "fp.abs" [ f ]

(** [fp_eq f1 f2] returns true if [f1] equals [f2] (IEEE 754 equality). *)
let fp_eq f1 f2 = app_ "fp.eq" [ f1; f2 ]

(** [fp_leq f1 f2] returns true if [f1 <= f2]. *)
let fp_leq f1 f2 = app_ "fp.leq" [ f1; f2 ]

(** [fp_lt f1 f2] returns true if [f1 < f2]. *)
let fp_lt f1 f2 = app_ "fp.lt" [ f1; f2 ]

(** [fp_add f1 f2] returns [f1 + f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_add f1 f2 = app_ "fp.add" [ RoundingMode.default; f1; f2 ]

(** [fp_sub f1 f2] returns [f1 - f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_sub f1 f2 = app_ "fp.sub" [ RoundingMode.default; f1; f2 ]

(** [fp_mul f1 f2] returns [f1 * f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_mul f1 f2 = app_ "fp.mul" [ RoundingMode.default; f1; f2 ]

(** [fp_div f1 f2] returns [f1 / f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_div f1 f2 = app_ "fp.div" [ RoundingMode.default; f1; f2 ]

(** [fp_rem f1 f2] returns [f1 % f2] (no rounding mode is involved here). *)
let fp_rem f1 f2 = app_ "fp.rem" [ f1; f2 ]

(** [fp_is fc f] tests if [f] belongs to floating-point class [fc] (which is of
    OCaml's builtin [fpclass] type).*)
let fp_is (fc : fpclass) f =
  match fc with
  | FP_normal -> app_ "fp.isNormal" [ f ]
  | FP_subnormal -> app_ "fp.isSubnormal" [ f ]
  | FP_zero -> app_ "fp.isZero" [ f ]
  | FP_infinite -> app_ "fp.isInfinite" [ f ]
  | FP_nan -> app_ "fp.isNaN" [ f ]

(** [fp_round rm f] rounds [f] to an integer using rounding mode [rm]. *)
let fp_round (rm : RoundingMode.t) f =
  app_ "fp.roundToIntegral" [ RoundingMode.to_sexp rm; f ]

(** {2 FloatingPoint - BitVec conversions} *)

(** [float_of_bv size bv] interprets bitvector [bv] as a float of [size] bits.
    [size] must be one of 16, 32, 64 or 128.

    This is a bitwise reinterpretation, not a numeric conversion. *)
let float_of_bv size bv = app (ifam "to_fp" (float_shape size)) [ bv ]

(** [float_of_ubv rm size bv] converts unsigned bitvector [bv] to a float, with
    rounding mode [rm]. [size] must be one of 16, 32, 64 or 128. *)
let float_of_ubv rm size bv =
  app (ifam "to_fp_unsigned" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

(** [float_of_sbv rm size bv] converts signed bitvector [bv] to a float, with
    rounding mode [rm]. [size] must be one of 16, 32, 64 or 128. *)
let float_of_sbv rm size bv =
  app (ifam "to_fp" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

(** [ubv_of_float rm size f] converts float [f] to an unsigned bitvector of
    [size] bits, with rounding mode [rm]. This is a numeric conversion, not a
    bitwise reinterpretation. If [f] is out of range for the target bitvector,
    or NaN or inf, the result is undefined. *)
let ubv_of_float rm n f =
  app (ifam "fp.to_ubv" [ n ]) [ RoundingMode.to_sexp rm; f ]

(** [sbv_of_float rm size f] converts float [f] to a signed bitvector of [size]
    bits, with rounding mode [rm]. This is a numeric conversion, not a bitwise
    reinterpretation. If [f] is out of range for the target bitvector, or NaN or
    inf, the result is undefined. *)
let sbv_of_float rm n f =
  app (ifam "fp.to_sbv" [ n ]) [ RoundingMode.to_sexp rm; f ]

(** {2 Int - BitVec conversions} *)

(** [int_of_bv signed bv] converts bitvector [bv] to an integer. If [signed] is
    true, [bv] is interpreted as a signed bitvector; otherwise, it is
    interpreted as unsigned. *)
let int_of_bv signed bv =
  if signed then app_ "sbv_to_int" [ bv ] else app_ "ubv_to_int" [ bv ]

(** [bv_of_int size n] converts integer [n] to a bitvector of [size] bits. *)
let bv_of_int size n = app (ifam "int_to_bv" [ size ]) [ n ]

(** {2 BitVec overflow predicates} *)

(** [bv_nego x] is true if negating [x] would overflow (signed). *)
let bv_nego x = app_ "bvnego" [ x ]

(** [bv_uaddo l r] returns true if [l + r] would overflow (unsigned). *)
let bv_uaddo l r = app_ "bvuaddo" [ l; r ]

(** [bv_saddo l r] returns true if [l + r] would overflow (signed). *)
let bv_saddo l r = app_ "bvsaddo" [ l; r ]

(** [bv_usubo l r] returns true if [l - r] would underflow (unsigned). *)
let bv_usubo l r = app_ "bvusubo" [ l; r ]

(** [bv_ssubo l r] returns true if [l - r] would underflow or overflow (signed).
*)
let bv_ssubo l r = app_ "bvssubo" [ l; r ]

(** [bv_umulo l r] returns true if [l * r] would overflow (unsigned). *)
let bv_umulo l r = app_ "bvumulo" [ l; r ]

(** [bv_smulo l r] returns true if [l * r] would overflow (signed). *)
let bv_smulo l r = app_ "bvsmulo" [ l; r ]

(** {2 Commands} *)

(** SMT-LIB [(reset)] command to reset the solver state. *)
let reset = simple_command [ "reset" ]
