(** A small, performance-focused SMT-LIB s-expression and solver-process
    library, specialised to Soteria's needs.

    Performance notes:
    - Serialization writes directly into a reused {!Buffer.t} (no
      pretty-printing, no whole-tree intermediate string).
    - Solver responses are read with a hand-written buffered reader and a
      recursive-descent parser specialised to the small response grammar, rather
      than a generic s-expression lexer.
    - The handful of hot response tokens ([success], [sat], [unsat], [unknown])
      are interned, so the [check] / [ack_command] round-trips do not allocate a
      fresh atom. *)

include Fast_sexp
module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

(** {1 SMT term and command builders} *)

(* Apply a function to some arguments. *)
let app f args = match args with [] -> f | _ -> List (f :: args)
let app_ f (args : sexp list) : sexp = app (Atom f) args

(* Application as infix operators: [f $$ args] (multiple) and [f $ arg]. *)
let ( $$ ) = app
let ( $$. ) = app_
let ( $ ) f v = f $$ [ v ]

(* Type annotation *)
let as_type x t = "as" $$. [ x; t ]
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
let ite x y z = a_ite $$ [ x; y; z ]
let eq x y = a_eq $$ [ x; y ]
let distinct xs = match xs with [] -> s_true | _ -> a_distinct $$ xs
let bool_not p = a_not $ p
let bool_and p q = a_and $$ [ p; q ]
let bool_ands ps = match ps with [] -> s_true | _ -> a_and $$ ps
let bool_or p q = a_or $$ [ p; q ]
let bool_ors ps = match ps with [] -> s_false | _ -> a_or $$ ps
let exists qs body = "exists" $$. [ list qs; body ]

(** {2 Integers} *)

let t_int = Atom "Int"
let a_neg = Atom "-"
let num_neg x = a_neg $ x
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
let num_lt x y = a_lt $$ [ x; y ]
let num_leq x y = a_leq $$ [ x; y ]
let num_add x y = a_add $$ [ x; y ]
let num_sub x y = a_sub $$ [ x; y ]
let num_mul x y = a_mul $$ [ x; y ]
let num_div x y = a_div $$ [ x; y ]
let num_mod x y = a_mod $$ [ x; y ]
let num_rem x y = a_rem $$ [ x; y ]

(** {2 Bit-vectors} *)

(* [t_bits w] is the type of bit vectors of width [w]. *)
let t_bits w = ifam "BitVec" [ w ]

(* A non-negative bit-vector literal in binary. *)
let bv_nat_bin w v = Atom ("#b" ^ Z.format ("0" ^ string_of_int w ^ "b") v)

(* A non-negative bit-vector literal in hex (width must be a multiple of 4). *)
let bv_nat_hex w v = Atom ("#x" ^ Z.format ("0" ^ string_of_int (w / 4) ^ "x") v)
let a_bvneg = Atom "bvneg"
let bv_neg x = a_bvneg $ x

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
let bv_ult x y = a_bvult $$ [ x; y ]
let bv_uleq x y = a_bvule $$ [ x; y ]
let bv_slt x y = a_bvslt $$ [ x; y ]
let bv_sleq x y = a_bvsle $$ [ x; y ]
let bv_concat x y = a_concat $$ [ x; y ]
let bv_sign_extend i x = ifam "sign_extend" [ i ] $ x
let bv_zero_extend i x = ifam "zero_extend" [ i ] $ x
let bv_extract last_ix first_ix x = ifam "extract" [ last_ix; first_ix ] $ x
let bv_not x = a_bvnot $ x
let bv_and x y = a_bvand $$ [ x; y ]
let bv_or x y = a_bvor $$ [ x; y ]
let bv_xor x y = a_bvxor $$ [ x; y ]
let bv_add x y = a_bvadd $$ [ x; y ]
let bv_sub x y = a_bvsub $$ [ x; y ]
let bv_mul x y = a_bvmul $$ [ x; y ]
let bv_udiv x y = a_bvudiv $$ [ x; y ]
let bv_urem x y = a_bvurem $$ [ x; y ]
let bv_sdiv x y = a_bvsdiv $$ [ x; y ]
let bv_srem x y = a_bvsrem $$ [ x; y ]
let bv_smod x y = a_bvsmod $$ [ x; y ]
let bv_shl x y = a_bvshl $$ [ x; y ]
let bv_lshr x y = a_bvlshr $$ [ x; y ]
let bv_ashr x y = a_bvashr $$ [ x; y ]

(** {2 Rounding modes} *)

module RoundingMode = struct
  type t =
    | NearestTiesToEven  (** ties to even (RNE) *)
    | NearestTiesToAway  (** ties away from zero (RNA) *)
    | Ceil  (** toward +inf (RTP) *)
    | Floor  (** toward -inf (RTN) *)
    | Truncate  (** toward zero (RTZ) *)
  [@@deriving eq, show { with_path = false }, ord]

  let to_sexp = function
    | NearestTiesToEven -> atom "RNE"
    | NearestTiesToAway -> atom "RNA"
    | Ceil -> atom "RTP"
    | Floor -> atom "RTN"
    | Truncate -> atom "RTZ"

  (* Equivalent to NearestTiesToAway; default for FloatingPoint operations. *)
  let default = to_sexp NearestTiesToAway
end

(** {2 Floating-point} *)

(* [float_shape n] is [[exp; mant]] for an IEEE float of [n] bits, where [mant]
   includes the hidden bit (so [n = exp + mant]). Sizes 16/32/64/128 only. *)
let float_shape = function
  | 16 -> [ 5; 11 ]
  | 32 -> [ 8; 24 ]
  | 64 -> [ 11; 53 ]
  | 128 -> [ 15; 113 ]
  | n -> failwith (Printf.sprintf "Unsupported float size: %d" n)

let t_f16 = atom "Float16"
let t_f32 = atom "Float32"
let t_f64 = atom "Float64"
let t_f128 = atom "Float128"

(* Float32 constant from an OCaml float (8 exponent, 23 explicit mantissa bits);
   may lose precision. *)
let f32_k f =
  let bin = Int32.bits_of_float f in
  "fp"
  $$.
  [
    bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
    bv_nat_bin 8
      (Z.of_int32 @@ Int32.logand 0xffl @@ Int32.shift_right_logical bin 23);
    bv_nat_bin 23 (Z.of_int32 @@ Int32.logand bin 0x7fffffl);
  ]

(* Float64 constant from an OCaml float (11 exponent, 52 mantissa bits);
   lossless. *)
let f64_k f =
  let bin = Int64.bits_of_float f in
  "fp"
  $$.
  [
    bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
    bv_nat_bin 11
      (Z.of_int64 @@ Int64.logand 0x7ffL @@ Int64.shift_right_logical bin 52);
    bv_nat_bin 52 (Z.of_int64 @@ Int64.logand bin 0xfffffffffffffL);
  ]

(* Float128 constant, via Float64 then [to_fp]; lossy. *)
let f128_k f =
  app (ifam "to_fp" (float_shape 128)) [ RoundingMode.default; f64_k f ]

(* Float16 constant, via Float32 then [to_fp]; lossy. *)
let f16_k f =
  app (ifam "to_fp" (float_shape 16)) [ RoundingMode.default; f32_k f ]

let fp_abs f = "fp.abs" $$. [ f ]
let fp_eq f1 f2 = "fp.eq" $$. [ f1; f2 ]
let fp_leq f1 f2 = "fp.leq" $$. [ f1; f2 ]
let fp_lt f1 f2 = "fp.lt" $$. [ f1; f2 ]
let fp_add f1 f2 = "fp.add" $$. [ RoundingMode.default; f1; f2 ]
let fp_sub f1 f2 = "fp.sub" $$. [ RoundingMode.default; f1; f2 ]
let fp_mul f1 f2 = "fp.mul" $$. [ RoundingMode.default; f1; f2 ]
let fp_div f1 f2 = "fp.div" $$. [ RoundingMode.default; f1; f2 ]
let fp_rem f1 f2 = "fp.rem" $$. [ f1; f2 ]

(* [fp_is fc f] tests if [f] belongs to floating-point class [fc]. *)
let fp_is (fc : fpclass) f =
  match fc with
  | FP_normal -> "fp.isNormal" $$. [ f ]
  | FP_subnormal -> "fp.isSubnormal" $$. [ f ]
  | FP_zero -> "fp.isZero" $$. [ f ]
  | FP_infinite -> "fp.isInfinite" $$. [ f ]
  | FP_nan -> "fp.isNaN" $$. [ f ]

let fp_round (rm : RoundingMode.t) f =
  "fp.roundToIntegral" $$. [ RoundingMode.to_sexp rm; f ]

(** {2 Float/bit-vector conversions} *)

(* Bitwise reinterpretation of [bv] as a float of [size] bits (16/32/64/128). *)
let float_of_bv size bv = app (ifam "to_fp" (float_shape size)) [ bv ]

(* Numeric conversion of an unsigned bit-vector to a float. *)
let float_of_ubv rm size bv =
  app (ifam "to_fp_unsigned" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

(* Numeric conversion of a signed bit-vector to a float. *)
let float_of_sbv rm size bv =
  app (ifam "to_fp" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

(* Numeric conversion of a float to an unsigned [n]-bit bit-vector; undefined if
   out of range, NaN or inf. *)
let ubv_of_float rm n f =
  app (ifam "fp.to_ubv" [ n ]) [ RoundingMode.to_sexp rm; f ]

(* Numeric conversion of a float to a signed [n]-bit bit-vector; undefined if
   out of range, NaN or inf. *)
let sbv_of_float rm n f =
  app (ifam "fp.to_sbv" [ n ]) [ RoundingMode.to_sexp rm; f ]

(** {2 Int/bit-vector conversions} *)

(* [int_of_bv signed bv] reads [bv] as a (signed or unsigned) integer. *)
let int_of_bv signed bv =
  if signed then "sbv_to_int" $$. [ bv ] else "ubv_to_int" $$. [ bv ]

(* [bv_of_int size n] converts integer [n] to a [size]-bit bit-vector. *)
let bv_of_int size n = app (ifam "int_to_bv" [ size ]) [ n ]

(** {2 Bit-vector overflow predicates} *)

let bv_nego x = "bvnego" $$. [ x ]
let bv_uaddo l r = "bvuaddo" $$. [ l; r ]
let bv_saddo l r = "bvsaddo" $$. [ l; r ]
let bv_usubo l r = "bvusubo" $$. [ l; r ]
let bv_ssubo l r = "bvssubo" $$. [ l; r ]
let bv_umulo l r = "bvumulo" $$. [ l; r ]
let bv_smulo l r = "bvsmulo" $$. [ l; r ]

(** {2 Sequences} *)

(* [t_seq $ elt] is the type of sequences of [elt]. *)
let t_seq = atom "Seq"
let seq_singl x = atom "seq.unit" $$ [ x ]
let seq_concat xs = atom "seq.++" $$ xs

(** {2 Commands} *)

let simple_command xs = List (List.map atom xs)
let set_option x y = simple_command [ "set-option"; x; y ]
let push n = simple_command [ "push"; string_of_int n ]
let pop n = simple_command [ "pop"; string_of_int n ]
let declare_fun f ps r = "declare-fun" $$. [ Atom f; List ps; r ]
let declare f t = declare_fun f [] t

type con_field = string * sexp

let declare_datatype name type_params cons =
  let mk_field ((f, arg_ty) : con_field) = List [ Atom f; arg_ty ] in
  let mk_con (c, fs) = List (Atom c :: List.map mk_field fs) in
  let mk_cons = List (List.map mk_con cons) in
  let def =
    match type_params with
    | [] -> mk_cons
    | _ -> "par" $$. [ List (List.map atom type_params); mk_cons ]
  in
  "declare-datatype" $$. [ Atom name; def ]

let assume e = "assert" $$. [ e ]

(* SMT-LIB [(reset)] command to reset the solver state. *)
let reset = simple_command [ "reset" ]

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
  ack_command : sexp -> sexp;
      (** Send a command and synchronously wait for its response. Any
          fire-and-forget commands sent earlier with {!field:command} are
          flushed (their acknowledgements drained) first, so the response read
          here lines up with this command's own query. Used for the commands
          whose answer we actually need ([check-sat], [get-model]). *)
  command : sexp -> unit;
      (** Send a command without waiting for its acknowledgement. The solver
          still runs in [:print-success true] mode, so each such command still
          produces exactly one [success] (or error) line; those are drained in
          one batch by the next {!field:ack_command}. This turns the long chain
          of synchronous round-trips (declare / assert / push / pop / reset that
          precedes every [check-sat]) into pipelined writes, which is where most
          of the solver wall-clock time was being spent. *)
  stop : unit -> unit;
  force_stop : unit -> unit;
  config : solver_config;
}

(** {2 Acknowledged commands and checking} *)

(** Send a command and verify the solver acknowledged it with [success].

    This goes through {!field:ack_command}, so it is synchronous: prefer
    {!command} on the hot path where the acknowledgement can be deferred. *)
let ack_command (s : solver) cmd =
  match s.ack_command cmd with
  | Atom "success" -> ()
  | ans -> raise (UnexpectedSolverResponse ans)

(** Send a command without waiting for its acknowledgement; see
    {!field:command}. *)
let command (s : solver) cmd = s.command cmd

type result = Unsat | Unknown | Sat [@@deriving show { with_path = false }]

let s_check_sat = List [ Atom "check-sat" ]

let check s =
  match s.ack_command s_check_sat with
  | Atom "unsat" -> Unsat
  | Atom "sat" -> Sat
  | Atom "unknown" -> Unknown
  | ans -> raise (UnexpectedSolverResponse ans)

(** {2 Models} *)

let s_get_model = List [ Atom "get-model" ]

let get_model s =
  let ans = s.ack_command s_get_model in
  match s.config.exts with
  | CVC5 | Other -> ans
  | Z3 -> (
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
          when String.equal q "forall"
               || String.equal q "exists"
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
      match ans with
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
            | x :: xs -> (
                if StrSet.mem x !processed then arrange xs
                else if StrSet.mem x !processing then
                  raise (UnexpectedSolverResponse ans) (* recursive *)
                else
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

(* Outstanding fire-and-forget commands are drained before the pipe buffer can
   fill: each un-acked command queues a short [success] line in the solver's
   stdout pipe, and left unbounded that could deadlock (we block writing while
   the solver blocks writing its output). The acknowledgements for already-sent
   commands are guaranteed to be in flight, so reading exactly [pending] of them
   never blocks indefinitely. *)
let drain_threshold = 1024

let new_solver (cfg : solver_config) : solver =
  let args = Array.of_list (cfg.exe :: cfg.opts) in
  let proc = Unix.open_process_args_full cfg.exe args [||] in
  let pid = Unix.process_full_pid proc in
  let in_chan, out_chan, in_err_chan = proc in
  let reader = Reader.create in_chan in_err_chan in
  let cmd_buf = Buffer.create 4096 in
  (* Number of fire-and-forget commands whose [success] acknowledgement has not
     been read back yet. *)
  let pending = ref 0 in

  let write_command c =
    Buffer.clear cmd_buf;
    write_buf cmd_buf c;
    Buffer.output_buffer out_chan cmd_buf;
    output_char out_chan '\n';
    flush out_chan;
    cfg.log.send (fun () -> Buffer.contents cmd_buf)
  in
  let read_response () =
    let ans = Reader.read_sexp reader in
    cfg.log.receive (fun () -> to_string ans);
    ans
  in
  (* Read and discard the acknowledgements of all commands sent with [send].

     The solver emits exactly one line per command (a [success], or an [(error
     ...)] if a command actually failed), so we must consume all [n] of them to
     keep the stream aligned -- bailing out on the first error would leave the
     remaining acknowledgements unread and desynchronise every later response.
     We therefore drain the full batch and only afterwards surface the
     unexpected lines, the same way [ack_command] would (the next
     [check]/[get_model] is wrapped to turn this into [Unknown]/[None]). *)
  let drain () =
    let n = !pending in
    pending := 0;
    let bad = ref [] in
    for _ = 1 to n do
      match read_response () with
      | Atom "success" -> ()
      | ans -> bad := ans :: !bad
    done;
    match List.rev !bad with
    | [] -> ()
    | [ ans ] -> raise (UnexpectedSolverResponse ans)
    | bads -> raise (UnexpectedSolverResponse (List bads))
  in
  let command_no_ack c =
    write_command c;
    incr pending;
    if !pending >= drain_threshold then drain ()
  in
  let command_with_ack c =
    drain ();
    write_command c;
    read_response ()
  in
  (* [stop] is idempotent: it can be called explicitly and is also registered as
     a GC finaliser, so [Unix.close_process_full] must not run twice. *)
  let stopped = ref false in
  let stop_command () =
    if not !stopped then (
      stopped := true;
      (try
         output_string out_chan "(exit)\n";
         flush out_chan
       with Sys_error _ -> ());
      let _ = Unix.close_process_full proc in
      cfg.log.stop ())
  in
  let force_stop_command () =
    if not !stopped then (
      stopped := true;
      (try Unix.kill pid 9 with Unix.Unix_error _ -> ());
      let _ = Unix.close_process_full proc in
      cfg.log.stop ())
  in
  let s =
    {
      ack_command = command_with_ack;
      command = command_no_ack;
      stop = stop_command;
      force_stop = force_stop_command;
      config = cfg;
    }
  in
  (* [:print-success] is mandatory for the acknowledgement protocol that
     {!field:command}/{!field:ack_command} rely on, so it is owned here rather
     than left to the client. Z3 keeps it set across [(reset)]; any other
     per-client options (models, timeout, ...) are (re-)established by the
     client, including after a reset. *)
  ack_command s (set_option ":print-success" "true");
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
