type implementation =
  args:Value.t list -> (Value.t, Error.with_trace, unit) Phpsymex.Result.t

let invalid_count function_name expected actual =
  Phpsymex.error
    (Error.Invalid_argument_count { function_name; expected; actual })

let no_arguments function_name args run =
  match args with
  | [] -> run ()
  | _ -> invalid_count function_name 0 (List.length args)

let boolean_argument function_name args run =
  match args with
  | [ Value.Bool condition ] -> run condition
  | [ value ] ->
      Phpsymex.error
        (Error.Invalid_argument_type
           {
             function_name;
             position = 1;
             expected = "bool";
             actual = Value.kind value;
           })
  | _ -> invalid_count function_name 1 (List.length args)

let symbolic_bool ~args =
  no_arguments "Soteria\\symbolic_bool()" args @@ fun () ->
  let open Phpsymex.Syntax in
  let* value = Phpsymex.nondet Value.Typed.t_bool in
  let value = Value.Bool value in
  let* () = Phpsymex.record_symbolic_input value in
  Phpsymex.Result.ok value

let symbolic_int ~args =
  no_arguments "Soteria\\symbolic_int()" args @@ fun () ->
  let open Phpsymex.Syntax in
  let* value = Phpsymex.nondet (Value.Typed.t_int Value.integer_bits) in
  let value = Value.Int value in
  let* () = Phpsymex.record_symbolic_input value in
  Phpsymex.Result.ok value

let symbolic_float ~args =
  no_arguments "Soteria\\symbolic_float()" args @@ fun () ->
  let open Phpsymex.Syntax in
  let* value = Phpsymex.nondet Value.Typed.t_f64 in
  let value = Value.Float value in
  let* () = Phpsymex.record_symbolic_input value in
  Phpsymex.Result.ok value

let expect_fail ~args =
  no_arguments "Soteria\\expect_fail()" args @@ fun () ->
  let open Phpsymex.Syntax in
  let* () = Phpsymex.expect_failure () in
  Phpsymex.Result.ok Value.null

let assume ~args =
  boolean_argument "Soteria\\assume()" args @@ fun condition ->
  let open Phpsymex.Syntax in
  let* () = Phpsymex.assume [ condition ] in
  Phpsymex.Result.ok Value.null

let assert_ ~args =
  boolean_argument "Soteria\\assert()" args @@ fun condition ->
  Phpsymex.branch_on
    (Value.Typed.Bool.not condition)
    ~left_branch_name:"Assertion failure" ~right_branch_name:"Assertion success"
    ~then_:(fun () -> Phpsymex.error Error.Failed_assertion)
    ~else_:(fun () -> Phpsymex.Result.ok Value.null)

let implementations : (string * implementation) list =
  [
    ("soteria\\symbolic_bool", symbolic_bool);
    ("soteria\\symbolic_int", symbolic_int);
    ("soteria\\symbolic_float", symbolic_float);
    ("soteria\\assume", assume);
    ("soteria\\assert", assert_);
    ("soteria\\expect_fail", expect_fail);
  ]

let canonical_name name =
  let name =
    if String.length name > 0 && name.[0] = '\\' then
      String.sub name 1 (String.length name - 1)
    else name
  in
  String.lowercase_ascii name

let find name = List.assoc_opt (canonical_name name) implementations

let call name args =
  match find name with
  | Some implementation -> implementation ~args
  | None -> Phpsymex.not_impl ("builtin " ^ name)
