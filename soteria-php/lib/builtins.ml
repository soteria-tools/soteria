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
  let open Phpsymex.Syntax in
  if%sat[@lname "Assertion failure"] [@rname "Assertion success"]
    Value.Typed.Bool.not condition
  then Phpsymex.error Error.Failed_assertion
  else Phpsymex.Result.ok Value.null

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

let runtime_error name args =
  let name = canonical_name name in
  let count function_name expected =
    let actual = List.length args in
    if actual = expected then None
    else
      Some
        {
          Error.class_name = "ArgumentCountError";
          message =
            Printf.sprintf "%s expects exactly %d argument%s, %d given"
              function_name expected
              (if expected = 1 then "" else "s")
              actual;
        }
  in
  match name with
  | "soteria\\symbolic_bool" -> count "Soteria\\symbolic_bool()" 0
  | "soteria\\symbolic_int" -> count "Soteria\\symbolic_int()" 0
  | "soteria\\symbolic_float" -> count "Soteria\\symbolic_float()" 0
  | "soteria\\expect_fail" -> count "Soteria\\expect_fail()" 0
  | ("soteria\\assume" | "soteria\\assert") as name -> (
      let function_name =
        if String.equal name "soteria\\assume" then "Soteria\\assume()"
        else "Soteria\\assert()"
      in
      match (count function_name 1, args) with
      | Some error, _ -> Some error
      | None, [ Value.Bool _ ] -> None
      | None, [ value ] ->
          Some
            {
              Error.class_name = "TypeError";
              message =
                Printf.sprintf "%s argument #1 must be of type bool, %s given"
                  function_name (Value.type_name value);
            }
      | None, _ -> assert false)
  | _ -> None

let call name args =
  match find name with
  | Some implementation -> implementation ~args
  | None -> Phpsymex.not_impl ("builtin " ^ name)
