module Solver =
  Soteria.Solvers.Z3.Make (Soteria.Bv_values.Encoding.Make (Value.Typed))

module Var_map = Soteria.Symex.Var.Map

type concrete = Bool of bool | Int of int64 | Float of float
type binding = { name : string; value : concrete }

let variables inputs path_condition =
  let add_value variables value =
    let result = ref variables in
    Value.Typed.iter_vars value (fun (variable, ty) ->
        result := Var_map.add variable (Value.Typed.untype_type ty) !result);
    !result
  in
  let variables =
    List.fold_left
      (fun variables expression ->
        add_value variables (Value.Typed.type_ expression))
      Var_map.empty path_condition
  in
  List.fold_left
    (fun variables (input : Error.Trace.symbolic_input) ->
      match input.value with
      | Value.Bool value -> add_value variables value
      | Value.Int value -> add_value variables value
      | Value.Float value -> add_value variables value
      | _ -> variables)
    variables inputs

let model_definitions = function
  | Soteria.Smt.List definitions ->
      List.filter_map
        (function
          | Soteria.Smt.List
              [ Atom "define-fun"; Atom name; List []; _sort; body ] ->
              Some (name, body)
          | _ -> None)
        definitions
  | Atom _ -> []

let rec resolve definitions = function
  | Soteria.Smt.Atom name as value -> (
      match List.assoc_opt name definitions with
      | Some resolved when resolved <> value -> resolve definitions resolved
      | Some _ | None -> value)
  | value -> value

let z_of_binary digits =
  let value = ref Z.zero in
  String.iter
    (function
      | '0' -> value := Z.shift_left !value 1
      | '1' -> value := Z.succ (Z.shift_left !value 1)
      | _ -> raise Exit)
    digits;
  !value

let z_of_bitvector = function
  | Soteria.Smt.Atom value when String.starts_with ~prefix:"#b" value ->
      Some (z_of_binary (String.sub value 2 (String.length value - 2)))
  | Atom value when String.starts_with ~prefix:"#x" value ->
      Some (Z.of_string_base 16 (String.sub value 2 (String.length value - 2)))
  | List [ Atom "_"; Atom value; Atom _width ]
    when String.starts_with ~prefix:"bv" value ->
      Some (Z.of_string (String.sub value 2 (String.length value - 2)))
  | _ -> None

let bool_of_model definitions value =
  match resolve definitions value with
  | Soteria.Smt.Atom "true" -> Some true
  | Atom "false" -> Some false
  | _ -> None

let int_of_model definitions value =
  resolve definitions value
  |> z_of_bitvector
  |> Option.map (Value.Typed.BitVec.bv_to_z true Value.integer_bits)
  |> Option.map Z.to_int64

let float_of_parts sign exponent significand =
  match
    (z_of_bitvector sign, z_of_bitvector exponent, z_of_bitvector significand)
  with
  | Some sign, Some exponent, Some significand ->
      let bits =
        Z.logor (Z.shift_left sign 63)
          (Z.logor (Z.shift_left exponent 52) significand)
      in
      Some (Int64.float_of_bits (Z.to_int64 bits))
  | _ -> None

let float_of_model definitions value =
  match resolve definitions value with
  | Soteria.Smt.List [ Atom "fp"; sign; exponent; significand ] ->
      float_of_parts sign exponent significand
  | List [ Atom "_"; Atom "+zero"; Atom "11"; Atom "53" ] -> Some 0.0
  | List [ Atom "_"; Atom "-zero"; Atom "11"; Atom "53" ] -> Some (-0.0)
  | List [ Atom "_"; Atom "+oo"; Atom "11"; Atom "53" ] -> Some infinity
  | List [ Atom "_"; Atom "-oo"; Atom "11"; Atom "53" ] -> Some neg_infinity
  | List [ Atom "_"; Atom "NaN"; Atom "11"; Atom "53" ] -> Some nan
  | _ -> None

let variable_of_value value =
  let variable = ref None in
  Value.Typed.iter_vars value (fun (found, _) -> variable := Some found);
  !variable

let bindings ~inputs ~path_condition =
  let inputs = List.rev inputs in
  if inputs = [] then Some []
  else
    let solver = Solver.init () in
    variables inputs path_condition
    |> Var_map.iter (fun variable ty -> Solver.declare_var solver variable ty);
    List.iter (Solver.add_constraint solver) path_condition;
    match Solver.check_sat solver with
    | Soteria.Symex.Solver_result.Sat -> (
        match Solver.get_model solver with
        | None -> None
        | Some model ->
            let definitions = model_definitions model in
            let concrete_value symbolic parse default =
              Option.bind (variable_of_value symbolic) (fun variable ->
                  match
                    List.assoc_opt
                      (Soteria.Symex.Var.to_string variable)
                      definitions
                  with
                  | None -> Some default
                  | Some body -> parse definitions body)
            in
            let binding (input : Error.Trace.symbolic_input) =
              match input.value with
              | Value.Bool symbolic ->
                  Option.map
                    (fun value -> { name = input.name; value = Bool value })
                    (concrete_value symbolic bool_of_model false)
              | Value.Int symbolic ->
                  Option.map
                    (fun value -> { name = input.name; value = Int value })
                    (concrete_value symbolic int_of_model 0L)
              | Value.Float symbolic ->
                  Option.map
                    (fun value -> { name = input.name; value = Float value })
                    (concrete_value symbolic float_of_model 0.0)
              | _ -> None
            in
            let rec collect bindings = function
              | [] -> Some (List.rev bindings)
              | input :: inputs ->
                  Option.bind (binding input) (fun binding ->
                      collect (binding :: bindings) inputs)
            in
            collect [] inputs)
    | Unsat | Unknown -> None

let pp_float formatter value =
  match classify_float value with
  | FP_nan -> Format.pp_print_string formatter "NAN"
  | FP_infinite when value > 0.0 -> Format.pp_print_string formatter "INF"
  | FP_infinite -> Format.pp_print_string formatter "-INF"
  | FP_normal | FP_subnormal | FP_zero ->
      let rendered = Printf.sprintf "%.17g" value in
      if
        String.contains rendered '.'
        || String.contains rendered 'e'
        || String.contains rendered 'E'
      then Format.pp_print_string formatter rendered
      else Format.fprintf formatter "%s.0" rendered

let pp_concrete formatter = function
  | Bool value -> Format.pp_print_bool formatter value
  | Int value -> Format.pp_print_string formatter (Int64.to_string value)
  | Float value -> pp_float formatter value

let pp_binding formatter { name; value } =
  Format.fprintf formatter "$%s = %a" name pp_concrete value

let print bindings =
  match bindings with
  | [] -> ()
  | _ ->
      Format.eprintf "@[<v>Counterexample:@,%a@]@."
        (Format.pp_print_list
           ~pp_sep:(fun formatter () -> Format.pp_print_cut formatter ())
           pp_binding)
        bindings
