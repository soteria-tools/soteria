open Ppxlib

module Extension_name = struct
  type t = Debug | Info | Warn | Error | Trace | Smt

  let to_string = function
    | Debug -> "l.debug"
    | Info -> "l.info"
    | Warn -> "l.warn"
    | Error -> "l.error"
    | Trace -> "l.trace"
    | Smt -> "l.smt"
end

let associated_fn ~loc = function
  | Extension_name.Debug -> [%expr L.debug]
  | Info -> [%expr L.info]
  | Warn -> [%expr L.warn]
  | Error -> [%expr L.error]
  | Trace -> [%expr L.trace]
  | Smt -> [%expr L.smt]

let split_apply expr =
  let rec aux acc expr =
    match expr.pexp_desc with
    | Pexp_apply (fn, args) -> aux (args @ acc) fn
    | _ -> (expr, acc)
  in
  aux [] expr

let validate_payload ~ext fmt args =
  let string_of_arg_label = function
    | Nolabel -> ""
    | Labelled l -> "~" ^ l
    | Optional l -> "?" ^ l
  in
  let has_labelled_arg = function Nolabel, _ -> false | _ -> true in
  match List.find_opt has_labelled_arg args with
  | Some (label, _) ->
      Location.raise_errorf ~loc:fmt.pexp_loc
        "%%%s does not support labelled or optional arguments (found %s)"
        (Extension_name.to_string ext)
        (string_of_arg_label label)
  | None -> (
      match fmt.pexp_desc with
      | Pexp_constant (Pconst_string _) -> ()
      | _ ->
          Location.raise_errorf ~loc:fmt.pexp_loc
            "%%%s expects a string literal format as first argument"
            (Extension_name.to_string ext))

let expand ~ext expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let fn = associated_fn ~loc ext in
  let fmt, args = split_apply expr in
  let () = validate_payload ~ext fmt args in
  (* we use "m__" as the name of the argument to the function passed to [fn] to
     avoid potential name clashes with variables in the original expression *)
  let m_call =
    Ast_builder.Default.pexp_apply ~loc [%expr m__] ((Nolabel, fmt) :: args)
  in
  [%expr [%e fn] (fun m__ -> [%e m_call])]
