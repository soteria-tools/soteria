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

let rec split_apply expr =
  match expr.pexp_desc with
  | Pexp_apply (fn, args) ->
      let head, args' = split_apply fn in
      (head, args' @ args)
  | _ -> (expr, [])

let expand ~ext expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let fn = associated_fn ~loc ext in
  let fmt, args = split_apply expr in
  (* we use "m__" as the name of the argument to the function passed to [fn] to
     avoid potential name clashes with variables in the original expression *)
  let m_call =
    Ast_builder.Default.pexp_apply ~loc [%expr m__] ((Nolabel, fmt) :: args)
  in
  [%expr [%e fn] (fun m__ -> [%e m_call])]
