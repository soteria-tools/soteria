(** Registration of the [%%svalue { ... }] structure-item extension.

    Usage:

    {[
      open Soteria_std
      open Hc
      module Var = Symex.Var

      [%%svalue
      {|
        ty Bool | Int
        leaf    Var  : poly = Var.t
        literal Bool : Bool = bool          as of_bool
        literal Int  : Int  = Z.t           as int_z   print Z.pp_print

        op not : Bool -> Bool "!" = Not { ... }
        op add : Int -> Int -> Int "+" = Plus { commutative fold Z.add ... }
        ...
      |}]
    ]} *)

open Ppxlib

let string_payload =
  Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))

let extract_string (e : expression) : string =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string (s, _, _)) -> s
  | _ ->
      Location.raise_errorf ~loc:e.pexp_loc
        "[%%svalue] expects a single string payload, e.g. [%%svalue {| ... |}]"

let expand ~ctxt e =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let src = extract_string e in
  let prog = Svalue_dsl_parse.parse_program ~base:e.pexp_loc src in
  match Svalue_dsl_gen.generate ~loc prog with
  | [ item ] -> item
  | items ->
      (* [generate] always wraps everything in a single [include struct]. *)
      Ast_builder.Default.pstr_include ~loc
        (Ast_builder.Default.include_infos ~loc
           (Ast_builder.Default.pmod_structure ~loc items))

let ext =
  Extension.V3.declare "svalue" Extension.Context.structure_item
    string_payload expand

let register () = Driver.register_transformation "svalue" ~extensions:[ ext ]
