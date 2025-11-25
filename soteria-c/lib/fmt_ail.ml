open Cerb_frontend.AilSyntax
open Ail_tys
open Cerb_frontend.Pp_ail

let pp_to_fmt (pprinter : 'a -> PPrint.document) :
    Format.formatter -> 'a -> unit =
 fun ft a ->
  let buffer = Buffer.create 1023 in
  PPrint.ToBuffer.pretty 0.5 80 buffer (pprinter a);
  Fmt.pf ft "%s" (Buffer.contents buffer)

let pp_id_kind ft kind =
  Fmt.string ft
    (match kind with
    | IK_declaration -> "decl"
    | IK_definition -> "def"
    | IK_tentative -> "tent")

let pp_loc = Fmt.of_to_string Cerb_location.location_to_string
let pp_id = pp_to_fmt (Cerb_frontend.Pp_symbol.pp_identifier ~clever:true)
let pp_sym = Symbol_std.pp
let pp_ty = pp_to_fmt Cerb_frontend.Pp_core_ctype.pp_ctype
let pp_ty_ ft t = pp_ty ft (Ctype ([], t))
let pp_int_ty = pp_to_fmt Cerb_frontend.Pp_ail.pp_integerType
let pp_arithop = pp_to_fmt pp_arithmeticOperator
let pp_binop = pp_to_fmt pp_binaryOperator
let pp_unop = pp_to_fmt pp_unaryOperator
let pp_constant = pp_to_fmt pp_constant

let pp_invalid_reason ft (reason : ail_invalid_reason) =
  match reason with
  | AilInvalid_desugaring_init_failed msg ->
      Fmt.pf ft "desugaring init failed: %s" msg
  | AilInvalid_other msg -> Fmt.pf ft "other: %s" msg

let pp_expr : Format.formatter -> expr -> unit =
  pp_to_fmt (fun e -> pp_expression e)

let pp_stmt : Format.formatter -> stmt -> unit =
  pp_to_fmt (fun s -> pp_statement s)

let pp_program : Format.formatter -> program -> unit =
  pp_to_fmt (pp_program ~show_include:true)

let pp_program_ast ft (entry_point, sigma) =
  let profile = !Soteria.Terminal.Profile.profile in
  let show_includes = true in
  pp_to_fmt
    (Cerb_frontend.Pp_ail_ast.pp_program profile.color show_includes)
    ft (entry_point, sigma)
