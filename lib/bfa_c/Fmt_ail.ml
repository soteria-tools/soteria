open Ail_tys
open Cerb_frontend.Pp_ail

let pp_to_fmt (pprinter : 'a -> PPrint.document) : 'a Fmt.t =
 fun ft a ->
  let buffer = Buffer.create 1024 in
  PPrint.ToBuffer.pretty 0.5 80 buffer (pprinter a);
  Fmt.pf ft "%s" (Buffer.contents buffer)

let pp_expr : expr Fmt.t = pp_to_fmt (fun e -> pp_expression e)
let pp_stmt : stmt Fmt.t = pp_to_fmt (fun s -> pp_statement s)

let pp_program : program Fmt.t =
  pp_to_fmt (pp_program ~show_include:true ~executable_spec:false)
