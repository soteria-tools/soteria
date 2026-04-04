Deriving diagnostic
  $ ../test.sh simple.ml
  type t =
    | Overflow [@diag.kind "INTEGER_OVERFLOW"] [@diag.severity Warning]
        [@diag.format "Overflow"]
    | Parse_error of string [@diag.kind "PARSING_ERROR"] [@diag.severity Error]
        [@diag.format "Parsing error: %a"]
  [@@deriving diagnostic]
  
  include struct
    let _ = fun (_ : t) -> ()
  
    let pp ft (e : t) =
      match e with
      | Overflow -> Format.pp_print_string ft "Overflow"
      | Parse_error v0 ->
          (Fmt.pf ft "Parsing error: %a") Format.pp_print_string v0
  
    let _ = pp
  
    let kind_string (e : t) =
      match e with
      | Overflow -> "INTEGER_OVERFLOW"
      | Parse_error v0 -> "PARSING_ERROR"
  
    let _ = kind_string
  
    let severity (e : t) =
      match e with
      | Overflow -> Soteria.Terminal.Diagnostic.Warning
      | Parse_error v0 -> Soteria.Terminal.Diagnostic.Error
  
    let _ = severity
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
