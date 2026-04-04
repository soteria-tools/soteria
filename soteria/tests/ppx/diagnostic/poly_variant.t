Deriving diagnostic polymorphic variants
  $ ../test.sh poly_variant.ml
  type t =
    [ `Overflow [@diag.kind "INTEGER_OVERFLOW"] [@diag.severity Warning]
    | `Parse of string [@diag.format "parse=%a"] ]
  [@@deriving diagnostic]
  
  include struct
    let _ = fun (_ : t) -> ()
  
    let pp ft (e : t) =
      match e with
      | `Overflow -> Format.pp_print_string ft "Overflow"
      | `Parse v0 -> (Fmt.pf ft "parse=%a") Format.pp_print_string v0
  
    let _ = pp
  
    let kind_string (e : t) =
      match e with `Overflow -> "INTEGER_OVERFLOW" | `Parse v0 -> "PARSE"
  
    let _ = kind_string
  
    let severity (e : t) =
      match e with
      | `Overflow -> Soteria.Terminal.Diagnostic.Warning
      | `Parse v0 -> Soteria.Terminal.Diagnostic.Error
  
    let _ = severity
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
