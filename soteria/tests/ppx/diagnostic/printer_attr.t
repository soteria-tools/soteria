Deriving diagnostic with [@printer]
  $ ../test.sh printer_attr.ml
  module Types = struct
    type ty = int
  end
  
  let pp_ty ft i = Fmt.pf ft "T%d" i
  
  type t = Variant of (Types.ty[@printer pp_ty]) [@diag.format "v=%a"]
  [@@deriving diagnostic]
  
  include struct
    let _ = fun (_ : t) -> ()
    let pp ft (e : t) = match e with Variant v0 -> (Fmt.pf ft "v=%a") pp_ty v0
    let _ = pp
    let kind_string (e : t) = match e with Variant v0 -> "VARIANT"
    let _ = kind_string
  
    let severity (e : t) =
      match e with Variant v0 -> Soteria.Terminal.Diagnostic.Error
  
    let _ = severity
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
