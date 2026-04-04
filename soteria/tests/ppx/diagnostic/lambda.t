Deriving diagnostic lambda printer
  $ ../test.sh lambda.ml
  type t =
    | Pair of string * int
        [@diag.format fun ft (s, i) -> Fmt.pf ft "pair(%s,%d)" s i]
  [@@deriving diagnostic]
  
  include struct
    let _ = fun (_ : t) -> ()
  
    let pp ft (e : t) =
      match e with
      | Pair (v0, v1) -> (fun ft (s, i) -> Fmt.pf ft "pair(%s,%d)" s i) ft (v0, v1)
  
    let _ = pp
    let kind_string (e : t) = match e with Pair (v0, v1) -> "PAIR"
    let _ = kind_string
  
    let severity (e : t) =
      match e with Pair (v0, v1) -> Soteria.Terminal.Diagnostic.Error
  
    let _ = severity
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
