include Stdlib.Dynarray

let pp pp_elem = Fmt.brackets @@ Fmt.iter iter pp_elem
