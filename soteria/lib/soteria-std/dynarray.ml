include Stdlib.Dynarray

let pp pp_elem = Fmt.brackets @@ Fmt.iter ~sep:Fmt.comma iter pp_elem
