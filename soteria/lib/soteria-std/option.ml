include Stdlib.Option

let bind2 f x y = match (x, y) with Some x, Some y -> f x y | _ -> None
let map2 f x y = match (x, y) with Some x, Some y -> Some (f x y) | _ -> None

let merge f x y =
  match (x, y) with
  | Some x, Some y -> Some (f x y)
  | Some x, None | None, Some x -> Some x
  | None, None -> None
