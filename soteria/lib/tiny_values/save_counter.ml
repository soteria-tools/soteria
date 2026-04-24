type t = int ref

let init () = ref 0

let reset t =
  if !t < 0 then failwith "Solver reset: save_counter < 0";
  t := 0

let save t = incr t
let backtrack_n t n = t := !t - n
