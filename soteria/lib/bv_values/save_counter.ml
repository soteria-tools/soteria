type t = int ref

let init () = ref 0
let reset t = t := 0
let save t = incr t
let backtrack_n t n = t := !t - n
