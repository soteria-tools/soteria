module Foo = struct
  type t = int

  let init () = 0
  let backtrack_n x n = x - n
  let save x = x + 1
  let reset _ = 0
end

module Bar = struct
  type t = string

  let init () = ""
  let backtrack_n x n = String.sub x 0 (String.length x - n)
  let save x = x ^ "a"
  let reset _ = ""
end
