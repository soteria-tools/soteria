include Stdlib.Result

let foldM ~fold xs ~init ~f =
  Monad.foldM ~return:ok ~bind:(fun f x -> bind x f) ~fold ~init ~f xs

let fold_list xs ~init ~f = foldM ~fold:Foldable.List.fold ~init ~f xs
let fold_iter xs ~init ~f = foldM ~fold:Foldable.Iter.fold ~init ~f xs
let fold_seq xs ~init ~f = foldM ~fold:Foldable.Seq.fold ~init ~f xs

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end
