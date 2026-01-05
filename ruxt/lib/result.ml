include Stdlib.Result

let foldM ~fold x ~init ~f = Monad.foldM ~bind ~return:ok ~fold x ~init ~f
let fold_list x ~init ~f = foldM ~fold:Foldable.List.fold x ~init ~f
let fold_iter x ~init ~f = foldM ~fold:Foldable.Iter.fold x ~init ~f
let fold_seq x ~init ~f = foldM ~fold:Foldable.Seq.fold x ~init ~f

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end
