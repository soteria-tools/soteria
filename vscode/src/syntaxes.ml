module Result_promise = struct
  let ( let** ) (x : ('a, 'b) Result.t Promise.t) f =
    let open Promise.Syntax in
    let* res = x in
    match res with Ok x -> f x | Error y -> Promise.return (Error y)
end
