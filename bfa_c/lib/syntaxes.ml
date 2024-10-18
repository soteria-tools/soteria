module Option = struct
  let ( let+ ) x f = Option.map f x
  let ( let* ) = Option.bind
end

module Result = struct
  let ( let+ ) x f = Result.map f x
  let ( let* ) = Result.bind
  let ( let- ) x f = match x with Ok x -> Ok x | Error e -> f e
end

module List = struct
  let ( let+ ) x f = List.map f x
  let ( let* ) x f = List.concat_map f x
end
