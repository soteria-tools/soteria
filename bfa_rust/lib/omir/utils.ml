module Res_list = struct
  type ('a, 'e) t = ('a, 'e) result list

  let bind (l : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
    List.map (function Ok x -> f x | Error e -> [ Error e ]) l |> List.concat

  let combine (l : ('a, 'e) t) : ('a list, 'e) result =
    let rec aux (l : ('a, 'e) t) (acc : 'a list) : ('a list, 'e) result =
      match l with
      | [] -> Ok acc
      | Ok x :: tl -> aux tl (x :: acc)
      | Error e :: _ -> Error e
    in
    aux l []
end

module Opt_list = struct
  type 'a t = 'a option list

  let to_res_list (e : 'e) : 'a t -> ('a, 'e) Res_list.t =
    List.map (function Some x -> Ok x | None -> Error e)

  let combine (l : 'a t) : 'a list option =
    let rec aux (l : 'a t) (acc : 'a list) : 'a list option =
      match l with
      | [] -> Some acc
      | Some x :: tl -> aux tl (x :: acc)
      | None :: _ -> None
    in
    aux l []
end

module JsonUtils = struct
  let ( $> ) json name = Yojson.Safe.Util.member name json
  let ( $>@ ) json name = Yojson.Safe.Util.to_list (json $> name)

  let ( $>. ) json () =
    match json with `Null -> Ok () | _ -> Error "Expected null"

  let to_string_res : Yojson.Safe.t -> (string, string) result = function
    | `String s -> Ok s
    | _ -> Error "Expected string"

  let to_int_res : Yojson.Safe.t -> (int, string) result = function
    | `Int i -> Ok i
    | _ -> Error "Expected integer"

  let to_int_list_res : Yojson.Safe.t -> (int list, string) result = function
    | `List l -> List.map to_int_res l |> Res_list.combine
    | _ -> Error "Expected list of integers"
end

module Syntaxes = struct
  module Result = struct
    let ( let* ) = Result.bind
    let ( let+ ) = Result.map
    let ( let*@ ) l f = Result.bind (Res_list.combine l) f
  end

  module Option = struct
    let ( let* ) = Option.bind
    let ( let+ ) = Option.map
  end

  module Pipe = struct
    let ( |>+ ) x y = x |> List.map y
  end
end
