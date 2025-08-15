include Stdlib.Hashtbl

module type HashedAndStringEncodable = sig
  include Stdlib.Hashtbl.HashedType

  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module MakeYojsonable (Key : HashedAndStringEncodable) = struct
  include Stdlib.Hashtbl.Make (Key)

  let to_yojson (value_to_yojson : 'a -> Yojson.Safe.t) (tbl : 'a t) :
      Yojson.Safe.t =
    let l =
      to_seq tbl |> Seq.map (fun (k, v) -> (Key.to_string k, value_to_yojson v))
    in
    `Assoc (List.of_seq l)

  let of_yojson (value_of_yojson : Yojson.Safe.t -> ('a, string) result)
      (json : Yojson.Safe.t) : ('a t, string) result =
    let open Syntaxes.Result in
    match json with
    | `Assoc kvs -> (
        let tbl = create 0 in

        try
          List.iter
            (fun (k, v) ->
              let res =
                let* k = Key.of_string k in
                let+ v = value_of_yojson v in
                (k, v)
              in
              let k, v =
                Result.get_or_raise (fun s -> Yojson.Json_error s) res
              in
              add tbl k v)
            kvs;
          Ok tbl
        with Yojson.Json_error s -> Error s)
    | _ -> Error "Expected an object"
end

module Hint = MakeYojsonable (Int)

module Hstring = MakeYojsonable (struct
  include String

  let to_string s = s
  let of_string s = Ok s
end)
