(** Extensions to [Stdlib.Hashtbl] with JSON support. *)

include Stdlib.Hashtbl

module type S = sig
  include S

  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
end

module Make (Key : HashedType) = struct
  include Make (Key)

  (** Check if there exists a key-value pair in the hash table that satisfies
      the given predicate. *)
  let exists (f : key -> 'a -> bool) (tbl : 'a t) : bool =
    try
      iter (fun k v -> if f k v then raise Exit) tbl;
      false
    with Exit -> true

  (** Check if all key-value pairs in the hash table satisfy the given
      predicate. *)
  let for_all (f : key -> 'a -> bool) (tbl : 'a t) : bool =
    try
      iter (fun k v -> if not (f k v) then raise Exit) tbl;
      true
    with Exit -> false
end

module type HashedAndStringEncodable = sig
  include Stdlib.Hashtbl.HashedType

  val to_string : t -> string
  val of_string : string -> (t, string) result
end

module MakeYojsonable (Key : HashedAndStringEncodable) = struct
  include Make (Key)

  (** Convert a hash table to a Yojson object. *)
  let to_yojson (value_to_yojson : 'a -> Yojson.Safe.t) (tbl : 'a t) :
      Yojson.Safe.t =
    let l =
      to_seq tbl |> Seq.map (fun (k, v) -> (Key.to_string k, value_to_yojson v))
    in
    `Assoc (List.of_seq l)

  (** Convert a Yojson object to a hash table. *)
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
              let k, v = Result.get_or ~err:Yojson.json_error res in
              add tbl k v)
            kvs;
          Ok tbl
        with Yojson.Json_error s -> Error s)
    | _ -> Error "Expected an object"
end

(** Hash table with integer keys and JSON support. *)
module Hint = MakeYojsonable (Int)

(** Hash table with string keys and JSON support. *)
module Hstring = MakeYojsonable (struct
  include String

  let to_string s = s
  let of_string s = Ok s
end)
