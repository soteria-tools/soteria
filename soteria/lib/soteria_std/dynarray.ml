(** Extensions to [Stdlib.Dynarray] with pretty-printing and JSON support.

    This module includes the standard dynamic array implementation and adds
    formatting functions compatible with [Fmt] and JSON serialization via
    [Yojson]. *)

include Stdlib.Dynarray

let pp pp_elem = Fmt.brackets @@ Fmt.iter ~sep:Fmt.comma iter pp_elem

let to_yojson (elem_to_yojson : 'a -> Yojson.Safe.t) t =
  let l = ref [] in
  iter (fun x -> l := elem_to_yojson x :: !l) t;
  `List (List.rev !l)

let of_yojson (elem_of_yojson : Yojson.Safe.t -> ('a, string) Result.t) :
    Yojson.Safe.t -> ('a t, string) Result.t = function
  | `List l ->
      let l = Monad.ResultM.all elem_of_yojson l in
      Result.map of_list l
  | _ -> Error "Expected a list for Dynarray.of_yojson"
