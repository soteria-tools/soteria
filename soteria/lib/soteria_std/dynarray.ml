(** Extensions to [Stdlib.Dynarray] with pretty-printing and JSON support. *)

include Stdlib.Dynarray

(** Pretty-printer for dynamic arrays compatible with [Fmt]. *)
let pp pp_elem = Fmt.brackets @@ Fmt.iter ~sep:Fmt.comma iter pp_elem

(** Convert a dynamic array to a Yojson list using the provided element
    converter. *)
let to_yojson (elem_to_yojson : 'a -> Yojson.Safe.t) t =
  let l = ref [] in
  iter (fun x -> l := elem_to_yojson x :: !l) t;
  `List (List.rev !l)

(** Convert a Yojson value to a dynamic array using the provided element
    converter. *)
let of_yojson (elem_of_yojson : Yojson.Safe.t -> ('a, string) Result.t) :
    Yojson.Safe.t -> ('a t, string) Result.t = function
  | `List l ->
      let l = Monad.ResultM.all elem_of_yojson l in
      Result.map of_list l
  | _ -> Error "Expected a list for Dynarray.of_yojson"
