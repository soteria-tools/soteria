(** Extensions to [Stdlib.Int] with safe string conversion. *)

include Stdlib.Int

let to_string = string_of_int

let of_string s =
  match int_of_string_opt s with
  | Some i -> Ok i
  | None -> Error ("Invalid integer: " ^ s)
