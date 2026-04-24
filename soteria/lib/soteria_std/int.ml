(** Extensions to [Stdlib.Int] with safe string conversion. *)

include Stdlib.Int

(** Convert an integer to a string. *)
let to_string = string_of_int

(** Parse an integer from a string. *)
let of_string s =
  match int_of_string_opt s with
  | Some i -> Ok i
  | None -> Error ("Invalid integer: " ^ s)

(** No-op, used for [PatriciaTree] functors. *)
let to_int = Fun.id

(** Pretty printer *)
let pp = Fmt.int
