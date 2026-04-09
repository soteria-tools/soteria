(** Extensions to [Stdlib.Option] with utility functions. *)

include Stdlib.Option

(** Bind a function over two options. *)
let bind2 f x y = match (x, y) with Some x, Some y -> f x y | _ -> None

(** Map a function over two options. *)
let map2 f x y = match (x, y) with Some x, Some y -> Some (f x y) | _ -> None

(** Iterate a function over two options. *)
let iter2 f x y = match (x, y) with Some x, Some y -> f x y | _ -> ()

(** Merge two options using a function to combine values if both are present. *)
let merge f x y =
  match (x, y) with
  | Some x, Some y -> Some (f x y)
  | Some x, None | None, Some x -> Some x
  | None, None -> None

(** Get the value of an option.

    @raise Invalid_argument if the option is [None]. *)
let get ?(msg = "Option.get None") = function
  | Some x -> x
  | None -> raise (Invalid_argument ("Option.get None:" ^ msg))

(** Merge two options, preferring the first if it is [Some]. Equivalent to
    [merge (fun l _ -> l)] *)
let[@inline] either x y = match x with Some _ -> x | _ -> y
