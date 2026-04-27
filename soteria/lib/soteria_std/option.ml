(** Extensions to [Stdlib.Option] with utility functions. *)

include Stdlib.Option

(** The monad equivalent of [some] *)
let return = some

(** The bind operator; we swap it to match the usual order of arguments for
    monads. *)
let bind x f = bind f x

(** Bind a function over two options. *)
let bind2 f x y = match (x, y) with Some x, Some y -> f x y | _ -> None

(** Map a function over two options. *)
let map2 f x y = match (x, y) with Some x, Some y -> Some (f x y) | _ -> None

(** Merge two options using a function to combine values if both are present. *)
let merge f x y =
  match (x, y) with
  | Some x, Some y -> Some (f x y)
  | Some x, None | None, Some x -> Some x
  | None, None -> None

(** [or_ x y] returns [x] if it is some, otherwise returns [y]. Equivalent to
    [merge (fun l _ -> l)] *)
let[@inline] or_ x y = match x with Some _ -> x | _ -> y

(** Get the value of an option.

    @raise Invalid_argument if the option is [None]. *)
let get ?(msg = "Option.get None") = function
  | Some x -> x
  | None -> raise (Invalid_argument ("Option.get None:" ^ msg))
