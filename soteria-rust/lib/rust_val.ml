open Charon
module T = Typed.T

type meta = T.cval Typed.t option
and 'ptr full_ptr = 'ptr * meta [@@deriving show { with_path = false }]

(** The type of Rust values *)
type 'ptr t =
  | Base of T.cval Typed.t
      (** a literal value, like a boolean, integer, float *)
  | Ptr of 'ptr full_ptr
      (** pointer, parametric to enable Ruxt, with optional metadata *)
  | Enum of T.cval Typed.t * 'ptr t list
      (** the enum's discriminant and values *)
  | Struct of 'ptr t list  (** contains ordered fields *)
  | Tuple of 'ptr t list  (** n-tuple of values *)
  | Array of 'ptr array_content list
      (** an array, with a custom representation to avoid excessive memory
          consumption for large arrays from [Operand::ArrayRepeat. *)
  | Union of Types.field_id * 'ptr t  (** field and value of union *)
  | ConstFn of Types.fn_ptr  (** the function represented by this ZST value *)

(** To avoid out of memory errors for very large arrays (e.g. [(); usize::MAX]),
    we use a custom array content representation that allows repeated values. *)
and 'ptr array_content =
  | One of 'ptr t  (** Simple list of values *)
  | Repeat of 'ptr t * Z.t  (** a value repeated N times *)

type 'ptr rust_val = 'ptr t

(** [is_empty v] is true if the value is "empty"; ie. it doesn't contain any
    [Base] or [Ptr] value. We also consider [Enum] to be non-empty, because of
    the discriminant, though this *may* be wrong if it's a ZST.

    Used in unsizing, to find the field with the pointer to modify. *)
let rec is_empty = function
  | Base _ -> false
  | Ptr _ -> false
  | Enum (_, _) -> false
  | ConstFn _ -> false
  | Struct fields -> List.for_all is_empty fields
  | Tuple vals -> List.for_all is_empty vals
  | Array vals ->
      List.for_all
        (function One v -> is_empty v | Repeat (v, _) -> is_empty v)
        vals
  | Union (_, v) -> is_empty v

let rec pp_rust_val pp_ptr fmt =
  let pp_rust_val = pp_rust_val pp_ptr in
  function
  | Base v -> Fmt.pf fmt "%a" Typed.ppa v
  | Ptr (p, None) -> Fmt.pf fmt "Ptr(%a)" pp_ptr p
  | Ptr (p, Some meta) -> Fmt.pf fmt "Ptr(%a, %a)" pp_ptr p Typed.ppa meta
  | Enum (disc, vals) ->
      Fmt.pf fmt "Enum(%a: %a)" Typed.ppa disc
        (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val)
        vals
  | Struct fields ->
      Fmt.pf fmt "{%a}" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) fields
  | Tuple vals ->
      Fmt.pf fmt "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_rust_val) vals
  | Array content ->
      Fmt.pf fmt "[%a]"
        Fmt.(list ~sep:(Fmt.any ", ") (pp_array_content pp_ptr))
        content
  | Union (field_id, v) ->
      Fmt.pf fmt "Union(%a: %a)" Types.pp_field_id field_id pp_rust_val v
  | ConstFn fn_ptr -> Fmt.pf fmt "FnPtr(%a)" Types.pp_fn_ptr fn_ptr

and pp_array_content pp_ptr fmt = function
  | One v -> (pp_rust_val pp_ptr) fmt v
  | Repeat (v, n) -> Fmt.pf fmt "%a x %a" (pp_rust_val pp_ptr) v Z.pp_print n

let pp = pp_rust_val
let ppa_rust_val ft rv = pp_rust_val (Fmt.any "?") ft rv
let array lst = Array (List.map (fun v -> One v) lst)

let array_length l =
  List.fold_left
    (fun l v -> match v with One _ -> Z.succ l | Repeat (_, n) -> Z.add l n)
    Z.zero l

let array_repeat v n = Array [ Repeat (v, n) ]

(** [array_map f v] applies [f] to all values in the array content [v], and
    returns the list containing the result. If [v] is an array repetition, it
    will only apply [f] once to that value, on top of once for every exception
    in the repetition. *)
let array_map f = List.map @@ function One v -> f v | Repeat (v, _) -> f v

(** [array_flatten vs] flatten [vs]'s content into a list of rust values; this
    will expand repetitions, and may use a lot of memory. Avoid this if
    possible. *)
let array_flatten vs =
  List.concat_map
    (function
      | One v -> [ v ] | Repeat (v, n) -> List.init (Z.to_int n) (fun _ -> v))
    vs

let unit_ = Tuple []

let as_ptr = function
  | Ptr ptr -> ptr
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a pointer, got: %a"
        ppa_rust_val v

let as_base = function
  | Enum (v, []) | Base v -> v
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v

let as_base_of ~ty = function
  | Enum (v, []) | Base v -> (
      match Typed.cast_checked v ty with
      | Some v -> v
      | None ->
          Fmt.failwith "Unexpected rust_val type, expected %a, got %a (%a)"
            Typed.ppa_ty ty Typed.ppa v Svalue.pp_ty (Typed.get_ty v))
  | v ->
      Fmt.failwith "Unexpected rust_val kind, expected a base value got: %a"
        ppa_rust_val v
