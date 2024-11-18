module type KeyS = sig
  type t
  type 'a symex
  type value

  include Stdlib.Map.OrderedType with type t := t

  val pp : t Fmt.t
  val sem_eq : t -> t -> value
  val fresh : unit -> t symex
  val distinct : t list -> value
end

module Make
    (Symex : Symex.S)
    (Key : KeyS with type 'a symex = 'a Symex.t and type value = Symex.Value.t) =
struct
  open Symex.Syntax
  open Symex
  module M = Stdlib.Map.Make (Key)

  type 'a t = 'a M.t

  let pp pp_value =
    let open Fmt in
    let iter f = M.iter (fun k v -> f (k, v)) in
    let pp_binding ft (k, v) = pf ft "@[<2>%a ->@ %a@]" Key.pp k pp_value v in
    braces (Fmt.iter ~sep:(any ";@\n") iter pp_binding)

  let empty = M.empty

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (loc : Key.t) (st : 'a t) =
    let rec find_bindings = function
      | [] -> Symex.return None
      | (k, v) :: tl ->
          if%sat Key.sem_eq loc k then Symex.return (Some v)
          else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an if. *)
    in
    match M.find_opt loc st with
    | Some v -> Symex.return (Some v)
    | None -> find_bindings (M.bindings st)

  let alloc (type a) ~(new_codom : a) (st : a t) : (Key.t * a t, 'err) Result.t
      =
    let* key = Key.fresh () in
    let learned =
      if M.is_empty st then []
      else [ Key.distinct (key :: (M.bindings st |> List.map fst)) ]
    in
    Result.ok ~learned (key, M.add key new_codom st)

  let wrap (f : 'a -> ('b * 'a, 'err) Symex.Result.t) (loc : Key.t) (st : 'a t)
      =
    let* found = find_opt_sym loc st in
    match found with
    | Some sst ->
        let++ res, sst' = f sst in
        (* Should I check for emptyness here? *)
        (res, M.add loc sst' st)
    | None -> Symex.Result.error `MissingKey

  let wrap_read_only (f : 'a -> ('b, 'err) Symex.Result.t) (loc : Key.t)
      (st : 'a t) =
    let* found = find_opt_sym loc st in
    match found with
    | Some sst -> f sst
    | None -> Symex.Result.error `MissingKey
end
