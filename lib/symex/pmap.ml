module type KeyS = sig
  type t
  type 'a symex
  type value

  include Stdlib.Map.OrderedType with type t := t

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
    let constr = Key.distinct (key :: (M.bindings st |> List.map fst)) in
    Result.ok ~learned:[ constr ] (key, M.add key new_codom st)

  let wrap (f : 'a -> ('b * 'a, 'err) Symex.Result.t) (loc : Key.t) (st : 'a t)
      =
    let* found = find_opt_sym loc st in
    match found with
    | Some sst ->
        let++ res, sst' = f sst in
        (* Should I check for emptyness here? *)
        (res, M.add loc sst' st)
    | None -> Symex.Result.error `MissingKey
end
