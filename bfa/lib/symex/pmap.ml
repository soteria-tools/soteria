module type KeyS = sig
  type t
  type 'a symex
  type value

  include Stdlib.Map.OrderedType with type t := t

  val pp : t Fmt.t
  val sem_eq : t -> t -> value
  val fresh : ?constrs:(t -> value list) -> unit -> t symex
  val distinct : t list -> value list
end

module Make
    (Symex : Symex.S)
    (Key : KeyS with type 'a symex = 'a Symex.t and type value = Symex.Value.t) =
struct
  open Symex.Syntax
  open Symex
  module M = Stdlib.Map.Make (Key)

  type 'a t = 'a M.t
  type 'a cp = Key.t * 'a

  let pp ?(ignore = fun _ -> false) pp_value =
    let open Fmt in
    let iter f = M.iter (fun k v -> f (k, v)) in
    let pp_binding ft (k, v) = pf ft "@[<2>%a ->@ %a@]" Key.pp k pp_value v in
    let iter_non_ignored f m =
      iter (fun (k, v) -> if ignore (k, v) then () else f (k, v)) m
    in
    braces (Fmt.iter ~sep:(any ";@\n") iter_non_ignored pp_binding)

  let of_opt = function None -> M.empty | Some m -> m
  let to_opt m = if M.is_empty m then None else Some m
  let add_opt k v m = M.update k (fun _ -> v) m

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

  let alloc (type a) ~(new_codom : a) (st : a t option) :
      (Key.t * a t option, 'err) Result.t =
    let st = of_opt st in
    let* key =
      Key.fresh
        ~constrs:(fun key ->
          Key.distinct (key :: (M.bindings st |> List.map fst)))
        ()
    in
    Result.ok (key, to_opt (M.add key new_codom st))

  let wrap (f : 'a option -> ('b * 'a option, 'err) Symex.Result.t)
      (loc : Key.t) (st : 'a t option) =
    let st = of_opt st in
    let* codom = find_opt_sym loc st in
    let++ res, codom = f codom in
    (res, to_opt (add_opt loc codom st))

  let produce (prod : 'inner_cp -> 'inner_st option -> 'inner_st option Symex.t)
      (cp : 'inner_cp cp) (st : 'inner_st t option) : 'inner_st t option Symex.t
      =
    let st = of_opt st in
    let loc, inner_cp = cp in
    let* codom = find_opt_sym loc st in
    let+ codom = prod inner_cp codom in
    to_opt (add_opt loc codom st)

  let consume
      (cons :
        'inner_cp ->
        'inner_st option ->
        ('a * 'inner_st option, 'err) Symex.Result.t) (cp : 'inner_cp cp)
      (st : 'inner_st t option) =
    let st = of_opt st in
    let loc, inner_cp = cp in
    let* codom = find_opt_sym loc st in
    let++ res, codom = cons inner_cp codom in
    (res, to_opt (add_opt loc codom st))

  let wrap_read_only (f : 'a option -> ('b, 'err) Symex.Result.t) (loc : Key.t)
      (st : 'a t option) =
    let st = of_opt st in
    let* codom = find_opt_sym loc st in
    f codom
end
