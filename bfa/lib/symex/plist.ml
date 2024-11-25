module type SInt_sig = sig
  (** Symbolic integers *)

  type t
  type 'a symex
  type value

  include Stdlib.Map.OrderedType with type t := t

  val pp : t Fmt.t
  val sem_eq : t -> t -> value
  val of_int : int -> t
  val in_range : t -> t * t -> value
  val greater_than : t -> t -> value
end

module Make
    (Symex : Symex.S)
    (SInt : SInt_sig
              with type 'a symex = 'a Symex.t
               and type value = Symex.Value.t) =
struct
  open Symex.Syntax
  open Symex
  module M = Stdlib.Map.Make (SInt)

  type 'a t = 'a M.t * SInt.t option

  let of_opt = function None -> (M.empty, None) | Some l -> l
  let add_opt k v m = M.update k (fun _ -> v) m

  let to_opt (m, b) =
    if Option.is_none b && M.is_empty m then None else Some (m, b)

  let pp pp_value ft ((m, b) : 'a t) =
    let pp_binding ft (k, v) = Fmt.pf ft "%a -> %a" SInt.pp k pp_value v in
    Fmt.pf ft "BOUND: %a ; {%a}" (Fmt.Dump.option SInt.pp) b
      (Fmt.list ~sep:(Fmt.any "; ") pp_binding)
      (M.bindings m)

  let empty = M.empty

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (ofs : SInt.t) (m : 'a M.t) =
    let rec find_bindings = function
      | [] -> Symex.return None
      | (k, v) :: tl ->
          if%sat SInt.sem_eq ofs k then Symex.return (Some v)
          else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an if. *)
    in
    match M.find_opt ofs m with
    | Some v -> Symex.return (Some v)
    | None -> find_bindings (M.bindings m)

  let assert_in_bounds (ofs : SInt.t) (b : SInt.t option) =
    let cond =
      match b with
      | None -> SInt.greater_than ofs (SInt.of_int 0)
      | Some b -> SInt.in_range ofs (SInt.of_int 0, b)
    in
    if%sat Symex.Value.not cond then Result.error `OutOfBounds else Result.ok ()

  let create (size : int) ~(new_codom : 'a) : 'a t =
    if size <= 0 then
      raise (Invalid_argument "Plist.create: size must be positive");
    let bindings = Seq.init size (fun i -> (SInt.of_int i, new_codom)) in
    let m = M.of_seq bindings in
    (m, Some (SInt.of_int size))

  let assert_exclusively_owned (b_opt : 'a t option) =
    let err = Result.error `MissingResource in
    match of_opt b_opt with
    | _, None -> err
    | m, Some b ->
        if%sat SInt.sem_eq b (SInt.of_int (M.cardinal m)) then Result.ok ()
        else err

  let wrap (f : 'a option -> ('b * 'a option, 'err) Symex.Result.t)
      (ofs : SInt.t) (t_opt : 'a t option) :
      ('b * 'a t option, 'err) Symex.Result.t =
    let m, b = of_opt t_opt in
    let** () = assert_in_bounds ofs b in
    let* sst = find_opt_sym ofs m in
    let++ res, sst' = f sst in
    (* Should I check for emptyness here? *)
    (res, to_opt (add_opt ofs sst' m, b))
end
