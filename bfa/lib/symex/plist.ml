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

  type 'a t = 'a M.t * SInt.t

  let pp pp_value ft ((m, b) : 'a t) =
    let pp_binding ft (k, v) = Fmt.pf ft "%a -> %a" SInt.pp k pp_value v in
    Fmt.pf ft "BOUND: %a ; {%a}" SInt.pp b
      (Fmt.list ~sep:(Fmt.any "; ") pp_binding)
      (M.bindings m)

  let empty = M.empty

  (* Symbolic process that under-approximates Map.find_opt *)
  let find_opt_sym (ofs : SInt.t) ((m, b) : 'a t) =
    let rec find_bindings = function
      | [] -> Result.error `MissingOffset
      | (k, v) :: tl ->
          if%sat SInt.sem_eq ofs k then Result.ok v else find_bindings tl
      (* TODO: Investigate: this is not a tailcall, because if%sat is not an if. *)
    in
    if%sat SInt.in_range ofs (SInt.of_int 0, b) then
      match M.find_opt ofs m with
      | Some v -> Result.ok v
      | None -> find_bindings (M.bindings m)
    else Result.error `OutOfBounds

  let create (size : int) ~(new_codom : 'a) : 'a t =
    let bindings = Seq.init size (fun i -> (SInt.of_int i, new_codom)) in
    let m = M.of_seq bindings in
    (m, SInt.of_int size)

  let is_exclusively_owned ((m, b) : 'a t) =
    Symex.return (SInt.sem_eq b (SInt.of_int (M.cardinal m)))

  let wrap (f : 'a -> ('b * 'a, 'err) Symex.Result.t) (ofs : SInt.t)
      ((m, b) : 'a t) =
    let** sst = find_opt_sym ofs (m, b) in
    let++ res, sst' = f sst in
    (* Should I check for emptyness here? *)
    (res, (M.add ofs sst' m, b))
end
