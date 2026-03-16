open Rustsymex
open Charon
open Typed
open Typed.Syntax
open Typed.Infix
open T

(** A map to store information on "decayed" pointers, namely mapping from
    locations to integers (their decayed) value.

    It exposes two functions: [decay], to convert a location to an integer
    (given the size and alignment of its allocation), and [from_exposed] which
    does the reverse operation. Importantly, while [decay] is total,
    [from_exposed] is partial: if a provenance cannot be guessed from the
    integer, [None] is returned. *)
module type DecayMapS = sig
  type t

  val empty : t
  val pp : Format.formatter -> t -> unit

  (** Decays the given location into an integer, updating the decay map
      accordingly. If [expose] is true, the provenance is marked as exposed, and
      can be retrieved later with [from_exposed]. Returns the decayed integer,
      along with the updated decay map. *)
  val decay :
    expose:bool ->
    size:[< sint ] Typed.t ->
    align:[< nonzero ] Typed.t ->
    [< sloc ] Typed.t ->
    t ->
    (sint Typed.t * t) Rustsymex.t

  (** Tries finding, for the given integer, the matching provenance in the decay
      map. If found, it returns that provenance, along with the exposed address
      for that allocation at offset 0. Otherwise returns [None]. *)
  val from_exposed :
    [< sint ] Typed.t ->
    t ->
    ((sloc Typed.t * sint Typed.t) option * t) Rustsymex.t
end

module DecayMap : DecayMapS = struct
  module MapKey = struct
    include Typed

    type t = sloc Typed.t

    let to_int = unique_tag
    let pp = ppa
    let simplify = Rustsymex.simplify
  end

  type entry = { address : sint Typed.t; exposed : bool }
  [@@deriving show { with_path = false }]

  module Map =
    Soteria.Data.S_map.Direct_access_patricia_tree (Rustsymex) (MapKey)

  type t = entry Map.t

  let pp = Map.pp pp_entry
  let empty = Map.empty

  let decay ~expose ~size ~align (loc : [< sloc ] Typed.t) (map : t) :
      (T.sint Typed.t * t) Rustsymex.t =
    let open Rustsymex.Syntax in
    if%sat Typed.Ptr.is_null_loc loc then return (Usize.(0s), map)
    else
      let* key, entry = Map.find_opt (cast loc) map in
      match entry with
      | Some { address; exposed } when Stdlib.not exposed && expose ->
          let map = Map.syntactic_add key { address; exposed = true } map in
          return (address, map)
      | Some { address; exposed = _ } -> return (address, map)
      | None ->
          let* address = nondet (Typed.t_usize ()) in
          let isize_max = Layout.max_value_z (TInt Isize) in
          let+ () =
            assume
              [
                (address %@ align ==@ Usize.(0s));
                align <=@ address;
                address <@ Typed.BitVec.usize isize_max -!@ size;
              ]
          in
          let map = Map.syntactic_add key { address; exposed = expose } map in
          (address, map)

  let from_exposed (loc_int : [< sint ] Typed.t) (map : t) =
    (* UX: we only consider the first one; this is more or less correct, as per
       the documentation of [with_exposed_provenance]: "The provenance of the
       returned pointer is that of some pointer that was previously exposed"

       See
       https://doc.rust-lang.org/nightly/std/ptr/fn.with_exposed_provenance.html *)
    let usize_ty = Typed.t_usize () in
    let bindings = Map.syntactic_bindings map in
    let binding =
      Typed.iter_vars loc_int
      |> Iter.filter (fun (_, ty) -> Typed.equal_ty usize_ty ty)
      |> Iter.filter_map (fun (var, ty) ->
          let v = Typed.mk_var var ty in
          Seq.find
            (fun (_, { address; exposed }) -> exposed && Typed.equal v address)
            bindings)
      |> Iter.map (fun (loc, { address; _ }) -> (loc, address))
      |> Iter.to_opt
    in
    return (binding, map)
end

module DecayMapMonad = struct
  include
    Soteria.Sym_states.State_monad.Make
      (Rustsymex)
      (struct
        type t = DecayMap.t
      end)

  let not_impl msg = lift @@ not_impl msg
  let of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x
  let match_on xs ~constr = lift @@ match_on xs ~constr
  let get_where () = lift @@ get_trace ()
end

(** The base type of pointers, permitting simple operations on the pointer type.
    The majority of relevant operations are exposed via the state monad's
    pointer module, {!Rust_state_m.S.Sptr}. *)
module type S = sig
  (** pointer type *)
  type t

  val pp : t Fmt.t

  (** Converts an address into a pointer, without provenance. *)
  val of_address : [< sint ] Typed.t -> t

  (** Whether this is the null pointer, meaning it always decays to 0. *)
  val is_null : t -> sbool Typed.t

  (** Whether this pointer has provenance, i.e. points to some allocation. *)
  val has_provenance : t -> sbool Typed.t

  (** If these two pointers have the same provenance, i.e. point to the same
      allocation (or if they both have no provenance). *)
  val have_same_provenance : t -> t -> sbool Typed.t

  (** The distance, in bytes, between two pointers; if they point to different
      allocations, they are decayed and substracted. *)
  val distance : t -> t -> sint Typed.t DecayMapMonad.t

  (** Generates a nondeterministic pointer, that is valid for accesses to the
      given type. The location of the pointer is nondeterministic; it could
      point to any allocation. *)
  val nondet : Types.ty -> (t, [> Error.t ], 'a) Result.t

  (** [offset ?check ?ty ~signed ptr off] Offsets [ptr] by the size of [ty] *
      [off], interpreting [off] as a [signed] integer. [ty] defaults to u8. May
      result in a dangling pointer error if the pointer goes over the allocation
      limit. This check can be disabled with [~check:false]. *)
  val offset :
    ?check:bool ->
    ?ty:Charon.Types.ty ->
    signed:bool ->
    t ->
    [< sint ] Typed.t ->
    (t, [> Error.t ], 'a) Result.t

  (** Decay a pointer into an integer value, losing provenance.
      {b This does not expose the address of the allocation; for that, use
         [expose]} *)
  val decay : t -> [> sint ] Typed.t DecayMapMonad.t

  (** Decay a pointer into an integer value, exposing the address of the
      allocation, allowing it to be retrieved with [DecayMapS.from_exposed]
      later. *)
  val expose : t -> [> sint ] Typed.t DecayMapMonad.t

  (** For Miri: the allocation ID of this location, as a u64 *)
  val as_id : t -> [> sint ] Typed.t

  (** For Miri: get the allocation info for this pointer: its size and alignment
  *)
  val allocation_info : t -> [> sint ] Typed.t * [> nonzero ] Typed.t

  val iter_vars : t -> (Svalue.Var.t * 'b ty -> unit) -> unit
  val subst : (Svalue.Var.t -> Svalue.Var.t) -> t -> t
end
