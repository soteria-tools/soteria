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
  include Soteria.Sym_states.Base.M(Rustsymex).S

  val empty : t

  (** Decays the given location into an integer, updating the decay map
      accordingly. If [expose] is true, the provenance is marked as exposed, and
      can be retrieved later with [from_exposed]. Returns the decayed integer,
      along with the updated decay map. *)
  val decay :
    expose:bool ->
    size:[< sint ] Typed.t ->
    align:[< nonzero ] Typed.t ->
    [< sloc ] Typed.t ->
    sint Typed.t SM.t

  (** Tries finding, for the given integer, the matching provenance in the decay
      map. If found, it returns that provenance, along with the exposed address
      for that allocation at offset 0. Otherwise returns [None]. *)
  val from_exposed :
    [< sint ] Typed.t -> (sloc Typed.t * sint Typed.t) option SM.t
end

module DecayMap = struct
  module MapKey = struct
    include Typed

    type t = sloc Typed.t
    type syn = Expr.t [@@deriving show { with_path = false }]

    let to_int = unique_tag
    let pp = ppa
    let show = Fmt.to_to_string pp
    let simplify = Rustsymex.simplify
    let fresh _ = failwith "Cannot allocate in DecayMap"
    let to_syn = Expr.of_value
    let learn_eq s l = Consumer.learn_eq s l
    let exprs_syn s : Expr.t list = [ s ]
    let subst = Expr.subst
  end

  module Entry = struct
    type t = { address : sint Typed.t; exposed : bool }
    [@@deriving show { with_path = false }]

    type syn = { address : Expr.t; exposed : bool }
    [@@deriving show { with_path = false }]

    let fresh () = failwith "No fresh for DecayMap.SM.Entry"

    let to_syn ({ address; exposed } : t) =
      { address = Expr.of_value address; exposed }

    let sem_eq (s1 : t) (s2 : t) =
      Typed.of_bool (s1.exposed = s2.exposed) &&@ (s1.address ==@ s2.address)

    let learn_eq (s : syn) (st : t) =
      if s.exposed <> st.exposed then Consumer.lfail Typed.v_false
      else Consumer.learn_eq s.address st.address

    let exprs_syn ({ address; exposed = _ } : syn) : Expr.t list = [ address ]

    let subst s ({ address; exposed } : syn) : t =
      { address = Expr.subst s address; exposed }
  end

  module EntryExcl = Soteria.Sym_states.Agree.Make (Rustsymex) (Entry)

  include
    Soteria.Sym_states.Pmap.Direct_access_patricia_tree (Rustsymex) (MapKey)
      (EntryExcl)

  module SM = struct
    include SM

    let[@inline] not_impl msg = lift @@ not_impl msg
    let[@inline] of_opt_not_impl msg x = lift @@ of_opt_not_impl msg x
    let[@inline] match_on xs ~constr = lift @@ match_on xs ~constr
    let[@inline] get_where () = lift @@ get_trace ()
  end

  open SM
  open Syntax

  let decay ~expose ~size ~align (loc : [< sloc ] Typed.t) : T.sint Typed.t SM.t
      =
    if%sat Typed.Ptr.is_null_loc loc then return Usize.(0s)
    else
      wrap loc
        (let open EntryExcl.SM in
         let open Syntax in
         let* entry = get_state () in
         match entry with
         | Some { address; exposed } when Stdlib.not exposed && expose ->
             let* () = set_state (Some { address; exposed = true }) in
             Result.ok address
         | Some { address; exposed = _ } -> Result.ok address
         | None ->
             let* address = nondet (Typed.t_usize ()) in
             let isize_max = Layout.max_value_z (TInt Isize) in
             let* () =
               assume
                 [
                   (address %@ align ==@ Usize.(0s));
                   align <=@ address;
                   address <@ Typed.BitVec.usize isize_max -!@ size;
                 ]
             in
             let* () = set_state (Some { address; exposed = expose }) in
             Result.ok address)
      |> Fun.flip map Soteria.Symex.Compo_res.get_ok

  let from_exposed (loc_int : [< sint ] Typed.t) :
      (sloc Typed.t * sint Typed.t) option SM.t =
    (* UX: we only consider the first one; this is more or less correct, as per
       the documentation of [with_exposed_provenance]: "The provenance of the
       returned pointer is that of some pointer that was previously exposed"

       See
       https://doc.rust-lang.org/nightly/std/ptr/fn.with_exposed_provenance.html *)
    let usize_ty = Typed.t_usize () in
    let+ map = get_state () in
    let bindings = syntactic_bindings (of_opt map) in
    Typed.iter_vars loc_int
    |> Iter.filter (fun (_, ty) -> Typed.equal_ty usize_ty ty)
    |> Iter.filter_map (fun (var, _) ->
        let v = Typed.mk_var var usize_ty in
        Seq.find
          (fun (_, ({ address; exposed } : Entry.t)) ->
            exposed && Typed.equal v address)
          bindings)
    |> Iter.map (fun (loc, ({ address; _ } : Entry.t)) -> (loc, address))
    |> Iter.to_opt
end

module D_abstr = Soteria.Data.Abstr.M (DecayMap.SM)

(** The base type of pointers, permitting simple operations on the pointer type.
    The majority of relevant operations are exposed via the state monad's
    pointer module, {!Rust_state_m.S.Sptr}. *)
module type S = sig
  (** pointer type *)
  type t [@@mixins D_abstr.S_with_syn]

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
  val distance : t -> t -> sint Typed.t DecayMap.SM.t

  (** Generates a nondeterministic pointer, that is valid for accesses to the
      given type. The location of the pointer is nondeterministic; it could
      point to any allocation. *)
  val nondet : Types.ty -> (t, [> Error.t ], 'a) Result.t

  (** Decay a pointer into an integer value, losing provenance.
      {b This does not expose the address of the allocation; for that, use
         [expose]} *)
  val decay : t -> [> sint ] Typed.t DecayMap.SM.t

  (** Decay a pointer into an integer value, exposing the address of the
      allocation, allowing it to be retrieved with [DecayMapS.from_exposed]
      later. *)
  val expose : t -> [> sint ] Typed.t DecayMap.SM.t

  (** For Miri: the allocation ID of this location, as a u64 *)
  val as_id : t -> [> sint ] Typed.t

  (** For Miri: get the allocation info for this pointer: its size and alignment
  *)
  val allocation_info : t -> [> sint ] Typed.t * [> nonzero ] Typed.t
end
