(** The base type of pointers, permitting simple operations on the pointer type.
    The majority of relevant operations are exposed via the state monad's
    pointer module, {{!State.StateM.S.Sptr}[StateM.Sptr]}. *)

open Rustsymex
open Charon
open Svalue
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

  module SM : sig
    include module type of SM

    val not_impl :
      ?tip:string * string option ->
      ?issue:int ->
      ('a, Format.formatter, unit, 'b t) format4 ->
      'a

    val of_opt_not_impl :
      ?tip:string * string option -> ?issue:int -> string -> 'a option -> 'a t

    val match_on : 'a list -> constr:('a -> sbool Typed.t) -> 'a option t
    val get_where : unit -> Trace.t t
  end

  val empty : t

  (** Decays the given location into an integer, updating the decay map
      accordingly. If [expose] is true, the provenance is marked as exposed, and
      can be retrieved later with [from_exposed]. Returns the decayed integer,
      along with the updated decay map. *)
  val decay :
    expose:bool ->
    size:[< sint ] Typed.t ->
    align:[< nonzero ] Typed.t ->
    sloc Typed.t ->
    sint Typed.t SM.t

  (** Tries finding, for the given integer, the matching provenance in the decay
      map. If found, it returns that provenance, along with the exposed address
      for that allocation at offset 0. Otherwise returns [None]. *)
  val from_exposed :
    [< sint ] Typed.t -> (sloc Typed.t * sint Typed.t) option SM.t
end

module DecayMap : DecayMapS = struct
  module MapKey = struct
    include Typed

    type t = sloc Typed.t
    type syn = Expr.t [@@deriving show { with_path = false }]

    let to_int = unique_tag
    let pp = ppa
    let show = Fmt.to_to_string pp
    let simplify = Rustsymex.simplify
    let fresh _ = L.failwith "Cannot allocate in DecayMap"
    let to_syn = Expr.of_value
    let learn_eq s l = Consumer.learn_eq s l
    let exprs_syn s : Expr.t list = [ s ]
    let subst = Expr.subst
  end

  module Entry = struct
    type 'addr raw = { address : 'addr; exposed : bool }
    [@@deriving show { with_path = false }]

    type t = sint Typed.t raw [@@deriving show { with_path = false }]
    type syn = Expr.t raw [@@deriving show { with_path = false }]

    let fresh () = L.failwith "No fresh for DecayMap.SM.Entry"

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

    let[@inline] not_impl ?tip ?issue fmt =
      Fmt.kstr (fun msg -> lift @@ Rustsymex.not_impl ?tip ?issue "%s" msg) fmt

    let[@inline] of_opt_not_impl ?tip ?issue msg x =
      lift @@ of_opt_not_impl ?tip ?issue msg x

    let[@inline] match_on xs ~constr = lift @@ match_on xs ~constr
    let[@inline] get_where () = lift @@ get_trace ()
  end

  open SM
  open Syntax

  let decay ~expose ~size ~align (loc : [< sloc ] Typed.t) : T.sint Typed.t SM.t
      =
    if%sat Typed.Ptr.is_null_loc loc then return Usize.(0s)
    else
      let* state = get_state () in
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
             Soteria.Stats.As_ctx.incr Rustsymex.StatKeys.decayed_pointers;
             let* address = nondet (Typed.t_usize ()) in
             let isize_max = Layout.max_value_z (TInt Isize) in
             (* Distinct allocations live at distinct addresses. We
                under-approximate this by only requiring the base addresses to
                differ. *)
             let disctinct =
               syntactic_bindings (of_opt state)
               |> Seq.map (fun (_, Entry.{ address; _ }) -> address)
               |> Seq.cons address
               |> Typed.distinct_seq
             in
             let* () =
               assume
                 [
                   (address %@ align ==@ Usize.(0s));
                   align <=@ address;
                   address <@ Typed.BitVec.usize isize_max -!@ size;
                   disctinct;
                 ]
             in
             let* () = set_state (Some { address; exposed = expose }) in
             Result.ok address)
      |> map Compo_res.get_ok

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

type t = Typed.(T.sptr_t t)

let pp = Typed.ppa

(** Creates a dangling pointer to the given type, if that type is a ZST; returns
    [None] otherwise. *)
let dangling_if_zst ty =
  let open Rustsymex in
  let open Syntax in
  let** layout = Layout.layout_of ty in
  if%sat layout.size ==@ Usize.(0s) then
    (* UX: really any address that is well-aligned is valid, we
       under-approximate here to make our life easier. *)
    Result.ok (Some (Typed.Ptr.of_address layout.align))
  else Result.ok None

(** A simplified, untyped (and {b unsafe}) version of [offset], that adds a
    signed integer to this pointer's offset. This offset doesn't check whether
    the resulting pointer is dangling after being offset. *)
let raw_offset ptr off_by =
  let open Rustsymex.Syntax in
  let _, ovf = Typed.Ptr.ofs ptr +$?@ off_by in
  let++ () = assert_or_error (Typed.not ovf) `UBDanglingPointer in
  Typed.Ptr.add_ofs ptr off_by

let[@inline] _decay ~expose p =
  let open DecayMap.SM.Syntax in
  let size = Typed.Ptr.size_of p in
  let align = Typed.Ptr.align_of p in
  let loc = Typed.Ptr.loc p in
  let ofs = Typed.Ptr.ofs p in
  let+ loc_int = DecayMap.decay ~expose ~size ~align loc in
  [%l.debug "Decay %a -> %a" Typed.ppa loc Typed.ppa loc_int];
  loc_int +!!@ ofs

(** Decay a pointer into an integer value, losing provenance.
    {b This does not expose the address of the allocation; for that, use
       {!expose}} *)
let decay p = _decay ~expose:false p

(** Decay a pointer into an integer value, exposing the address of the
    allocation, allowing it to be retrieved with {!DecayMap.from_exposed} later.
*)
let expose p = _decay ~expose:true p

(** The distance, in bytes, between two pointers; if they point to different
    allocations, they are decayed and substracted. *)
let distance p1 p2 : [> sint ] Typed.t DecayMap.SM.t =
  let open DecayMap.SM.Syntax in
  if%sat Typed.Ptr.have_same_provenance p1 p2 then
    DecayMap.SM.return (Typed.Ptr.ofs p1 -!@ Typed.Ptr.ofs p2)
  else
    let* ptr1 = decay p1 in
    let+ ptr2 = decay p2 in
    ptr1 -!@ ptr2
