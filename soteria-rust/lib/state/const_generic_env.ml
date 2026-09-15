open Svalue
open Charon
module SM_Base = Rustsymex

(* TODO: we should move the symbolic layout cache out of Rustsymex into here,
   and have a [Typed.type_var_id -> Layout] map to store the layout of generic
   types *)

(* NOTE: the const generic IDs used here are only relevant for the environment
   of the current analysed function. In compositional mode, one must be careful
   to remap and unify the const generic arguments properly. For example: [rust{

   fn foo<const C1: u32, const C2: u32>() -> (u32, u32) { (C1, C2) }

   fn bar<const C: u32>() -> (u32, u32) { foo::<C, C>() }

   }] *)

module Key = struct
  type t = Types.const_generic_var_id
  [@@deriving show { with_path = false }, ord]
end

module Abstr = Soteria.Data.Abstr.M (SM_Base)

module Entry =
  Soteria.Sym_states.Agree.Make
    (SM_Base)
    (struct
      include Abstr.With_syn_of_value (struct
        type ty = Typed.T.any

        (* NOTE: [any] has no runtime type, so [fresh] -- and thus
           [Entry.load]/[Entry.unwrap] -- cannot be used here; they raise. *)
        let ty () = L.failwith "cannot instantiate type 'any'"
      end)

      (* [sem_eq_untyped] is [false] when the runtime types differ, so
         [Agree.produce] would silently vanish the path on a type mismatch,
         dropping a behaviour. A mismatch is a bug, not a behaviour: be loud. *)
      let sem_eq x y =
        match Typed.cast_checked2 x y with
        | Some (x, y, _) -> Typed.sem_eq x y
        | None ->
            L.failwith "produced const generic %a disagrees in type with %a"
              Typed.ppa x Typed.ppa y
    end)

include Soteria.Sym_states.Pmap.Concrete (SM_Base) (Key) (Entry)

let lookup_const_generic id ty =
  wrap id
    (let open Entry.SM in
     let open Syntax in
     let* st = get_state () in
     match st with
     | Some v -> Result.ok (Typed.as_any v)
     | None ->
         (* FIXME: this is not sound in compositional reasoning: the binding may
            be in the frame, so this should be a miss with fix [(id, ?v)]. That
            fix is expressible -- [Agree.unwrap] isn't usable, as [fresh] has no
            type to instantiate at [T.any], but the parameter's declared type is
            known, so [Result.miss [ Entry.to_syn v ]] does the job. *)
         let**^ v = Value_codec.nondet_valid ty in
         let* () = set_state (Some v) in
         Result.ok (Typed.as_any v))
