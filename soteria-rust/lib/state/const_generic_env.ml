open Svalue
open Charon
open Compo_res
module SM_Base = Rustsymex

(* TODO: we should move the symbolic layout cache out of Rustsymex into here,
   and have a [Typed.type_var_id -> Layout] map to store the layout of generic
   types *)

module Key = struct
  type t = Types.const_generic_var_id
  [@@deriving show { with_path = false }, ord]
end

module Abstr = Soteria.Data.Abstr.M (SM_Base)

module Entry =
  Soteria.Sym_states.Excl.Make
    (SM_Base)
    (Abstr.With_syn_of_value (struct
      type ty = Typed.T.any

      let ty () = L.failwith "cannot instantiate type 'any'"
    end))

include Soteria.Sym_states.Pmap.Concrete (SM_Base) (Key) (Entry)
open SM
open Syntax

let lookup_const_generic id ty =
  wrap id
    (let open Entry.SM in
     let open Syntax in
     let* st = get_state () in
     match st with
     | Some v -> Result.ok (Typed.as_any v)
     | None ->
         let**^ v = Value_codec.nondet_valid ty in
         let* () = set_state (Some v) in
         Result.ok (Typed.as_any v))
