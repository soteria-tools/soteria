open Svalue
open Charon
open Compo_res
module SM_Base = Rustsymex

(* HACK: do we need to normalise the type? e.g. so usize and Id::<usize> have
   the same TypeID, where [type Id<T> = T] *)

module Key = struct
  type t = Types.ty [@@deriving show { with_path = false }, ord]
end

module Abstr = Soteria.Data.Abstr.M (SM_Base)

module Entry =
  Soteria.Sym_states.Excl.Make
    (SM_Base)
    (Abstr.With_syn_of_value (struct
      type ty = Typed.T.sint

      let ty () = Typed.t_int U128
    end))

include Soteria.Sym_states.Pmap.Concrete (SM_Base) (Key) (Entry)
open SM
open Syntax

let get_type_id (ty : Types.ty) =
  let* st = get_state () in
  let st = of_opt st in
  wrap ty (function
    | Some type_id as prev -> SM_Base.return (Ok type_id, prev)
    | None ->
        let open Rustsymex in
        let open Syntax in
        (* the identifier of a type is opaque; all we know is that distinct
           types have distinct identifiers. *)
        let* id = nondet (Typed.t_lit (TUInt U128)) in
        let distinct =
          syntactic_bindings st
          |> Seq.map snd
          |> Seq.cons id
          |> Typed.distinct_seq
        in
        let+ () = assume [ distinct ] in
        (Ok id, Some id))
