open Svalue
open Typed.Infix
open Typed.Syntax
open Compo_res
module SM_Base = Rustsymex

(* HACK: do we need to normalise the function declaration? *)

module Abstr = Soteria.Data.Abstr.M (SM_Base)

module Key = struct
  include Abstr.With_syn_of_value (struct
    type ty = Typed.T.sloc

    let ty () = Typed.t_loc ()
  end)

  let compare = Typed.compare
  let simplify = SM_Base.simplify
  let distinct_seq = Typed.distinct_seq
  let to_int = Typed.unique_tag
end

module Entry =
  Soteria.Sym_states.Agree.Make
    (SM_Base)
    (struct
      include Common.Fun_kind

      type syn = t [@@deriving show]

      let fresh () = failwith "fresh on Functions_map.Entry"
      let to_syn f = f
      let subst _ f = f
      let exprs_syn _ = []
      let sem_eq x y = Typed.of_bool (equal x y)

      let learn_eq syn f =
        if equal syn f then SM_Base.Consumer.ok ()
        else SM_Base.Consumer.lfail Typed.v_false
    end)

include Soteria.Sym_states.Pmap.Make_patricia_tree (SM_Base) (Key) (Entry)
open SM
open Syntax

let lookup_fn ptr =
  let ptr = Typed.Ptr.ptr_of ptr in
  let loc, ofs = Typed.Ptr.decompose ptr in
  let** () = assert_or_error (ofs ==@ Usize.(0s)) `MisalignedFnPointer in
  wrap loc
    (let open Entry.SM in
     let open Syntax in
     let* st = get_state () in
     match (st, Config.get_mode ()) with
     | Some fn, _ -> Result.ok fn
     | None, Whole_program -> Result.error `NotAFnPointer
     | None, Compositional ->
         (* FIXME: the binding may be in the frame, so we can't error. this
            should be a miss with fix [(loc, ?fn)], which we can't produce since
            [Fun_kind.t] is not a symbolic value. *)
         Entry.SM.lift
         @@ Rustsymex.not_impl
              "tried looking up a function pointer that is not known.")

let lookup_fn_loc fn_ref =
  let* fns = get_state () in
  let fns = of_opt fns in
  syntactic_bindings fns
  |> Seq.find_map (fun (loc, fn) ->
      if Common.Fun_kind.equal fn fn_ref then Some loc else None)
  |> Result.ok

let declare_fn_at loc fn_ref =
  wrap loc (function
    | None -> SM_Base.return (Ok (), Some fn_ref)
    | Some _ -> L.failwith "re-declared a function over a pre-existing location")
