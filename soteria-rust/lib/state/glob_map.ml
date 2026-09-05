open Svalue
open Charon
open Compo_res
module SM_Base = Rustsymex

(* HACK: do we need to normalise the generics in some way? e.g. so
   STATIC::<usize> and STATIC::<Id::<usize>> point to the same thing, where
   [type Id<T> = T] *)

type global = String of string | Global of Types.global_decl_ref
[@@deriving show { with_path = false }, ord, eq]

module Key = struct
  type t = global [@@deriving show { with_path = false }, ord]
end

module Abstr = Soteria.Data.Abstr.M (SM_Base)

module Entry =
  Soteria.Sym_states.Excl.Make
    (SM_Base)
    (Abstr.With_syn_of_value (struct
      type ty = Typed.T.sptr_f

      let ty () = Typed.t_ptr_f ()
    end))

include Soteria.Sym_states.Pmap.Concrete (SM_Base) (Key) (Entry)
open SM
open Syntax

let add_assert_new (g : global) (ptr : Typed.([< T.sptr_f ] t)) =
  wrap g (fun prev ->
      assert (Option.is_none prev);
      SM_Base.return (Ok (), Some (ptr :> Typed.(T.sptr_f t))))

let store_str_global str ptr = add_assert_new (String str) ptr
let store_global g ptr = add_assert_new (Global g) ptr

let load g =
  wrap g (fun curr ->
      SM_Base.return
        ( Ok
            (curr
              : Typed.T.sptr_f Typed.t option
              :> Typed.([> T.sptr_f ] t) option),
          curr ))

let load_str_global str = load (String str)
let load_global g = load (Global g)
