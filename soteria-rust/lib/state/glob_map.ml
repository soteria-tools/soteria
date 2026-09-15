open Svalue
open Charon
open Compo_res
module SM_Base = Rustsymex

(* HACK: do we need to normalise the generics in some way? e.g. so
   STATIC::<usize> and STATIC::<Id::<usize>> point to the same thing, where
   [type Id<T> = T] *)

(* NOTE: globals are lazily initialised: [load] returns [None] when a global is
   absent, and the interpreter then evaluates the initialiser and allocates a
   block for it. For statics this is not frame preserving: the binding (and its
   block) may be in the frame, so in compositional mode [load] should miss
   instead. For consts and string literals, Rust does not guarantee address
   identity, so allocating a fresh block per use is sound. *)

type global = String of string | Global of Types.global_decl_ref
[@@deriving show { with_path = false }, ord, eq]

module Key = struct
  type t = global [@@deriving show { with_path = false }, ord]
end

module Abstr = Soteria.Data.Abstr.M (SM_Base)

module Entry =
  Soteria.Sym_states.Agree.Make
    (SM_Base)
    (struct
      include Abstr.With_syn_of_value (struct
        type ty = Typed.T.sptr_f

        let ty () = Typed.t_ptr_f ()
      end)

      let sem_eq x y =
        match Typed.cast_checked2 x y with
        | Some (x, y, _) -> Typed.sem_eq x y
        | None ->
            L.failwith "produced global %a disagrees in type with %a" Typed.ppa
              x Typed.ppa y
    end)

include Soteria.Sym_states.Pmap.Concrete (SM_Base) (Key) (Entry)

let add_assert_new (g : global) (ptr : Typed.([< T.sptr_f ] t)) =
  wrap g (fun prev ->
      assert (Option.is_none prev);
      SM_Base.return (Ok (), Some (ptr :> Typed.(T.sptr_f t))))

let store_str_global str ptr = add_assert_new (String str) ptr
let store_global g ptr = add_assert_new (Global g) ptr
let load g = wrap g @@ Entry.load ()
let load_str_global str = load (String str)
let load_global g = load (Global g)
