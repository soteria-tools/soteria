open Tree_state
module Bi = Soteria.Sym_states.Bi_abd.Make (Rustsymex)
include Bi (Tree_state)
module Sptr = Tree_state.Sptr

let pp_serialized = Tree_state.pp_serialized
let show_serialized = Tree_state.show_serialized
let subst_serialized = Tree_state.subst_serialized
let iter_vars_serialized = Tree_state.iter_vars_serialized
let serialize _ = failwith "Can't serialize Bi_state"

let pp_pretty ~ignore_freed ft t =
  pp' ~inner:(Tree_state.pp_pretty ~ignore_freed) ft t

let empty = None

let[@inline] load ?ignore_borrow ptr ty =
  wrap ~fuel:3 (load ?ignore_borrow ptr ty)

let[@inline] load_discriminant ptr ty = wrap (load_discriminant ptr ty)
let[@inline] store ptr ty v = wrap ~fuel:3 (store ptr ty v)
let[@inline] zeros ptr size = wrap (zeros ptr size)
let[@inline] alloc_ty ?kind ?span ty = wrap (alloc_ty ?kind ?span ty)
let[@inline] alloc_tys ?kind ?span tys = wrap (alloc_tys ?kind ?span tys)

let[@inline] alloc_untyped ?kind ?span ~zeroed ~size ~align =
  wrap (alloc_untyped ?kind ?span ~zeroed ~size ~align)

let[@inline] copy_nonoverlapping ~src ~dst ~size =
  wrap (copy_nonoverlapping ~src ~dst ~size)

let[@inline] uninit ptr ty = wrap (uninit ptr ty)
let[@inline] free ptr = wrap (free ptr)
let[@inline] check_ptr_align ptr ty = wrap (check_ptr_align ptr ty)
let[@inline] borrow ?protect ptr ty = wrap (borrow ?protect ptr ty)
let[@inline] unprotect ptr ty = wrap (unprotect ptr ty)
let[@inline] with_exposed addr = wrap (with_exposed addr)
let[@inline] tb_load ptr ty = wrap (tb_load ptr ty)
let[@inline] load_global g = wrap (load_global g)
let[@inline] store_global g ptr = wrap (store_global g ptr)
let[@inline] load_str_global str = wrap (load_str_global str)
let[@inline] store_str_global str ptr = wrap (store_str_global str ptr)
let[@inline] declare_fn fn = wrap (declare_fn fn)
let[@inline] lookup_fn fn = wrap (lookup_fn fn)
let[@inline] lookup_const_generic id ty = wrap (lookup_const_generic id ty)
let[@inline] add_error e = wrap (add_error e)
let[@inline] leak_check () = wrap (leak_check ())
let[@inline] run_thread_exits () = wrap (run_thread_exits ())
let[@inline] pop_error () = wrap (pop_error ())
let[@inline] with_decay_map x = wrap_no_fail (with_decay_map x)
let[@inline] fake_read ptr ty = wrap_no_fail (fake_read ptr ty)
let[@inline] size_and_align_of_val ty ptr = wrap (size_and_align_of_val ty ptr)
let[@inline] check_non_dangling ptr ty = wrap (check_non_dangling ptr ty)

let[@inline] register_thread_exit
    (f : unit -> (unit, Error.with_trace, serialized list) SM.Result.t) =
  let unlifted () :
      (unit, Error.with_trace, serialized list) Tree_state.SM.Result.t =
   fun st ->
    (* HACK: is this sound ? *)
    let open Rustsymex.Syntax in
    let+ res, st = f () (Some (st, [])) in
    (res, Option.bind st fst)
  in
  wrap (register_thread_exit unlifted)

let to_spec st_opt =
  let st, pre = of_opt st_opt in
  (pre, Option.fold ~none:[] ~some:Tree_state.serialize st)
