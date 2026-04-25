open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
module SMap = Map.Make (String)

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

(* Functions we could not stub, but we do for performance *)
type fixme_fn = PromiseAlignment

(* Soteria builtin functions *)
type soteria_fn = Assert | Assume | NondetBytes | Panic
type fn = Fixme of fixme_fn | Soteria of soteria_fn

type extern_fn = Alloc of alloc_fn | Miri of miri_fn | Panic of panic_fn

and alloc_fn =
  | Alloc of { zeroed : bool }
  | Dealloc
  | Realloc
  | NoAllocShimIsUnstable

and miri_fn = Alloc | AllocId | Dealloc | Nop
and panic_fn = PanicCleanup

let extern_functions =
  [
    (* Allocator *)
    ("__rust_alloc", Alloc (Alloc { zeroed = false }));
    ("__rust_alloc_zeroed", Alloc (Alloc { zeroed = true }));
    ("__rust_dealloc", Alloc Dealloc);
    ("__rust_no_alloc_shim_is_unstable_v2", Alloc NoAllocShimIsUnstable);
    ("__rust_realloc", Alloc Realloc);
    (* Miri builtins *)
    ("miri_get_alloc_id", Miri AllocId);
    ("miri_pointer_name", Miri Nop);
    ("miri_print_borrow_state", Miri Nop);
    ("miri_run_provenance_gc", Miri Nop);
    ("miri_write_to_stdout", Miri Nop);
    ("miri_alloc", Miri Alloc);
    ("miri_dealloc", Miri Dealloc);
    (* Panics *)
    ("__rust_panic_cleanup", Panic PanicCleanup);
  ]
  |> SMap.of_list

let std_fun_pair_list =
  [
    (* Soteria builtins *)
    ("soteria::assert", Soteria Assert);
    ("soteria::assume", Soteria Assume);
    ("soteria::nondet_bytes", Soteria NondetBytes);
    ("soteria::panic", Soteria Panic);
    (* Kani builtins -- we re-define these for nicer call traces *)
    ("kani::assert", Soteria Assert);
    ("kani::assume", Soteria Assume);
    ("kani::panic", Soteria Panic);
    (* Miri builtins *)
    (* HACK: this should be handled with intrinsics. *)
    ("std::intrinsics::miri_promise_symbolic_alignment", Fixme PromiseAlignment);
  ]

let opaque_names = List.map fst std_fun_pair_list

let std_fun_map =
  std_fun_pair_list
  |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
  |> NameMatcherMap.of_list

module M (StateM : State.StateM.S) = struct
  module Alloc = Alloc.M (StateM)
  module Intrinsics = Intrinsics.M (StateM)
  module Miri = Miri.M (StateM)
  module Soteria_lib = Soteria_lib.M (StateM)
  module Std = Std.M (StateM)
  module Optim_name = Optim.M (StateM)
  module System_name = System.M (StateM)
  module Fixme_name = Fixme.M (StateM)

  let fn_to_stub fn_sig _fn_name _fun_exec = function
    | Soteria Assert -> Soteria_lib.assert_
    | Soteria Assume -> Soteria_lib.assume
    | Soteria NondetBytes -> Soteria_lib.nondet_bytes fn_sig
    | Soteria Panic -> Soteria_lib.panic ?msg:None
    | Fixme PromiseAlignment -> Miri.promise_alignement

  let extern_fn_to_stub = function
    | Alloc (Alloc { zeroed }) -> Alloc.alloc ~zeroed
    | Alloc Dealloc -> Alloc.dealloc
    | Alloc NoAllocShimIsUnstable -> Alloc.no_alloc_shim_is_unstable
    | Alloc Realloc -> Alloc.realloc
    | Miri Alloc -> Miri.alloc
    | Miri AllocId -> Miri.alloc_id
    | Miri Dealloc -> Miri.dealloc
    | Miri Nop -> Miri.nop
    | Panic PanicCleanup -> Std.panic_cleanup

  let eval_stub (f : UllbcAst.fun_decl) fun_exec generics =
    (* Try the generated stub modules first, in priority order. *)
    let generated =
      List.find_map Fun.id
        [
          Optim_name.eval_fun f fun_exec generics;
          System_name.eval_fun f fun_exec generics;
          Fixme_name.eval_fun f fun_exec generics;
        ]
    in
    match generated with
    | Some _ -> generated
    | None ->
        (* Fall back to the small set of soteria/kani/miri manual builtins. *)
        let name = f.item_meta.name in
        let ctx = Crate.as_namematcher_ctx () in
        let stub = NameMatcherMap.find_opt ctx match_config name std_fun_map in
        Option.map
          (fun stub args -> fn_to_stub f.signature name fun_exec stub args)
          stub

  let eval_intrinsic (f : UllbcAst.fun_decl) name generics fun_exec =
    (* In the case of monomorphised code, the generics will be empty but present
       in the name; we need to get them there. *)
    let generics =
      match List.last_opt f.item_meta.name with
      | Some (PeInstantiated mono) -> mono.binder_value
      | _ -> generics
    in
    Intrinsics.eval_fun name fun_exec generics

  let eval_extern name =
    match SMap.find_opt name extern_functions with
    | Some extern_fn -> extern_fn_to_stub extern_fn
    | None ->
        fun _args ->
          Fmt.kstr StateM.not_impl "Extern function %s is not handled" name
end
