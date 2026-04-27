open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
module SMap = Map.Make (String)

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

(** Functions we stub *)
type fn =
  | Soteria of Soteria_lib.fn
  | Optim of Optim.fn
  | System of System.fn
  | Fixme of Fixme.fn
  | Tokio of Tokio.fn

(** Extern functions we must implement manually *)
type extern_fn =
  | Alloc of Extern.Alloc.fn
  | Miri of Extern.Miri.fn
  | Std of Extern.Std.fn

let extern_functions =
  (Extern.Alloc.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> Alloc f)
  @ (Extern.Miri.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> Miri f)
  @ (Extern.Std.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> Std f)
  |> SMap.of_list

let std_fun_pair_list =
  (Soteria_lib.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> Soteria f)
  @ (Optim.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> Optim f)
  @ (System.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> System f)
  @ (Fixme.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> Fixme f)
  @ (Tokio.fn_pats |> List.map @@ Pair.map_snd @@ fun f -> Tokio f)

let opaque_names = List.map fst std_fun_pair_list

let std_fun_map =
  std_fun_pair_list
  |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
  |> NameMatcherMap.of_list

module M (StateM : State.StateM.S) = struct
  (* intrinsics *)
  module Intrinsics = Intrinsics.M (StateM)

  (* externs *)
  module Alloc = Extern.Alloc.M (StateM)
  module Miri = Extern.Miri.M (StateM)
  module Std = Extern.Std.M (StateM)

  (* stubs *)
  module Soteria_lib = Soteria_lib.M (StateM)
  module Optim = Optim.M (StateM)
  module System = System.M (StateM)
  module Tokio = Tokio.M (StateM)
  module Fixme = Fixme.M (StateM)

  let fn_to_stub fun_sig fun_exec generics = function
    | Soteria f -> Soteria_lib.fn_to_stub f fun_sig fun_exec generics
    | Fixme f -> Fixme.fn_to_stub f fun_sig fun_exec generics
    | Optim f -> Optim.fn_to_stub f fun_sig fun_exec generics
    | System f -> System.fn_to_stub f fun_sig fun_exec generics
    | Tokio f -> Tokio.fn_to_stub f fun_sig fun_exec generics

  let[@inline] extern_fn_to_stub = function
    | Alloc f -> Alloc.fn_to_stub f
    | Miri f -> Miri.fn_to_stub f
    | Std f -> Std.fn_to_stub f

  let get_generics (f : UllbcAst.fun_decl) generics =
    (* In the case of monomorphised code, the generics will be empty but present
       in the name; we need to get them there. *)
    match List.last_opt f.item_meta.name with
    | Some (PeInstantiated mono) -> mono.binder_value
    | _ -> generics

  let eval_stub (f : UllbcAst.fun_decl) fun_exec generics =
    let name = f.item_meta.name in
    let ctx = Crate.as_namematcher_ctx () in
    let generics = get_generics f generics in
    NameMatcherMap.find_opt ctx match_config name std_fun_map
    |> Option.map (fn_to_stub f.signature fun_exec generics)

  let eval_intrinsic (f : UllbcAst.fun_decl) name generics fun_exec =
    let generics = get_generics f generics in
    Intrinsics.eval_fun name fun_exec generics

  let eval_extern name =
    match SMap.find_opt name extern_functions with
    | Some extern_fn -> extern_fn_to_stub extern_fn
    | None ->
        fun _args ->
          Fmt.kstr StateM.not_impl "Extern function %s is not handled" name
end
