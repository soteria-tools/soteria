open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Charon_util
module T = Typed.T

module M (Heap : Heap_intf.S) = struct
  let assert_ _ ~crate:_ ~(args : rust_val list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ Base t ] ->
          Rustsymex.of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then Heap.error `FailedAssert state
    else Result.ok (Charon_util.unit_, state)

  let assume _ ~crate:_ ~args ~state =
    let* to_assume =
      match args with
      | [ Base t ] ->
          Rustsymex.of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "assume with non-one arguments"
    in
    Fmt.pr "Assuming: %a\n" Typed.ppa to_assume;
    let* () = Rustsymex.assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Charon_util.unit_, state)

  let nondet (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty = fun_sig.output in
    let* value = Layout.nondet ty in
    Result.ok (value, state)

  let global_map =
    let open Values in
    [
      ("core::num::{usize}::MIN", Base (Layout.min_value Usize));
      ("core::num::{usize}::MAX", Base (Layout.max_value Usize));
      ("core::num::{u8}::MIN", Base (Layout.min_value U8));
      ("core::num::{u8}::MAX", Base (Layout.max_value U8));
      ("core::num::{u16}::MIN", Base (Layout.min_value U16));
      ("core::num::{u16}::MAX", Base (Layout.max_value U16));
      ("core::num::{u32}::MIN", Base (Layout.min_value U32));
      ("core::num::{u32}::MAX", Base (Layout.max_value U32));
      ("core::num::{u64}::MIN", Base (Layout.min_value U64));
      ("core::num::{u64}::MAX", Base (Layout.max_value U64));
      ("core::num::{u128}::MIN", Base (Layout.min_value U128));
      ("core::num::{u128}::MAX", Base (Layout.max_value U128));
      ("core::num::{isize}::MIN", Base (Layout.min_value Isize));
      ("core::num::{isize}::MAX", Base (Layout.max_value Isize));
      ("core::num::{i8}::MIN", Base (Layout.min_value I8));
      ("core::num::{i8}::MAX", Base (Layout.max_value I8));
      ("core::num::{i16}::MIN", Base (Layout.min_value I16));
      ("core::num::{i16}::MAX", Base (Layout.max_value I16));
      ("core::num::{i32}::MIN", Base (Layout.min_value I32));
      ("core::num::{i32}::MAX", Base (Layout.max_value I32));
      ("core::num::{i64}::MIN", Base (Layout.min_value I64));
      ("core::num::{i64}::MAX", Base (Layout.max_value I64));
      ("core::num::{i128}::MIN", Base (Layout.min_value I128));
      ("core::num::{i128}::MAX", Base (Layout.max_value I128));
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  type std_fun = Assume | Assert | Any

  let std_fun_map =
    [ ("kani::assert", Assert); ("kani::assume", Assume); ("kani::any", Any) ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  let global_eval ~(crate : UllbcAst.crate) (g : UllbcAst.global_decl) :
      rust_val option =
    let ctx = NameMatcher.ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config g.item_meta.name global_map

  let std_fun_eval ~(crate : UllbcAst.crate) (f : UllbcAst.fun_decl) =
    let ctx = NameMatcher.ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config f.item_meta.name std_fun_map
    |> Option.map @@ function
       | Assume -> assume f.signature
       | Assert -> assert_ f.signature
       | Any -> nondet f.signature
end
