open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Typed.Infix
open Charon_util
module T = Typed.T

module M (Heap : Heap_intf.S) = struct
  let assert_ _ ~crate:_ ~(args : rust_val list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ Base t ] ->
          of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then Heap.error `FailedAssert state
    else Result.ok (Charon_util.unit_, state)

  let assume _ ~crate:_ ~args ~state =
    let* to_assume =
      match args with
      | [ Base t ] ->
          of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked t Typed.t_int)
      | _ -> not_impl "assume with non-one arguments"
    in
    Fmt.pr "Assuming: %a\n" Typed.ppa to_assume;
    let* () = assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Charon_util.unit_, state)

  let nondet (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty = fun_sig.output in
    let* value = Layout.nondet ty in
    Result.ok (value, state)

  let checked_op op (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral ty :: _ -> return ty
      | ty :: _ ->
          Fmt.kstr not_impl "checked_op with non literal: %a" Types.pp_ty ty
      | [] -> not_impl "checked_op with no inputs"
    in
    let* constrs =
      of_opt_not_impl ~msg:"constraints not implemented" (Layout.constraints ty)
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] ->
          let* left =
            of_opt_not_impl ~msg:"not an integer"
              (Typed.cast_checked left Typed.t_int)
          in
          let+ right =
            of_opt_not_impl ~msg:"not an integer"
              (Typed.cast_checked right Typed.t_int)
          in
          (left, right)
      | _ -> not_impl "checked_op with not two arguments"
    in
    let res = op left right in
    if%sat Typed.conj @@ constrs res then
      Result.ok (Enum (1s, [ Base res ]), state)
    else Result.ok (Enum (0s, []), state)

  let unchecked_op op (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral ty :: _ -> return ty
      | ty :: _ ->
          Fmt.kstr not_impl "checked_op with non literal: %a" Types.pp_ty ty
      | [] -> not_impl "checked_op with no inputs"
    in
    let* constrs =
      of_opt_not_impl ~msg:"constraints not implemented" (Layout.constraints ty)
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] ->
          let* left =
            of_opt_not_impl ~msg:"not an integer"
              (Typed.cast_checked left Typed.t_int)
          in
          let+ right =
            of_opt_not_impl ~msg:"not an integer"
              (Typed.cast_checked right Typed.t_int)
          in
          (left, right)
      | _ -> not_impl "checked_op with not two arguments"
    in
    let res = op left right in
    if%sat Typed.conj @@ constrs res then Result.ok (Base res, state)
    else Heap.error `Overflow state

  let is_some (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* val_ptr =
      match args with
      | [ Base ptr ] ->
          of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked ptr Typed.t_ptr)
      | _ -> not_impl "is_some expects a single ptr argument"
    in
    let* opt_ty =
      match fun_sig.inputs with
      | [ Types.TRef (_, ty, _) ] -> return ty
      | _ -> not_impl "unexpected is_some input type"
    in
    let** enum_value, state = Heap.load val_ptr opt_ty state in
    let* discr =
      match enum_value with
      | Enum (discr, _) -> return discr
      | _ ->
          Fmt.kstr not_impl
            "expected value pointed to in is_some to be an enum, got %a"
            pp_rust_val enum_value
    in
    if%sat discr ==@ 0s then Result.ok (Base 0s, state)
    else Result.ok (Base 1s, state)

  let unwrap _ ~crate:_ ~args ~state =
    let* discr, value =
      match args with
      | [ Enum (disc, value) ] -> return (disc, value)
      | _ -> not_impl "is_some expects a single option argument"
    in
    if%sat discr ==@ 0s then Heap.error (`StdErr "Unwrapped none") state
    else
      match value with
      | [ value ] -> Result.ok (value, state)
      | _ -> not_impl "option is some, but doesn't have one value"

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

  type std_op = Add | Sub | Mul

  type std_fun =
    | Assume
    | Assert
    | Any
    | Checked of std_op
    | Unchecked of std_op
    | IsSome
    | Unwrap

  let std_fun_map =
    [
      ("kani::assert", Assert);
      ("kani::assume", Assume);
      ("kani::any", Any);
      ("core::num::{@N}::checked_add", Checked Add);
      ("core::num::{@N}::unchecked_add", Unchecked Add);
      ("core::num::{@N}::checked_sub", Checked Sub);
      ("core::num::{@N}::unchecked_sub", Unchecked Sub);
      ("core::num::{@N}::checked_mul", Checked Mul);
      ("core::num::{@N}::unchecked_mul", Unchecked Mul);
      ("core::option::{@T}::is_some", IsSome);
      ("core::option::{@T}::unwrap", Unwrap);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  let global_eval ~(crate : UllbcAst.crate) (g : UllbcAst.global_decl) :
      rust_val option =
    let ctx = NameMatcher.ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config g.item_meta.name global_map

  let op_of = function Add -> ( +@ ) | Sub -> ( -@ ) | Mul -> ( *@ )

  let std_fun_eval ~(crate : UllbcAst.crate) (f : UllbcAst.fun_decl) =
    let ctx = NameMatcher.ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config f.item_meta.name std_fun_map
    |> Option.map @@ function
       | Assume -> assume f.signature
       | Assert -> assert_ f.signature
       | Any -> nondet f.signature
       | Checked op -> checked_op (op_of op) f.signature
       | Unchecked op -> unchecked_op (op_of op) f.signature
       | IsSome -> is_some f.signature
       | Unwrap -> unwrap f.signature
end
