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
    L.debug (fun g -> g "Assuming: %a\n" Typed.ppa to_assume);
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

  let wrapping_add (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral (TInteger ty) :: _ -> return ty
      | ty :: _ ->
          Fmt.kstr not_impl "wrapping_add with non integer: %a" Types.pp_ty ty
      | [] -> not_impl "wrapping_add with no inputs"
    in
    let min = Layout.min_value ty in
    let max = Layout.max_value ty in
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
    let res = left +@ right in
    if%sat res >@ max then Result.ok (Base (min +@ (res -@ max)), state)
    else
      if%sat res <@ min then Result.ok (Base (max -@ (min -@ res)), state)
      else Result.ok (Base res, state)

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
    Result.ok (Base (Typed.int_of_bool (discr ==@ 1s)), state)

  let is_none (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* val_ptr =
      match args with
      | [ Base ptr ] ->
          of_opt_not_impl ~msg:"not an integer"
            (Typed.cast_checked ptr Typed.t_ptr)
      | _ -> not_impl "is_none expects a single ptr argument"
    in
    let* opt_ty =
      match fun_sig.inputs with
      | [ Types.TRef (_, ty, _) ] -> return ty
      | _ -> not_impl "unexpected is_none input type"
    in
    let** enum_value, state = Heap.load val_ptr opt_ty state in
    let* discr =
      match enum_value with
      | Enum (discr, _) -> return discr
      | _ ->
          Fmt.kstr not_impl
            "expected value pointed to in is_none to be an enum, got %a"
            pp_rust_val enum_value
    in
    Result.ok (Base (Typed.int_of_bool (discr ==@ 0s)), state)

  let unwrap_opt _ ~crate:_ ~args ~state =
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

  let unwrap_res _ ~crate:_ ~args ~state =
    let* discr, value =
      match args with
      | [ Enum (disc, value) ] -> return (disc, value)
      | _ -> not_impl "is_some expects a single option argument"
    in
    if%sat discr ==@ 1s then Heap.error (`StdErr "Unwrapped Err") state
    else
      match value with
      | [ value ] -> Result.ok (value, state)
      | _ -> not_impl "Result is Ok, but doesn't have one value"

  let eq_values (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let rec aux state left right =
      match (left, right) with
      | Base left, Base right -> Result.ok (left ==@ right, state)
      | Enum (l_d, l_vs), Enum (r_d, r_vs) ->
          if List.compare_lengths l_vs r_vs <> 0 then
            if%sat Typed.not (l_d ==@ r_d) then Result.ok (Typed.v_false, state)
            else
              Fmt.kstr not_impl
                "eq_values: same enum discriminant, but mismatched lengths"
          else
            let disc_match = l_d ==@ r_d in
            let value_pairs = List.combine l_vs r_vs in
            if disc_match = Typed.v_false then Result.ok (Typed.v_false, state)
            else
              Result.fold_list value_pairs ~init:(Typed.v_true, state)
                ~f:(fun (acc, state) (left, right) ->
                  let++ b_val, state = aux state left right in
                  (b_val &&@ acc, state))
      | _ ->
          Fmt.kstr not_impl "Unexpected eq_values pair: %a / %a" pp_rust_val
            left pp_rust_val right
    in

    let* ty =
      match fun_sig.inputs with
      | Types.TRef (_, ty, _) :: _ -> return ty
      | ty :: _ ->
          Fmt.kstr not_impl "Unexpected type for eq_values: %a" Types.pp_ty ty
      | _ -> not_impl "Error: eq_values received no arguments?"
    in
    let left_ptr, right_ptr =
      match args with
      | [ left; right ] ->
          let left = as_base_of ~ty:Typed.t_ptr left in
          let right = as_base_of ~ty:Typed.t_ptr right in
          (left, right)
      | _ -> failwith "eq_values expects two arguments"
    in
    let** left, state = Heap.load left_ptr ty state in
    let** right, state = Heap.load right_ptr ty state in
    let++ b_val, state = aux state left right in
    (Base (Typed.int_of_bool b_val), state)

  let bool_not _ ~crate:_ ~args ~state =
    let* b_ptr =
      match args with
      | [ Base b ] ->
          of_opt_not_impl ~msg:"bool_not expected a pointer"
            (Typed.cast_checked b Typed.t_ptr)
      | _ -> not_impl "bool_not expects one Base argument"
    in
    let** b_rval, state = Heap.load b_ptr (Types.TLiteral TBool) state in
    let b_int = as_base_of ~ty:Typed.t_int b_rval in
    let b_int' = Typed.not_int_bool b_int in
    L.info (fun f ->
        f "bool_not: %a -> %a = %a" Typed.ppa b_ptr Typed.ppa b_int Typed.ppa
          b_int');
    Result.ok (Base b_int', state)

  let zeroed (fun_sig : UllbcAst.fun_sig) ~(crate : UllbcAst.crate) ~args:_
      ~state =
    let rec aux : Types.ty -> rust_val = function
      | TLiteral _ -> Base Typed.zero
      | TAdt (TAdtId t_id, _) -> (
          let adt = Types.TypeDeclId.Map.find t_id crate.type_decls in
          match adt.kind with
          | Struct fields ->
              Struct (List.map (fun (f : Types.field) -> aux f.field_ty) fields)
          | Enum [] -> failwith "zeroed does not handle empty enums!"
          | k ->
              Fmt.failwith "Unhandled zeroed ADT kind: %a"
                Types.pp_type_decl_kind k)
      | ty -> Fmt.failwith "Unhandled zeroed type: %a" Types.pp_ty ty
    in
    try Result.ok (aux fun_sig.output, state) with Failure f -> not_impl f

  let array_repeat ~crate:_ ~args ~state:_ =
    Fmt.kstr not_impl "Args: %a" Fmt.(list pp_rust_val) args

  type std_op = Add | Sub | Mul

  type std_fun =
    | Assume
    | Assert
    | Any
    | Checked of std_op
    | Unchecked of std_op
    | WrappingAdd
    | IsSome
    | IsNone
    | OptUnwrap
    | ResUnwrap
    | Eq
    | BoolNot
    | Zeroed

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
      ("core::num::{@N}::wrapping_add", WrappingAdd);
      ("core::intrinsics::wrapping_add", WrappingAdd);
      ("core::option::{@T}::is_some", IsSome);
      ("core::option::{@T}::is_none", IsNone);
      ("core::option::{@T}::unwrap", OptUnwrap);
      ("core::result::{@T}::unwrap", ResUnwrap);
      ("core::cmp::impls::{core::cmp::PartialEq}::eq", Eq);
      ("core::result::{core::cmp::PartialEq}::eq", Eq);
      ("core::ops::bit::{core::ops::bit::Not}::not", BoolNot);
      ("core::mem::zeroed", Zeroed);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  let op_of = function Add -> ( +@ ) | Sub -> ( -@ ) | Mul -> ( *@ )

  let std_fun_eval ~crate (f : UllbcAst.fun_decl) =
    let ctx = NameMatcher.ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config f.item_meta.name std_fun_map
    |> Option.map @@ function
       | Assume -> assume f.signature
       | Assert -> assert_ f.signature
       | Any -> nondet f.signature
       | Checked op -> checked_op (op_of op) f.signature
       | Unchecked op -> unchecked_op (op_of op) f.signature
       | WrappingAdd -> wrapping_add f.signature
       | IsSome -> is_some f.signature
       | IsNone -> is_none f.signature
       | OptUnwrap -> unwrap_opt f.signature
       | ResUnwrap -> unwrap_res f.signature
       | Eq -> eq_values f.signature
       | BoolNot -> bool_not f.signature
       | Zeroed -> zeroed f.signature

  let builtin_fun_eval ~crate:_ (f : Expressions.builtin_fun_id) =
    match f with
    | ArrayRepeat -> Some array_repeat
    | BoxNew | ArrayToSliceShared | ArrayToSliceMut | Index _ -> None
end
