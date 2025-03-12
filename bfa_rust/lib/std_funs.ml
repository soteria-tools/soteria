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
    let val_ptr, opt_ty =
      match (args, fun_sig.inputs) with
      | [ Ptr ptr ], [ Types.TRef (_, ty, _) ] -> (ptr, ty)
      | _ ->
          failwith
            "is_some expects a single ptr argument and a single tref input type"
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
    let val_ptr, opt_ty =
      match (args, fun_sig.inputs) with
      | [ Ptr ptr ], [ Types.TRef (_, ty, _) ] -> (ptr, ty)
      | _ ->
          failwith
            "is_none expects a single ptr argument and a single tref input type"
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

  let eq_values ~neg (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
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
    let left_ptr, right_ptr =
      match args with
      | [ Ptr left; Ptr right ] -> (left, right)
      | _ -> failwith "eq_values expects two arguments"
    in
    let** left_ptr, right_ptr, ty, state =
      match fun_sig.inputs with
      | Types.TRef (_, (Types.TRef (_, ty, _) as outer_ty), _) :: _ ->
          (* STD provides an implementation of eq for references (&T), where the arguments are
             thus &&T -- we handle this here by adding an indirection. *)
          let** left, state = Heap.load left_ptr outer_ty state in
          let** right, state = Heap.load right_ptr outer_ty state in
          let left_ptr = as_ptr left in
          let right_ptr = as_ptr right in
          Result.ok (left_ptr, right_ptr, ty, state)
      | Types.TRef (_, ty, _) :: _ -> Result.ok (left_ptr, right_ptr, ty, state)
      | ty :: _ ->
          Fmt.kstr not_impl "Unexpected type for eq_values: %a" Types.pp_ty ty
      | _ -> not_impl "Error: eq_values received no arguments?"
    in
    let** left, state = Heap.load left_ptr ty state in
    let** right, state = Heap.load right_ptr ty state in
    let++ b_val, state = aux state left right in
    let b_val = if neg then Typed.not b_val else b_val in
    let res = Typed.int_of_bool b_val in
    (Base res, state)

  let bool_not _ ~crate:_ ~args ~state =
    let b_ptr =
      match args with
      | [ Ptr b ] -> b
      | _ -> failwith "bool_not expects one Ptr argument"
    in
    let** b_rval, state = Heap.load b_ptr (Types.TLiteral TBool) state in
    let b_int = as_base_of ~ty:Typed.t_int b_rval in
    let b_int' = Typed.not_int_bool b_int in
    Result.ok (Base b_int', state)

  let zeroed (fun_sig : UllbcAst.fun_sig) ~(crate : UllbcAst.crate) ~args:_
      ~state =
    let rec aux : Types.ty -> rust_val = function
      | TLiteral _ -> Base Typed.zero
      | TRawPtr _ | TRef _ -> Ptr Typed.Ptr.null
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

  let array_repeat (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let rust_val, size =
      match (args, gen_args.const_generics) with
      | [ rust_val ], [ size ] ->
          (rust_val, Charon_util.int_of_const_generic size)
      | args, cgens ->
          Fmt.failwith
            "array_repeat: unexpected params / generic constants: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            cgens
    in
    Result.ok (Array (List.init size (fun _ -> rust_val)), state)

  let array_index (idx_op : Expressions.builtin_index_op)
      (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let ptr, size =
      match (idx_op.is_array, args, gen_args.const_generics) with
      (* Array with static size *)
      | true, Ptr ptr :: _, [ size ] ->
          (ptr, Typed.int @@ Charon_util.int_of_const_generic size)
      | false, FatPtr (ptr, size) :: _, [] -> (ptr, Typed.cast size)
      | _ ->
          Fmt.failwith "array_index: unexpected arguments: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            gen_args.const_generics
    in
    (* TODO: take into account idx.mutability *)
    let idx = as_base_of ~ty:Typed.t_int (List.nth args 1) in
    if%sat 0s <=@ idx &&@ (idx <@ size) then
      let ty = List.hd gen_args.types in
      let* offset = Layout.size_of_s ty in
      let ptr_loc, ptr_off = (Typed.Ptr.loc ptr, Typed.Ptr.ofs ptr) in
      let ptr_off' = ptr_off +@ (offset *@ idx) in
      let ptr' = Typed.Ptr.mk ptr_loc ptr_off' in
      if not idx_op.is_range then Result.ok (Ptr ptr', state)
      else
        let range_end = as_base_of ~ty:Typed.t_int (List.nth args 2) in
        (* range_end is exclusive *)
        if%sat idx <=@ range_end &&@ (range_end <=@ size) then
          let size = range_end -@ idx in
          Result.ok (FatPtr (ptr', size), state)
        else
          (* not sure this is the right diagnostic *)
          Heap.error `OutOfBounds state
    else Heap.error `OutOfBounds state

  (* Some array accesses are ran on functions, so we handle those here and redirect them.
     Eventually, it would be good to maybe make a Charon pass that gets rid of these before. *)
  let array_index_fn (fun_sig : UllbcAst.fun_sig) ~crate ~args ~state =
    match (args, fun_sig.inputs) with
    (* Unfortunate, but right now i don't have a better way to handle this *)
    | ( [ ptr; Struct [ idx_from; idx_to ] ],
        TRef (_, TAdt (TBuiltin ((TArray | TSlice) as mode), gargs), _) :: _ )
      ->
        let idx_op : Expressions.builtin_index_op =
          { is_array = mode = TArray; mutability = RShared; is_range = true }
        in
        array_index idx_op gargs ~crate ~args:[ ptr; idx_from; idx_to ] ~state
    | _ -> failwith "array_index (fn): unexpected arguments"

  let array_slice ~mut:_ (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let size =
      match gen_args.const_generics with
      | [ size ] -> Charon_util.int_of_const_generic size
      | _ -> failwith "array_slice: unexpected generic constants"
    in
    let arr_ptr =
      match args with
      | [ Ptr arr_ptr ] -> arr_ptr
      | _ -> failwith "array_index: unexpected arguments"
    in
    let slice = FatPtr (arr_ptr, Typed.int size) in
    Result.ok (slice, state)

  let slice_len _ ~crate:_ ~args ~state =
    match args with
    | [ FatPtr (_, len) ] -> Result.ok (Base len, state)
    | _ -> failwith "slice_len: unexpected arguments"

  let discriminant_value (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let value_ptr =
      match args with
      | [ Ptr value_ptr ] -> value_ptr
      | _ -> failwith "discriminant_value: unexpected arguments"
    in
    let value_ty =
      match funsig.inputs with
      | [ TRef (_, value_ty, _) ] -> value_ty
      | _ -> failwith "discriminant_value: unexpected arguments"
    in
    let++ value, state = Heap.load value_ptr value_ty state in
    match value with
    | Enum (discr, _) -> (Base discr, state)
    | _ -> failwith "discriminant_value: unexpected value"

  type std_op = Add | Sub | Mul
  type std_bool = Id | Neg

  type std_fun =
    | Any
    | Assert
    | Assume
    | BoolNot
    | Checked of std_op
    | DiscriminantValue
    | Eq of std_bool
    | Index
    | IsNone
    | IsSome
    | OptUnwrap
    | ResUnwrap
    | SliceLen
    | Unchecked of std_op
    | WrappingAdd
    | Zeroed

  let std_fun_map =
    [
      (* Kani *)
      ("kani::assert", Assert);
      ("kani::assume", Assume);
      ("kani::any", Any);
      (* Core *)
      ("core::array::{core::ops::index::Index}::index", Index);
      ("core::cmp::impls::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::cmp::impls::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::intrinsics::discriminant_value", DiscriminantValue);
      ("core::intrinsics::wrapping_add", WrappingAdd);
      ("core::mem::zeroed", Zeroed);
      ("core::num::{@N}::checked_add", Checked Add);
      ("core::num::{@N}::checked_mul", Checked Mul);
      ("core::num::{@N}::checked_sub", Checked Sub);
      ("core::num::{@N}::unchecked_add", Unchecked Add);
      ("core::num::{@N}::unchecked_mul", Unchecked Mul);
      ("core::num::{@N}::unchecked_sub", Unchecked Sub);
      ("core::num::{@N}::wrapping_add", WrappingAdd);
      ("core::option::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::option::{@T}::is_none", IsNone);
      ("core::option::{@T}::is_some", IsSome);
      ("core::option::{@T}::unwrap", OptUnwrap);
      ("core::result::{@T}::unwrap", ResUnwrap);
      ("core::result::{core::cmp::PartialEq}::eq", Eq Id);
      ("core::result::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::slice::index::{core::ops::index::Index}::index", Index);
      ("core::option::{core::cmp::PartialEq}::ne", Eq Neg);
      ("core::ops::bit::{core::ops::bit::Not}::not", BoolNot);
      ("core::slice::{@T}::len", SliceLen);
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
       | Any -> nondet f.signature
       | Assert -> assert_ f.signature
       | Assume -> assume f.signature
       | BoolNot -> bool_not f.signature
       | Checked op -> checked_op (op_of op) f.signature
       | DiscriminantValue -> discriminant_value f.signature
       | Eq b -> eq_values ~neg:(b = Neg) f.signature
       | Index -> array_index_fn f.signature
       | IsNone -> is_none f.signature
       | IsSome -> is_some f.signature
       | OptUnwrap -> unwrap_opt f.signature
       | ResUnwrap -> unwrap_res f.signature
       | SliceLen -> slice_len f.signature
       | Unchecked op -> unchecked_op (op_of op) f.signature
       | WrappingAdd -> wrapping_add f.signature
       | Zeroed -> zeroed f.signature

  let builtin_fun_eval ~crate:_ (f : Expressions.builtin_fun_id) generics =
    match f with
    | ArrayRepeat -> Some (array_repeat generics)
    | ArrayToSliceMut -> Some (array_slice ~mut:true generics)
    | ArrayToSliceShared -> Some (array_slice ~mut:false generics)
    | Index idx -> Some (array_index idx generics)
    | BoxNew -> None
end
