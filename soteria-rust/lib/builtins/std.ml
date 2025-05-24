open Charon
open Soteria_symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Typed.Infix
open Charon_util

module M (Heap : Heap_intf.S) = struct
  module Sptr = Heap.Sptr
  module Core = Core.M (Heap)
  module Encoder = Encoder.Make (Sptr)

  type nonrec rust_val = Sptr.t rust_val

  let pp_rust_val = pp_rust_val Sptr.pp

  let assert_ ~(args : rust_val list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ Base t ] -> cast_checked t ~ty:Typed.t_int
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then Heap.error (`FailedAssert None) state
    else Result.ok (Charon_util.unit_, state)

  let assume ~args ~state =
    let* to_assume =
      match args with
      | [ Base t ] -> cast_checked t ~ty:Typed.t_int
      | _ -> not_impl "assume with non-one arguments"
    in
    L.debug (fun g -> g "Assuming: %a\n" Typed.ppa to_assume);
    let* () = assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Charon_util.unit_, state)

  let kani_nondet (fun_sig : UllbcAst.fun_sig) ~args:_ ~state =
    let ty = fun_sig.output in
    let* value = Layout.nondet ty in
    Result.ok (value, state)

  let unchecked_op op (fun_sig : UllbcAst.fun_sig) ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral ty :: _ -> return ty
      | _ -> not_impl "unchecked_op wrong inputs"
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] -> return (left, right)
      | _ -> not_impl "unchecked_op with not two arguments"
    in
    let++ res = Core.eval_lit_binop op ty left right state in
    (Base res, state)

  let wrapping_op op (fun_sig : UllbcAst.fun_sig) ~args ~state =
    let* ity =
      match fun_sig.inputs with
      | TLiteral (TInteger ity) :: _ -> return ity
      | tys ->
          Fmt.kstr not_impl "wrapping_op invalid type: %a"
            Fmt.(list Types.pp_ty)
            tys
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] -> return (left, right)
      | _ -> not_impl "wrapping_op with not two arguments"
    in
    let** res = Core.safe_binop op left right state in
    let* res = Core.wrap_value ity res in
    Result.ok (Base res, state)

  let checked_op op (fun_sig : UllbcAst.fun_sig) ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral ty :: _ -> return ty
      | tys ->
          Fmt.kstr not_impl "wrapping_op invalid type: %a"
            Fmt.(list Types.pp_ty)
            tys
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] -> return (left, right)
      | _ -> not_impl "wrapping_op with not two arguments"
    in
    let++ res = Core.eval_checked_lit_binop op ty left right state in
    (res, state)

  let zeroed (fun_sig : UllbcAst.fun_sig) ~args:_ ~state =
    match Layout.zeroed ~null_ptr:Sptr.null_ptr fun_sig.output with
    | Some v -> Result.ok (v, state)
    | None -> Heap.error (`StdErr "Non-zeroable type") state

  let array_repeat (gen_args : Types.generic_args) ~args ~state =
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
      (gen_args : Types.generic_args) ~args ~state =
    let ptr, size =
      match (idx_op.is_array, List.hd args, gen_args.const_generics) with
      (* Array with static size *)
      | true, Ptr (ptr, None), [ size ] ->
          (ptr, Typed.int @@ Charon_util.int_of_const_generic size)
      | false, Ptr (ptr, Some size), [] -> (ptr, Typed.cast size)
      | _ ->
          Fmt.failwith "array_index: unexpected arguments: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            gen_args.const_generics
    in
    (* TODO: take into account idx.mutability *)
    let idx = as_base_of ~ty:Typed.t_int (List.nth args 1) in
    let ty = List.hd gen_args.types in
    let ptr' = Sptr.offset ~ty ptr idx in
    if not idx_op.is_range then
      if%sat 0s <=@ idx &&@ (idx <@ size) then
        Result.ok (Ptr (ptr', None), state)
      else Heap.error `OutOfBounds state
    else
      let range_end = as_base_of ~ty:Typed.t_int (List.nth args 2) in
      if%sat 0s <=@ idx &&@ (idx <=@ range_end) &&@ (range_end <=@ size) then
        let size = range_end -@ idx in
        Result.ok (Ptr (ptr', Some size), state)
      else Heap.error `OutOfBounds state

  (* Some array accesses are ran on functions, so we handle those here and redirect them.
     Eventually, it would be good to maybe make a Charon pass that gets rid of these before. *)
  let array_index_fn (fun_sig : UllbcAst.fun_sig) ~args ~state =
    let ptr, range, mode, gargs, range_id =
      match (args, fun_sig.inputs) with
      (* Unfortunate, but right now i don't have a better way to handle this... *)
      | ( [ ptr; Struct range ],
          [
            TRef (_, TAdt (TBuiltin ((TArray | TSlice) as mode), gargs), _);
            TAdt (TAdtId range_id, _);
          ] ) ->
          (ptr, range, mode, gargs, range_id)
      | _ -> failwith "Unexpected input type"
    in
    let range_item =
      match (Crate.get_adt range_id).item_meta.lang_item with
      | Some item -> item
      | None -> failwith "Unexpected range item"
    in
    let size =
      match (ptr, gargs.const_generics) with
      (* Array with static size *)
      | _, [ size ] -> Typed.int @@ Charon_util.int_of_const_generic size
      | Ptr (_, Some size), [] -> Typed.cast size
      | _ -> failwith "array_index (fn): couldn't calculate size"
    in
    let idx_from, idx_to =
      match (range_item, range) with
      | "RangeFull", [] -> (Base 0s, Base size)
      | "RangeFrom", [ from ] -> (from, Base size)
      | "RangeTo", [ to_ ] -> (Base 0s, to_)
      | "Range", [ from; to_ ] -> (from, to_)
      | "RangeInclusive", [ from; Base to_ ] ->
          (from, Base (Typed.cast to_ +@ 1s))
      | "RangeToInclusive", [ Base to_ ] ->
          (Base 0s, Base (Typed.cast to_ +@ 1s))
      | _ -> Fmt.failwith "array_index (fn): unexpected range %s" range_item
    in
    let idx_op : Expressions.builtin_index_op =
      { is_array = mode = TArray; mutability = RShared; is_range = true }
    in
    array_index idx_op gargs ~args:[ ptr; idx_from; idx_to ] ~state

  let array_slice ~mut:_ (gen_args : Types.generic_args) ~args ~state =
    let size =
      match gen_args.const_generics with
      | [ size ] -> Charon_util.int_of_const_generic size
      | _ -> failwith "array_slice: unexpected generic constants"
    in
    match args with
    | [ Ptr (ptr, None) ] -> Result.ok (Ptr (ptr, Some (Typed.int size)), state)
    | _ -> failwith "array_index: unexpected arguments"

  let discriminant_value (funsig : GAst.fun_sig) ~args ~state =
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

  let assert_zero_is_valid (fun_sig : GAst.fun_sig) ~args:_ ~state =
    let ty =
      List.hd
        (List.hd fun_sig.generics.trait_clauses).trait.binder_value
          .decl_generics
          .types
    in
    match Layout.zeroed ~null_ptr:Sptr.null_ptr ty with
    | None ->
        Heap.error (`Panic (Some "core::intrinsics::assert_zero_valid")) state
    | _ -> Result.ok (Tuple [], state)

  let size_of (fun_sig : GAst.fun_sig) ~args:_ ~state =
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    let+ size = Layout.size_of_s ty in
    Ok (Base size, state)

  let size_of_val (fun_sig : GAst.fun_sig) ~args ~state =
    let ty =
      match fun_sig.inputs with
      | [ TRawPtr (ty, _) ] -> ty
      | _ -> failwith "size_of_val: Invalid input type"
    in
    match (ty, args) with
    | TAdt (TBuiltin TSlice, { types = [ sub_ty ]; _ }), [ Ptr (_, Some meta) ]
      ->
        let* len = cast_checked meta ~ty:Typed.t_int in
        let+ size = Layout.size_of_s sub_ty in
        let size = size *@ len in
        Ok (Base size, state)
    | TAdt (TBuiltin TStr, _), [ Ptr (_, Some meta) ] ->
        let+ len = cast_checked meta ~ty:Typed.t_int in
        let size = Layout.size_of_int_ty U8 in
        let size = Typed.int size *@ len in
        Ok (Base size, state)
    | ty, _ ->
        let+ size = Layout.size_of_s ty in
        Ok (Base size, state)

  let min_align_of ~in_input (fun_sig : GAst.fun_sig) ~args:_ ~state =
    let ty =
      if in_input then
        match fun_sig.inputs with
        | [ TRawPtr (ty, _) ] -> ty
        | _ -> failwith "min_align_of: invalid input type"
      else
        (List.hd fun_sig.generics.trait_clauses).trait.binder_value
          .decl_generics
          .types
        |> List.hd
    in
    let* align = Layout.align_of_s ty in
    Result.ok (Base align, state)

  let box_new (gen_args : Types.generic_args) ~args ~state =
    let ty, v =
      match (gen_args, args) with
      | { types = [ ty ]; _ }, [ v ] -> (ty, v)
      | _ -> failwith "box new: invalid arguments"
    in
    let** ptr, state = Heap.alloc_ty ty state in
    let++ (), state = Heap.store ptr ty v state in
    (Ptr ptr, state)

  let ptr_op ?(byte = false) op (funsig : GAst.fun_sig) ~args ~state =
    let** ptr, meta, v =
      match args with
      | [ Ptr (ptr, meta); Base v ] -> Result.ok (ptr, meta, v)
      | [ Base _; Base _ ] -> Heap.error `UBPointerArithmetic state
      | _ -> not_impl "ptr_add: invalid arguments"
    in
    let* v = cast_checked v ~ty:Typed.t_int in
    let ty =
      if byte then Types.TLiteral (TInteger U8)
      else
        match funsig.inputs with
        | TRawPtr (ty, _) :: _ -> ty
        | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let v = if op = Expressions.Add then v else ~-v in
    let ptr' = Sptr.offset ~ty ptr v in
    if%sat Sptr.constraints ptr' then Result.ok (Ptr (ptr', meta), state)
    else Heap.error `Overflow state

  let box_into_raw ~args ~state =
    (* internally a box is exactly a pointer so nothing to do *)
    let box_ptr = List.hd args in
    Result.ok (box_ptr, state)

  let ptr_offset_from (funsig : GAst.fun_sig) ~args ~state =
    let ptr1, ptr2 =
      match args with
      | [ Ptr (ptr1, _); Ptr (ptr2, _) ] -> (ptr1, ptr2)
      | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let ty =
      match funsig.inputs with
      | TRawPtr (ty, _) :: _ -> ty
      | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let* size = Layout.size_of_s ty in
    if%sat Sptr.is_same_loc ptr1 ptr2 &&@ (size >@ 0s) then
      let size = Typed.cast size in
      let off = Sptr.distance ptr1 ptr2 in
      if%sat off %@ size ==@ 0s then Result.ok (Base (off /@ size), state)
      else Heap.error `UBPointerComparison state
    else Heap.error `UBPointerComparison state

  let black_box ~args ~state =
    match args with
    | [ v ] -> Result.ok (v, state)
    | _ -> failwith "black_box: invalid arguments"

  let transmute (funsig : GAst.fun_sig) ~args ~state =
    let from_ty = List.hd funsig.inputs in
    let to_ty = funsig.output in
    let v = List.hd args in
    let++ v =
      Heap.lift_err state
      @@ Encoder.transmute ~verify_ptr:(Heap.is_valid_ptr state) ~from_ty ~to_ty
           v
    in
    (v, state)

  let copy_nonoverlapping (ty : Types.ty) ~args ~state =
    let (from_ptr, _), (to_ptr, _), len =
      match args with
      | [ Ptr from_ptr; Ptr to_ptr; Base len ] ->
          (from_ptr, to_ptr, Typed.cast len)
      | _ -> failwith "copy_nonoverlapping: invalid arguments"
    in
    let** () = Heap.check_ptr_align from_ptr ty in
    let** () = Heap.check_ptr_align to_ptr ty in
    let* ty_size = Layout.size_of_s ty in
    let size = ty_size *@ len in
    let** () =
      if%sat Sptr.is_at_null_loc from_ptr ||@ Sptr.is_at_null_loc to_ptr then
        Heap.error `NullDereference state
      else
        (* check for overlap *)
        let from_ptr_end = Sptr.offset from_ptr size in
        let to_ptr_end = Sptr.offset to_ptr size in
        if%sat
          Sptr.is_same_loc from_ptr to_ptr
          &&@ (Sptr.distance from_ptr to_ptr_end
              <@ 0s
              &&@ (Sptr.distance to_ptr from_ptr_end <@ 0s))
        then Heap.error (`StdErr "copy_nonoverlapping overlapped") state
        else Result.ok ()
    in
    let++ (), state =
      Heap.copy_nonoverlapping ~dst:(to_ptr, None) ~src:(from_ptr, None) ~size
        state
    in
    (Tuple [], state)

  let copy_nonoverlapping_fn (funsig : GAst.fun_sig) =
    let ty =
      match funsig.inputs with
      | TRawPtr (ty, _) :: _ -> ty
      | _ -> failwith "copy_nonoverlapping: invalid arguments"
    in
    copy_nonoverlapping ty

  let mul_add ~args ~state =
    match args with
    | [ Base a; Base b; Base c ] ->
        let a : Typed.T.sfloat Typed.t = Typed.cast a in
        let b : Typed.T.sfloat Typed.t = Typed.cast b in
        let c : Typed.T.sfloat Typed.t = Typed.cast c in
        Result.ok (Base ((a *.@ b) +.@ c), state)
    | _ -> failwith "mul_add expects three arguments"

  let abs ~args ~state =
    match args with
    | [ Base v ] ->
        Result.ok (Base (Typed.cast @@ Typed.abs_f @@ Typed.cast v), state)
    | _ -> failwith "abs expects one argument"

  let write_bytes (fun_sig : GAst.fun_sig) ~args ~state =
    let ptr, dst, v, count =
      match args with
      | [ Ptr ((ptr, _) as dst); Base v; Base count ] -> (ptr, dst, v, count)
      | _ -> failwith "unexpected write_bytes arguments"
    in
    let* count = cast_checked count ~ty:Typed.t_int in
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    let** () = Heap.check_ptr_align ptr ty in
    let* size = Layout.size_of_s ty in
    let size = size *@ count in
    (* TODO: if v == 0, then we can replace this mess by initialising a Zeros subtree *)
    if%sat v ==@ 0s then
      let++ (), state = Heap.zeros dst size state in
      (Tuple [], state)
    else
      match Typed.kind size with
      | Int bytes ->
          let list = List.init (Z.to_int bytes) Fun.id in
          let++ (), state =
            Result.fold_list list ~init:((), state) ~f:(fun ((), state) i ->
                let ptr = Sptr.offset ptr @@ Typed.int i in
                Heap.store (ptr, None) (Types.TLiteral (TInteger U8)) (Base v)
                  state)
          in
          (Tuple [], state)
      | _ -> failwith "write_bytes: don't know how to handle symbolic sizes"

  let assert_inhabited (fun_sig : GAst.fun_sig) ~args:_ ~state =
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    if Layout.is_inhabited ty then Result.ok (Tuple [], state)
    else Heap.error (`Panic (Some "core::intrinsics::assert_inhabited")) state

  let from_raw_parts ~args ~state =
    match args with
    | [ Ptr (ptr, _); Base meta ] -> Result.ok (Ptr (ptr, Some meta), state)
    | [ Base v; Base meta ] ->
        let* v = cast_checked ~ty:Typed.t_int v in
        let ptr = Sptr.offset Sptr.null_ptr v in
        Result.ok (Ptr (ptr, Some meta), state)
    | _ ->
        Fmt.failwith "from_raw_parts: invalid arguments %a"
          Fmt.(list ~sep:comma pp_rust_val)
          args

  let nop ~args:_ ~state = Result.ok (Tuple [], state)

  let is_val_statically_known ~args:_ ~state =
    (* see: https://doc.rust-lang.org/std/intrinsics/fn.is_val_statically_known.html *)
    let* b = Rustsymex.nondet Typed.t_bool in
    Result.ok (Base (Typed.int_of_bool b), state)

  let std_assume ~args ~state =
    match args with
    | [ Base cond ] ->
        let* cond = cast_checked ~ty:Typed.t_int cond in
        if%sat Typed.bool_of_int cond then Result.ok (Tuple [], state)
        else Heap.error (`Panic (Some "core::intrinsics::assume")) state
    | _ -> failwith "std_assume: invalid arguments"

  let exact_div (funsig : GAst.fun_sig) ~args ~state =
    match (funsig.inputs, args) with
    | TLiteral lit :: _, [ Base l; Base r ] ->
        let* l, r, ty = cast_checked2 l r in
        let open Typed in
        let** res = Core.eval_lit_binop Expressions.Div lit l r state in
        if is_float ty then Result.ok (Base res, state)
        else
          if%sat (not (r ==@ 0s)) &&@ (l %@ cast r ==@ 0s) then
            Result.ok (Base res, state)
          else Heap.error (`Panic (Some "core::intrinsics::exact_div")) state
    | _ -> failwith "exact_div: invalid arguments"

  let ctpop (funsig : GAst.fun_sig) ~args ~state =
    match args with
    | [ Base v ] -> (
        let ty =
          match funsig.inputs with
          | [ TLiteral (TInteger ty) ] -> ty
          | _ -> failwith "ctpop: invalid arguments"
        in
        let bits = 8 * Layout.size_of_int_ty ty in
        let* v = cast_checked ~ty:Typed.t_int v in
        match Typed.kind v with
        | Int v ->
            (* convert to unsigned *)
            let v =
              if Layout.is_signed ty then
                let maxv = Z.shift_left Z.one bits in
                if Z.(v < zero) then Z.(((v mod maxv) + maxv) mod maxv) else v
              else v
            in
            let v = Typed.int @@ Z.popcount v in
            Result.ok (Base v, state)
        | _ ->
            (* convert to unsigned *)
            let* v =
              if Layout.is_signed ty then
                let max = Typed.nonzero_z (Z.shift_left Z.one bits) in
                if%sat v <@ 0s then return (((v %@ max) +@ max) %@ max)
                else return v
              else return v
            in
            let two = Typed.nonzero 2 in
            let res =
              List.init bits (fun i -> i)
              |> List.fold_left
                   (fun acc off ->
                     let pow = Typed.nonzero_z (Z.shift_left Z.one off) in
                     acc +@ (v /@ pow %@ two))
                   0s
            in
            Result.ok (Base (res :> Typed.T.cval Typed.t), state))
    | _ -> failwith "ctpop: invalid arguments"

  let compare_bytes ~args ~state =
    let l, r, len =
      match args with
      | [ Ptr (l, _); Ptr (r, _); Base len ] -> (l, r, len)
      | _ -> failwith "compare_bytes: invalid arguments"
    in
    let* len = cast_checked ~ty:Typed.t_int len in
    let byte = Types.TLiteral (TInteger U8) in
    let rec aux l r len state =
      if%sat len ==@ 0s then Result.ok (Base 0s, state)
      else
        let** bl, state = Heap.load (l, None) byte state in
        let bl = as_base_of ~ty:Typed.t_int bl in
        let** br, state = Heap.load (r, None) byte state in
        let br = as_base_of ~ty:Typed.t_int br in
        if%sat bl ==@ br then
          let l = Sptr.offset l 1s in
          let r = Sptr.offset r 1s in
          aux l r (len -@ 1s) state
        else
          if%sat bl <@ br then Result.ok (Base (-1s), state)
          else Result.ok (Base 1s, state)
    in
    aux l r len state

  let likely ~args ~state =
    match args with
    | [ (Base _ as res) ] -> Result.ok (res, state)
    | _ -> failwith "likely: invalid arguments"

  let copy_sign ~args ~state =
    let l, r =
      match args with
      | [ Base l; Base r ] -> (l, r)
      | _ -> failwith "copy_sign: invalid arguments"
    in
    let* l, r, ty = cast_checked2 l r in
    if Typed.is_float ty then
      let zero = Typed.float_like r 0.0 in
      if%sat [@lname "copy_sign < 0"] [@rname "copy_sign >=0"] r <.@ zero then
        let l' = Typed.neg (Typed.abs_f l) in
        Result.ok (Base (Typed.cast l'), state)
      else
        let l' = Typed.abs_f l in
        Result.ok (Base (Typed.cast l'), state)
    else not_impl "Expected floats in copy_sign"

  let variant_count (fun_sig : GAst.fun_sig) ~args:_ ~state =
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    match ty with
    | Types.TAdt (TAdtId id, _) when Crate.is_enum id ->
        let variants = Crate.as_enum id in
        let n = Typed.int @@ List.length variants in
        Result.ok (Base n, state)
    | _ -> Heap.error (`Panic (Some "core::intrinsics::variant_count")) state

  let std_panic ~args:_ ~state = Heap.error (`Panic None) state

  let float_is (fp : fpclass) ~args ~state =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is: invalid argument"
    in
    let* v =
      of_opt_not_impl ~msg:"float_is expects float" @@ Typed.cast_float v
    in
    let res =
      match fp with
      | FP_nan -> Typed.is_nan v
      | FP_normal -> Typed.is_normal v
      | FP_infinite -> Typed.is_infinite v
      | FP_zero -> Typed.is_zero v
      | FP_subnormal -> Typed.is_subnormal v
    in
    Result.ok (Base (Typed.int_of_bool res), state)

  let float_is_finite ~args ~state =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is_finite: invalid argument"
    in
    let* v =
      of_opt_not_impl ~msg:"float_is_finite expects float" @@ Typed.cast_float v
    in
    let res = Typed.((not (is_nan v)) &&@ not (is_infinite v)) in
    Result.ok (Base (Typed.int_of_bool res), state)

  let float_fast (bop : Expressions.binop) ~args ~state =
    let l, r =
      match args with
      | [ Base l; Base r ] -> (l, r)
      | _ -> failwith "fast_float: invalid arguments"
    in
    let l, r =
      match (Typed.cast_float l, Typed.cast_float r) with
      | Some l, Some r -> (l, r)
      | _ -> failwith "fast_float: invalid arguments"
    in
    let is_finite f = Typed.((not (is_nan f)) &&@ not (is_infinite f)) in
    if%sat is_finite l &&@ is_finite r then
      let bop =
        match bop with
        | Add -> ( +.@ )
        | Sub -> ( -.@ )
        | Mul -> ( *.@ )
        | Div -> ( /.@ )
        | _ -> failwith "fast_float: invalid binop"
      in
      Result.ok (Base (bop l r), state)
    else
      let name =
        match bop with
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div -> "div"
        | _ -> assert false
      in
      let msg =
        Fmt.str "core::intrinsics::f%s_fast: operands must be finite" name
      in
      Heap.error (`Panic (Some msg)) state

  let float_is_sign pos ~args ~state =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is_sign: invalid argument"
    in
    let* v =
      of_opt_not_impl ~msg:"float_is_sign expects float" @@ Typed.cast_float v
    in
    let res =
      if pos then Typed.(leq (float_like v 0.) v)
      else Typed.(leq v (float_like v (-0.)))
    in
    let res = res ||@ Typed.is_nan v in
    Result.ok (Base (Typed.int_of_bool res), state)
end
