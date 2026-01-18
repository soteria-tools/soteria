open Rust_val

module M (Rust_state_m : Rust_state_m.S) = struct
  open Rust_state_m
  open Syntax

  let parse_string ptr =
    let str_ty : Charon.Types.ty =
      TAdt
        { id = TBuiltin TStr; generics = Charon.TypesUtils.empty_generic_args }
    in
    let+ str_data = State.load ptr str_ty in
    let map_opt f l = Option.bind l (Monad.OptionM.all f) in
    match str_data with
    | Tuple bytes ->
        Some bytes
        |> map_opt (function Int b -> Typed.BitVec.to_z b | _ -> None)
        |> Option.map (fun cs ->
            let cs = List.map (fun z -> Char.chr (Z.to_int z)) cs in
            let str = String.of_seq @@ List.to_seq cs in
            if
              String.starts_with ~prefix:"\"" str
              && String.ends_with ~suffix:"\"" str
            then
              let unquoted = String.sub str 1 (String.length str - 2) in
              try Scanf.unescaped unquoted with _ -> unquoted
            else str)
    | _ -> None

  let assert_ args =
    let to_assert, msg =
      match args with
      | [ Int t; Ptr msg ] -> (Typed.cast_lit TBool t, msg)
      | _ -> failwith "to_assert with non-one arguments"
    in
    if%sat Typed.not (Typed.BitVec.to_bool to_assert) then
      let* str = parse_string msg in
      error (`FailedAssert str)
    else ok unit_

  let assume args =
    let to_assume =
      match args with
      | [ Int t ] -> Typed.cast_lit TBool t
      | _ -> failwith "assume with non-one arguments"
    in
    L.debug (fun g -> g "Assuming: %a\n" Typed.ppa to_assume);
    let+ () = assume [ Typed.BitVec.to_bool to_assume ] in
    unit_

  let nondet (fun_sig : Charon.Types.fun_sig) _ =
    let* output = Poly.subst_ty fun_sig.output in
    Encoder.nondet output

  let panic ?msg args =
    let* msg =
      match args with [ Ptr msg ] -> parse_string msg | _ -> ok msg
    in
    error (`Panic msg)
end
