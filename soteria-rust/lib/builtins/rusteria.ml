open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Charon_util

module M (Heap : Heap_intf.S) = struct
  module Sptr = Heap.Sptr

  type nonrec rust_val = Sptr.t rust_val

  let parse_string ptr state =
    let str_ty =
      Charon.Types.TAdt (TBuiltin TStr, Charon.TypesUtils.empty_generic_args)
    in
    let++ str_data, _ = Heap.load ptr str_ty state in
    let map_opt f l = Option.bind l (Monad.OptionM.all f) in
    match str_data with
    | Array bytes ->
        Some bytes
        |> map_opt (function Base b -> Some (Typed.kind b) | _ -> None)
        |> map_opt (function
             | Svalue.Int b -> Some (Char.chr (Z.to_int b))
             | _ -> None)
        |> Option.map (fun cs ->
               let str = String.of_seq @@ List.to_seq cs in
               if
                 String.starts_with ~prefix:"\"" str
                 && String.ends_with ~suffix:"\"" str
               then
                 let unquoted = String.sub str 1 (String.length str - 2) in
                 try Scanf.unescaped unquoted with _ -> unquoted
               else str)
    | _ -> None

  let assert_ ~(args : rust_val list) state =
    let open Typed.Infix in
    let* to_assert, msg =
      match args with
      | [ Base t; Ptr msg ] ->
          let+ t = cast_checked t ~ty:Typed.t_int in
          (t, msg)
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then
      let** str = parse_string msg state in
      Heap.error (`FailedAssert str) state
    else Result.ok (Charon_util.unit_, state)

  let assume ~args state =
    let* to_assume =
      match args with
      | [ Base t ] -> cast_checked t ~ty:Typed.t_int
      | _ -> not_impl "assume with non-one arguments"
    in
    L.debug (fun g -> g "Assuming: %a\n" Typed.ppa to_assume);
    let* () = assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Charon_util.unit_, state)

  let nondet (fun_sig : Charon.UllbcAst.fun_sig) ~args:_ state =
    let ty = fun_sig.output in
    let* value = Layout.nondet ty in
    Result.ok (value, state)

  let panic ~args state =
    let* msg =
      match args with
      | [ Ptr msg ] -> return msg
      | _ -> not_impl "panic with non-one arguments"
    in
    let** msg = parse_string msg state in
    Heap.error (`Panic msg) state
end
