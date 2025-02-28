open Charon.NameMatcher
open Rustsymex
open Rustsymex.Syntax
open Typed
open Typed.Syntax
open Charon_util
module T = Typed.T
open Layout.Archi

module M (Heap : Heap_intf.S) = struct
  let globals : (string * rust_val) list =
    [
      (* Min *)
      ("core::num::{usize}::MIN", Base zero);
      ("core::num::{u8}::MIN", Base zero);
      ("core::num::{u16}::MIN", Base zero);
      ("core::num::{u32}::MIN", Base zero);
      ("core::num::{u64}::MIN", Base zero);
      ("core::num::{u128}::MIN", Base zero);
      ( "core::num::{isize}::MIN",
        Base (int_z (Z.neg (Z.shift_left Z.one ((word_size * 8) - 1)))) );
      ("core::num::{i8}::MIN", Base (int ~-128));
      ("core::num::{i16}::MIN", Base (int_z (Z.neg (Z.shift_left Z.one 15))));
      ("core::num::{i32}::MIN", Base (int_z (Z.neg (Z.shift_left Z.one 31))));
      ("core::num::{i64}::MIN", Base (int_z (Z.neg (Z.shift_left Z.one 63))));
      ("core::num::{i128}::MIN", Base (int_z (Z.neg (Z.shift_left Z.one 127))));
      (* Max *)
      ( "core::num::{usize}::MAX",
        Base (int_z (Z.pred (Z.shift_left Z.one (word_size * 8)))) );
      ("core::num::{u8}::MAX", Base (int 255));
      ("core::num::{u16}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 16))));
      ("core::num::{u32}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 32))));
      ("core::num::{u64}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 64))));
      ("core::num::{u128}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 128))));
      ( "core::num::{isize}::MAX",
        Base (int_z (Z.pred (Z.shift_left Z.one ((word_size * 8) - 1)))) );
      ("core::num::{i8}::MAX", Base (int 127));
      ("core::num::{i16}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 15))));
      ("core::num::{i32}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 31))));
      ("core::num::{i64}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 63))));
      ("core::num::{i128}::MAX", Base (int_z (Z.pred (Z.shift_left Z.one 127))));
    ]

  let global_map =
    NameMatcherMap.of_list
    @@ List.map (fun (p, v) -> (parse_pattern p, v)) globals

  let match_config =
    { map_vars_to_vars = false; match_with_trait_decl_refs = false }

  let global_eval ~(crate : Charon.UllbcAst.crate)
      (g : Charon.UllbcAst.global_decl) : rust_val option =
    let ctx = ctx_from_crate crate in
    NameMatcherMap.find_opt ctx match_config g.item_meta.name global_map

  let assert_ ~crate:_ ~(args : rust_val list) ~state =
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

  let assume ~crate:_ ~args ~state =
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

  let nondet ~crate:_ ~args:_ ~state ty =
    let* value = Layout.nondet ty in
    Result.ok (value, state)
end
