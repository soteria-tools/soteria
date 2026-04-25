(** This file was generated with [scripts/stubs.py] -- do not edit it manually,
    instead modify the script and re-run it. *)

open Charon
open Common

module M (StateM : State.StateM.S) = struct
  open StateM

  type rust_val = Sptr.t Rust_val.t
  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
  type full_ptr = StateM.Sptr.t Rust_val.full_ptr

  module type S = sig
    val kani_assert :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret

    val kani_assume :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret

    val kani_panic :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret

    val soteria_assert :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret

    val soteria_assume :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret

    val nondet_bytes :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret

    val soteria_panic :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret
  end
end
