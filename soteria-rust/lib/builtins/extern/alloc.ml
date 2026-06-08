(** Builtins related to the default allocator.

    See https://doc.rust-lang.org/src/alloc/alloc.rs.html *)

open Svalue
open Typed
open Typed.Infix
open Typed.Syntax

type fn =
  | Alloc of { zeroed : bool }
  | Dealloc
  | Realloc
  | NoAllocShimIsUnstable
  | ErrorHandler

let fn_pats =
  [
    ("__rust_alloc", Alloc { zeroed = false });
    ("__rust_alloc_zeroed", Alloc { zeroed = true });
    ("__rust_dealloc", Dealloc);
    ("__rust_no_alloc_shim_is_unstable_v2", NoAllocShimIsUnstable);
    ("__rust_realloc", Realloc);
    ("__rust_alloc_error_handler", ErrorHandler);
  ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  (* NonNull<u8> is a struct { pointer: *const u8 is !null }. Extract the inner
     ptr. *)
  let ptr_of_nonnull nonull =
    match Typed.Adt.as_tuple @@ Typed.cast_any_adt nonull with
    | [ v ] -> Typed.cast_ptr_f v
    | _ -> Fmt.failwith "alloc: invalid NonNull argument: %a" Typed.ppa nonull

  let align_of_enum align : [> T.nonzero ] Typed.t =
    match Typed.Adt.as_tuple @@ Typed.cast_any_adt align with
    | [ align_enum ] ->
        let as_enum = Typed.cast_any_adt align_enum in
        let discr = Typed.Adt.discriminant_of as_enum in
        Typed.cast_nonzero @@ Typed.cast_i Usize discr
    | v -> Fmt.failwith "alloc: invalid align argument: %a" Typed.ppa align

  let alloc ?(zeroed = false) args =
    let size, align =
      match args with
      | [ size; align ] -> (Typed.cast_i Usize size, align_of_enum align)
      | _ -> failwith "alloc: invalid arguments"
    in
    let max_size = Layout.max_value_z (TInt Isize) in
    let max_size = Typed.BitVec.usize max_size in
    let* () =
      assert_ (Usize.(1s) <=@ align &&@ (size <=@ max_size)) `InvalidAlloc
    in
    State.alloc_untyped ~zeroed ~size ~align ()

  let dealloc args =
    let ptr, size, align =
      match args with
      | [ ptr_val; size; align ] ->
          (ptr_of_nonnull ptr_val, Typed.cast_i Usize size, align_of_enum align)
      | _ -> failwith "dealloc: invalid arguments"
    in
    let ptr_in = Typed.Ptr.ptr_of ptr in
    let alloc_size, alloc_align = Sptr.allocation_info ptr_in in
    let* () =
      assert_ (alloc_align ==?@ align &&@ (alloc_size ==?@ size)) `InvalidFree
    in
    let+ () = State.free ptr in
    Typed.Adt.mk_tuple []

  let realloc args =
    let ptr, old_size, align, size =
      match args with
      | [ ptr; old_size; align; size ] ->
          ( ptr_of_nonnull ptr,
            Typed.cast_i Usize old_size,
            align_of_enum align,
            Typed.cast_i Usize size )
      | _ -> failwith "realloc: invalid arguments"
    in
    let ptr_in = Typed.Ptr.ptr_of ptr in
    let prev_size, prev_align = Sptr.allocation_info ptr_in in
    let* () =
      assert_
        (prev_align ==?@ align &&@ (prev_size ==?@ old_size))
        `InvalidAlloc
    in
    let* new_ptr = State.alloc_untyped ~zeroed:false ~size ~align () in
    let copy_size = BV.min ~signed:false size prev_size in
    let* () = State.copy_nonoverlapping ~src:ptr ~dst:new_ptr ~size:copy_size in
    let+ () = State.free ptr in
    Typed.as_any new_ptr

  let no_alloc_shim_is_unstable _ = ok (Typed.Adt.mk_tuple [])
  let error_handler _ = error `InvalidAlloc

  let[@inline] fn_to_stub = function
    | Alloc { zeroed } -> alloc ~zeroed
    | Dealloc -> dealloc
    | NoAllocShimIsUnstable -> no_alloc_shim_is_unstable
    | Realloc -> realloc
    | ErrorHandler -> error_handler
end
