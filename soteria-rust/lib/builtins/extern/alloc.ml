(** Builtins related to the default allocator.

    See https://doc.rust-lang.org/src/alloc/alloc.rs.html *)

open Rust_val
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
  let ptr_of_nonnull nonull = as_ptr @@ as_tuple1 nonull

  let align_of_enum align : [> T.nonzero ] Typed.t =
    let discr =
      match Rust_val.get_ty align with
      | `Tuple -> discriminant_of (as_tuple1 align)
      | _ -> as_any_int align
    in
    BV.cast_nonzero @@ Typed.cast_i Usize discr

  let alloc ?(zeroed = false) args =
    let size, align =
      match args with
      | [ size; align ] -> (as_base_i Usize size, align_of_enum align)
      | _ -> L.failwith "alloc: invalid arguments"
    in
    let align = Typed.cast_i Usize align in
    let size = Typed.cast_i Usize size in
    let max_size = Layout.max_value_z (TInt Isize) in
    let max_size = Typed.BitVec.usize max_size in
    let* () =
      assert_ (Usize.(1s) <=@ align &&@ (size <=@ max_size)) `InvalidAlloc
    in
    let align = Typed.cast align in
    let+ ptr = State.alloc_untyped ~zeroed ~size ~align () in
    mk_ptr' ptr

  let dealloc args =
    let ((ptr_in, _) as ptr), size, align =
      match args with
      | [ ptr_val; size; align ] ->
          (ptr_of_nonnull ptr_val, as_base_i Usize size, align_of_enum align)
      | _ -> L.failwith "dealloc: invalid arguments"
    in
    let alloc_size, alloc_align = Sptr.allocation_info ptr_in in
    let* () =
      assert_ (alloc_align ==?@ align &&@ (alloc_size ==?@ size)) `InvalidFree
    in
    let+ () = State.free ptr in
    mk_tuple []

  let realloc args =
    let ptr, old_size, align, size =
      match args with
      | [ ptr; old_size; align; size ] ->
          ( ptr_of_nonnull ptr,
            as_base_i Usize old_size,
            align_of_enum align,
            as_base_i Usize size )
      | _ -> L.failwith "realloc: invalid arguments"
    in
    let ptr_in, _ = ptr in
    let prev_size, prev_align = Sptr.allocation_info ptr_in in
    let* () =
      assert_
        (prev_align ==?@ align &&@ (prev_size ==?@ old_size))
        `InvalidAlloc
    in
    let align = Typed.cast align in
    let size = Typed.cast_i Usize size in
    let* new_ptr = State.alloc_untyped ~zeroed:false ~size ~align () in
    let copy_size = BV.min ~signed:false size prev_size in
    let* () = State.copy_nonoverlapping ~src:ptr ~dst:new_ptr ~size:copy_size in
    let+ () = State.free ptr in
    mk_ptr' new_ptr

  let no_alloc_shim_is_unstable _ = ok (mk_tuple [])
  let error_handler _ = error `InvalidAlloc

  let[@inline] fn_to_stub = function
    | Alloc { zeroed } -> alloc ~zeroed
    | Dealloc -> dealloc
    | NoAllocShimIsUnstable -> no_alloc_shim_is_unstable
    | Realloc -> realloc
    | ErrorHandler -> error_handler
end
