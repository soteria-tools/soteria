module T = Typed.T

type 'err t = {
  args : T.cval Typed.t list;
      (** List of arguments values, corresponding to the formal arguments in
          order. Really a form of [(x == a0) * (y == a1)] *)
  pre : Heap.serialized list;  (** Pre-condition as a list of fixes *)
  pc : Svalue.t list;
      (** Path condition. Whether it is in the post or in the pre, it doesn't
          matter for UX. *)
  post : Heap.serialized;  (** Post condition as a serialized heap *)
  ret : (T.cval Typed.t, 'err) result;
      (** Return value. If `ok` then it is the C value that the function
          returned, if `err` then it is a description of the bug exhibitied by
          the code *)
}
[@@deriving show { with_path = false }]

(** This function prunes the summary by removing anything that isn't reachable
    from the arguments or the return value. It returns the updated summary, as
    well as a boolean capturing whether a memory leak was detected. A memory
    leak is detected if there was an unreachable block that was not freed. *)
let prune summary = (summary, false)
(* let reachable = Bfa_symex.Var *)

let is_memory_leak _summary =
  (* This should be a reachability analysis checking for locations that are:
     - Not reachable (through the heap or PC) from the the arguments
     - Not reachable from the return value (through the heap or PC)
     - Point to non-freed blocks *)
  false

let is_manifest_bug _summary =
  (* Here are the steps to implement this reachability analysis.
     - Filter the post-condition and PC to keep only the pieces that are reachable from the arguments or the return value.
     - produce the post-condition into the empty heap, keep the resulting PC, calling it PC_post.
     - If PC_post => PC, then the bug is manifest.
  *)
  false

let analyse_summary _summary = []
