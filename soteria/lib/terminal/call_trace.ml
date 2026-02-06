type 'a element = { loc : 'a; msg : string }

let mk_element ~loc ?(msg = "") () : 'a element = { loc; msg }

let pp_element pp_loc ft { loc; msg } =
  if msg = "" then Fmt.pf ft "• %a" pp_loc loc
  else Fmt.pf ft "• %s: %a" msg pp_loc loc

type 'a t = 'a element list

let empty : 'a t = []
let singleton ~loc ?(msg = "") () : 'a t = [ mk_element ~loc ~msg () ]
let pp pp_loc = Fmt.Dump.list (pp_element pp_loc)

(** [rename ?rev idx msg trace] renames the element at [idx] with the message
    [msg]. If there is no element at [idx], returns the input trace. If [rev] is
    true, index 0 corresponds to the root of the trace; otherwise index 0 is the
    top (most recent element) of the trace. *)
let rename ?(rev = false) idx msg trace =
  let len = List.length trace in
  if idx < 0 || idx >= len then trace
  else
    let idx = if rev then len - idx - 1 else idx in
    List.mapi (fun i elem -> if i = idx then { elem with msg } else elem) trace
