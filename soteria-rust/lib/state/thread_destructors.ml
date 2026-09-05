open Compo_res
module SM_Base = Rustsymex

(* NOTE: for the benefit of code modularity, we define thread destructors here.
   Because they are defined over a state, which itself wants to keep track of
   thread destructors, they actually require higher-order functions! It kind of
   works here, although we are not able to define a state monad, since [SM]
   doesn't support higher order. *)

(* TODO: how do we serialize this? An option is that rather than storing a
   higher order OCaml function, we hardcode a set list of possible callbacks
   with an ADT, and possibly just keep track of the function pointer to call +
   the arguments to pass. All of these are easy to serialize, as they are just
   values!

   Consuming then removes the callback from a list, and producing adds the
   callback. This does mean that suddenly the list is ordered, which is probably
   bad, but it might be worth a try, it seems unlikely that ordering would be
   problematic in practice.

   This solution does require changing [get_thread_exits]; since we need to
   resolve function pointers (which isn't accessible here), and execute
   functions (only doable through the interpreter). An option is that
   [get_thread_exits] returns the list of functions to call, and the interpreter
   handles it there.

   Regardless, serializing this probably doesn't matter much. Thread destructors
   are only called at the end of a process, and in compositional analysis that
   doesn't really happen. *)

type 'st t =
  unit -> 'st -> ((unit, Error.with_trace, unit) Compo_res.t * 'st) SM_Base.t

type _ syn = | [@@deriving show]

let pp ft _ = Fmt.string ft "<thread destructors>"

let to_syn st =
  Soteria.Terminal.Warn.warn
    "tried serializing thread destructors; this is not yet supported. Nothing \
     was serialized instead.";
  []

let ins_outs (syn : _ syn) = match syn with _ -> .
let produce (syn : _ syn) _ = match syn with _ -> .
let consume (syn : _ syn) _ = match syn with _ -> .

(* define helpers, given we don't have an SM to do the lifting *)
module StateM = Monad.StateT_p (SM_Base)
module Result = Compo_res.T2 (StateM)

let get_state () = Result.lift @@ StateM.get_state ()
let set_state st = Result.lift @@ StateM.set_state st
let ( let* ) x f = Result.bind f x
let ( let+ ) x f = Result.map f x

let register_thread_exit callback =
  (* HACK: we cannot expect thread exit callbacks to miss with syn, because when
     we define the callback type the syn type has not yet been defined. Instead
     we expect it to return unit; for now we fail, while we figure out a
     solution. *)
  let callback () =
    Result.map_missing
      (fun _ -> L.failwith "TODO: Miss in thread exit")
      (callback ())
  in
  let* thread_destructor = get_state () in
  let destructor =
    match thread_destructor with
    | None -> callback
    | Some destructor -> fun () -> Result.bind callback (destructor ())
  in
  set_state (Some destructor)

let get_thread_exits () (thread_destructor : 'st t option) =
  let destructor () =
    match thread_destructor with
    | None -> Result.ok ()
    | Some destructor -> Result.map_missing (fun _ -> []) (destructor ())
  in
  Result.ok destructor thread_destructor
