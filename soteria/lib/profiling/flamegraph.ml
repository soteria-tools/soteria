open Soteria_std
open Flamegraphs

type t = Flamegraph.t

let push_string_to_stack string (stack : Flamegraph.stack) =
  (* FIXME: This can be improved in Flamegraph *)
  let frame = Flamegraph.frame string in
  { stack with frames = stack.frames @ [ frame ] }

let pop_stack (stack : Flamegraph.stack) =
  let frames = List.remove_last stack.frames in
  { stack with frames }

module Make () = struct
  type _ Effect.t +=
    | Map_stack : (Flamegraph.stack -> Flamegraph.stack) -> unit Effect.t
    | Backtrack_n : int -> unit Effect.t
    | Save : unit Effect.t
    | Checkpoint : unit Effect.t

  let push_frame (s : string) : unit =
    Effect.perform (Map_stack (push_string_to_stack s))

  let pop_frame () = Effect.perform (Map_stack pop_stack)
  let save () = Effect.perform Save
  let backtrack_n n = Effect.perform (Backtrack_n n)
  let checkpoint () = Effect.perform Checkpoint

  module With_stack (M : Monad.Base) = struct
    let with_stack (name : string) (f : unit -> 'a M.t) : 'a M.t =
      push_frame name;
      M.map (f ()) (fun r ->
          pop_frame ();
          r)
  end

  let run f =
    let flamegraph = ref Flamegraph.empty in
    let current_stack : Flamegraph.stack Dynarray.t = Dynarray.create () in
    let last_checkpoint = ref (Unix.gettimeofday ()) in
    let checkpoint () =
      let current_time = Unix.gettimeofday () in
      let elapsed = current_time -. !last_checkpoint in
      last_checkpoint := current_time;
      let stack = Dynarray.get_last current_stack in
      let stack = { stack with weight = elapsed } in
      flamegraph := Flamegraph.add_stack stack !flamegraph
    in
    let map_stack (f : Flamegraph.stack -> Flamegraph.stack) =
      let last = Dynarray.pop_last current_stack in
      Dynarray.add_last current_stack (f last)
    in
    let open Effect.Deep in
    try f () with
    | effect Map_stack f, k ->
        checkpoint ();
        map_stack f;
        continue k ()
    | effect Backtrack_n n, k ->
        checkpoint ();
        let len = Dynarray.length current_stack in
        Dynarray.truncate current_stack (len - n);
        continue k ()
    | effect Save, k ->
        Dynarray.add_last current_stack (Dynarray.get_last current_stack);
        continue k ()
end
