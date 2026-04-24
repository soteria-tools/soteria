open Soteria_std

type frame = string
type stack = { frames : frame list; weight : float }

let push_string_to_stack string (stack : stack) =
  { stack with frames = string :: stack.frames }

let pop_stack (stack : stack) =
  let frames = List.tl stack.frames in
  { stack with frames }

let write_folded_stacks path stacks =
  match
    Out_channel.with_open_text path (fun oc ->
        List.iter
          (fun { frames; weight } ->
            let weight_us = int_of_float (weight *. 1_000_000.0) in
            if weight_us > 0 then
              let line = String.concat ";" (List.rev frames) in
              Printf.fprintf oc "%s %d\n" line weight_us)
          stacks)
  with
  | () -> ()
  | exception Sys_error e ->
      Logs.L.error (fun m -> m "Could not write flamegraph: %s" e)

module Make () = struct
  type _ Effect.t +=
    | Map_stack : (stack -> stack) -> unit Effect.t
    | Backtrack_n : int -> unit Effect.t
    | Save : unit Effect.t
    | Checkpoint : unit Effect.t

  let push_frame (s : string) : unit =
    Effect.perform (Map_stack (push_string_to_stack s))

  let pop_frame () = Effect.perform (Map_stack pop_stack)
  let save () = Effect.perform Save
  let backtrack_n n = Effect.perform (Backtrack_n n)
  let checkpoint () = Effect.perform Checkpoint

  module With_frame (M : Monad.Base) = struct
    let with_frame (name : string) (f : unit -> 'a M.t) : 'a M.t =
      push_frame name;
      M.map (f ()) (fun r ->
          pop_frame ();
          r)
  end

  let run ~flamegraph_pl f =
    let stacks = ref [] in
    let current_stack : stack Dynarray.t = Dynarray.create () in
    let () = Dynarray.add_last current_stack { frames = []; weight = 0.0 } in
    let last_checkpoint = ref (Unix.gettimeofday ()) in
    let map_stack (g : stack -> stack) =
      let last = Dynarray.pop_last current_stack in
      Dynarray.add_last current_stack (g last)
    in
    let checkpoint () =
      let current_time = Unix.gettimeofday () in
      let elapsed = current_time -. !last_checkpoint in
      last_checkpoint := current_time;
      let stack = Dynarray.get_last current_stack in
      stacks := { stack with weight = elapsed } :: !stacks
    in
    let open Effect.Deep in
    Fun.protect
      ~finally:(fun () ->
        checkpoint ();
        write_folded_stacks flamegraph_pl (List.rev !stacks))
      (fun () ->
        try f () with
        | effect Map_stack g, k ->
            checkpoint ();
            map_stack g;
            continue k ()
        | effect Backtrack_n n, k ->
            checkpoint ();
            let len = Dynarray.length current_stack in
            Dynarray.truncate current_stack (len - n);
            continue k ()
        | effect Save, k ->
            Dynarray.add_last current_stack (Dynarray.get_last current_stack);
            continue k ()
        | effect Checkpoint, k ->
            checkpoint ();
            continue k ())

  let run_ignored f =
    let open Effect.Deep in
    try f () with
    | effect Map_stack _, k -> continue k ()
    | effect Backtrack_n _, k -> continue k ()
    | effect Save, k -> continue k ()
    | effect Checkpoint, k -> continue k ()

  (** is [run] if [flamegraph_pl] is [Some _] and [run_ignored] otherwise *)
  let run_if_file ~flamegraph_pl =
    match flamegraph_pl with
    | Some flamegraph_pl -> fun f -> run ~flamegraph_pl f
    | None -> fun f -> run_ignored f
end
