open Soteria_std

type frame = string
type stack = { rev_frames : frame list; weight : float }

let push_string_to_stack string (stack : stack) =
  { stack with rev_frames = string :: stack.rev_frames }

let pop_stack (stack : stack) =
  let rev_frames = List.tl stack.rev_frames in
  { stack with rev_frames }

let dump path stacks =
  let path =
    if String.ends_with ~suffix:".collapsed" path then path
    else path ^ ".collapsed"
  in
  Unix.ensure_dir_exists (Filename.dirname path);
  match
    Out_channel.with_open_text path (fun oc ->
        List.iter
          (fun { rev_frames; weight } ->
            let weight_us = int_of_float (weight *. 1_000_000.0) in
            if weight_us > 0 then
              let line = String.concat ";" (List.rev rev_frames) in
              Printf.fprintf oc "%s %d\n" line weight_us)
          stacks)
  with
  | () -> ()
  | exception Sys_error e ->
      Terminal.Warn.warn (Fmt.str "Failed to write flamegraph to %s: %s" path e)

module Make (M : Monad.Base) = struct
  type _ Effect.t +=
    | Map_stack : (stack -> stack) -> unit Effect.t
    | Backtrack_n : int -> unit Effect.t
    | Save : unit Effect.t
    | Checkpoint : unit Effect.t

  type t = stack list

  let perform_ eff =
    if Option.is_some (Config.get ()).flamegraphs then Effect.perform eff
    else ()

  let push_frame (s : string) : unit =
    perform_ (Map_stack (push_string_to_stack s))

  let pop_frame () = perform_ (Map_stack pop_stack)
  let save () = perform_ Save
  let backtrack_n n = perform_ (Backtrack_n n)
  let checkpoint () = perform_ Checkpoint

  let with_frame (name : string) (f : unit -> 'a M.t) : 'a M.t =
    if Option.is_some (Config.get ()).flamegraphs then (
      (* the bind + return to make sure the effect isn't raised until the
         computation is ran. *)
      M.return ()
      |> M.bind @@ fun () ->
         push_frame name;
         f ()
         |> M.map @@ fun r ->
            pop_frame ();
            r)
    else f ()

  (** {2 Bookkeeping}
      Implementing {!Effects.Bookkeeping} *)

  type arg = string

  let with_ _name f =
    let stacks = ref [] in
    let current_stack : stack Dynarray.t = Dynarray.create () in
    Dynarray.add_last current_stack
      { rev_frames = [ "SYMBOLIC EXECUTION ROOT" ]; weight = 0.0 };
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
    try
      let res = f () in
      checkpoint ();
      (res, List.rev !stacks)
    with
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
        continue k ()

  let with_ignored () f =
    let open Effect.Deep in
    try f () with
    | effect Map_stack _, k -> continue k ()
    | effect Backtrack_n _, k -> continue k ()
    | effect Save, k -> continue k ()
    | effect Checkpoint, k -> continue k ()

  let with_dumped name f =
    match (Config.get ()).flamegraphs with
    | None -> with_ignored () f
    | Some dir ->
        let res, stack = with_ name f in
        let dest_file = Filename.concat dir name in
        dump dest_file stack;
        res
end
