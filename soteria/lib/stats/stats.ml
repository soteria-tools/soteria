module Hstring = Hashtbl.Hstring

type t = {
  mutable exec_time : float;
  give_up_reasons : int Hstring.t;
  mutable branch_number : int;
  mutable steps_number : int;
}
[@@deriving yojson]

let create () =
  {
    exec_time = 0.0;
    give_up_reasons = Hstring.create 0;
    branch_number = 1;
    steps_number = 0;
  }

let push_give_up_reason reason t =
  match Hstring.find_opt t.give_up_reasons reason with
  | Some count -> Hstring.replace t.give_up_reasons reason (count + 1)
  | None -> Hstring.add t.give_up_reasons reason 1

module As_ctx = struct
  type _ Effect.t += Get : t Effect.t

  let get () = Effect.perform Get

  let wrap f =
    let stats = get () in
    f stats

  let push_give_up_reason reason = wrap (push_give_up_reason reason)

  let with_stats f =
    let stats = create () in
    let time = Unix.gettimeofday () in
    let res = try f () with effect Get, k -> Effect.Deep.continue k stats in
    stats.exec_time <- Unix.gettimeofday () -. time;
    (res, stats)

  let add_branches n =
    wrap (fun stats -> stats.branch_number <- stats.branch_number + n)

  let add_steps n =
    wrap (fun stats -> stats.steps_number <- stats.steps_number + n)
end
