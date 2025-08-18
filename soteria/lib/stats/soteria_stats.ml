module Hstring = Hashtbl.Hstring
module Hsset = Hashset.Hstring
module Range = Soteria_terminal.Range
module Config = Config

type t = {
  mutable exec_time : float;  (** Execution time for the whole execution *)
  give_up_reasons : Range.t Dynarray.t Hstring.t;
      (** For each "give up reason" (a string) we keep track of the list of code
          locations at which we reached it *)
  missing_without_fixes : string Dynarray.t;  (** A list of *)
  mutable branch_number : int;  (** Number of explored branches *)
  mutable steps_number : int;  (** Number of steps taken *)
}
[@@deriving yojson]

(** Merges two stats records. Does not modify either. *)
let merge t1 t2 =
  let merge_hstrings h1 h2 =
    let h = Hstring.create (Hstring.length h1 + Hstring.length h2) in
    let add_pair key arr =
      match Hstring.find_opt h key with
      | Some existing_arr -> Dynarray.append existing_arr arr
      | None ->
          (* Avoid sharing memory... *)
          let empty = Dynarray.create () in
          Hstring.add h key empty;
          Dynarray.append empty arr
    in
    Hstring.iter add_pair h1;
    Hstring.iter add_pair h2;
    h
  in

  let give_up_reasons = merge_hstrings t1.give_up_reasons t2.give_up_reasons in
  let missing_without_fixes =
    let d = Dynarray.create () in
    Dynarray.append d t1.missing_without_fixes;
    Dynarray.append d t2.missing_without_fixes;
    d
  in
  {
    give_up_reasons;
    missing_without_fixes;
    exec_time = t1.exec_time +. t2.exec_time;
    branch_number = t1.branch_number + t2.branch_number;
    steps_number = t1.steps_number + t2.steps_number;
  }

let create () =
  {
    exec_time = 0.0;
    give_up_reasons = Hstring.create 0;
    missing_without_fixes = Dynarray.create ();
    branch_number = 1;
    steps_number = 0;
  }

let push_give_up_reason ~loc reason t =
  match Hstring.find_opt t.give_up_reasons reason with
  | Some arr -> Dynarray.add_last arr loc
  | None ->
      let arr = Dynarray.create () in
      Dynarray.add_last arr loc;
      Hstring.add t.give_up_reasons reason arr

let push_missing_without_fix reason t =
  Dynarray.add_last t.missing_without_fixes reason

let did_give_up t = Hstring.length t.give_up_reasons > 0

let dump t file =
  let oc = open_out file in
  let json = to_yojson t in
  Yojson.Safe.to_channel oc json;
  close_out oc

module As_ctx = struct
  type _ Effect.t += Get : t Effect.t

  let get () = Effect.perform Get

  let wrap f =
    let stats = get () in
    f stats

  let push_give_up_reason ~loc reason = wrap (push_give_up_reason ~loc reason)
  let push_missing_without_fix reason = wrap (push_missing_without_fix reason)

  let with_stats ?(stats = create ()) f =
    let time = Unix.gettimeofday () in
    let res = try f () with effect Get, k -> Effect.Deep.continue k stats in
    stats.exec_time <- stats.exec_time +. (Unix.gettimeofday () -. time);
    (res, stats)

  let add_branches n =
    wrap (fun stats -> stats.branch_number <- stats.branch_number + n)

  let add_steps n =
    wrap (fun stats -> stats.steps_number <- stats.steps_number + n)
end
