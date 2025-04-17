type charon_cmd = { charon : string list; rustc : string list }

let mk_cmd ?(charon = []) ?(rustc = []) () = { charon; rustc }
let empty_cmd = mk_cmd ()

let concat_cmd { charon = c1; rustc = r1 } { charon = c2; rustc = r2 } =
  { charon = c1 @ c2; rustc = r1 @ r2 }

let concat_cmd_if b fn c1 = if b then concat_cmd c1 (fn ()) else c1

let build_cmd { charon; rustc } =
  "charon rustc " ^ String.concat " " charon ^ " -- " ^ String.concat " " rustc

let exec_cmd cmd =
  L.debug (fun g -> g "Running command: %s" cmd);
  Sys.command cmd

let exec_and_read cmd =
  L.debug (fun g -> g "Running command: %s" cmd);
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp;
  r
