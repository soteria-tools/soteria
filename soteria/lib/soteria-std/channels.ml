let with_out_file file f =
  let oc = open_out file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)

let with_in_file file f =
  let ic = open_in file in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)

let read_file file =
  with_in_file file (fun ic ->
      let buffer = Buffer.create 1024 in
      let rec aux acc =
        match input_line ic with
        | line ->
            Buffer.add_string buffer (line ^ "\n");
            aux acc
        | exception End_of_file -> Buffer.contents buffer
      in
      aux ())
