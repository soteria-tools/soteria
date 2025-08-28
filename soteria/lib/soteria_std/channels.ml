let with_out_file file f =
  let oc = open_out file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)

let with_in_file file f =
  let ic = open_in file in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)
