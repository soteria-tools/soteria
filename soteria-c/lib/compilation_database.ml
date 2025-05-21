(* Not quite sure this is the right abstraction but we'll do with it. *)
type cmd = { directory : string; file : string; command : string list }

let cmd_of_yojson json =
  let rec split_c_files_args acc_c acc_rest = function
    | [] -> (acc_c, List.rev acc_rest)
    | "-c" :: c_file :: rest ->
        split_c_files_args (c_file :: acc_c) acc_rest rest
    | "-o" :: _ :: rest ->
        (* The -o flag is causing clang to not output anything to stdout... *)
        split_c_files_args acc_c acc_rest rest
    | x :: rest -> split_c_files_args acc_c (x :: acc_rest) rest
  in
  let open Yojson.Basic.Util in
  let directory = json |> member "directory" |> to_string in
  let file = json |> member "file" |> to_string in
  let command =
    json |> member "command" |> to_string |> String.split_on_char ' '
  in
  let c_files, command = split_c_files_args [] [] command in
  (* TODO: This is kind of rough parsing of the compilation_commands. 
               Let's see if we find examples that are different. *)
  let () =
    match c_files with
    | [ c ] when c = file -> ()
    | _ -> Fmt.failwith "Multiple -c arguments in compilation database?"
  in
  { directory; file; command }

type t = cmd list

let of_yojson json : t =
  let open Yojson.Basic.Util in
  json |> to_list |> List.map cmd_of_yojson

let from_file file =
  let json = Yojson.Basic.from_file ~fname:file file in
  of_yojson json
