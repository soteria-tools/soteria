(* Not quite sure this is the right abstraction but we'll do with it. *)
type cmd = { directory : string; file : string; command : string list }

(** Strings need to be properly unescaped, I think upgrading to Yojson 3.0 will
    fix this, but in the meantime the ecosystem is pretty locked... *)
let unescape_string s =
  let buffer = Buffer.create (String.length s) in
  let last_is_backslash = ref false in
  String.iter
    (fun c ->
      match c with
      | '\\' when not !last_is_backslash -> last_is_backslash := true
      | '\\' when !last_is_backslash ->
          Buffer.add_char buffer '\\';
          last_is_backslash := false
      | '"' when !last_is_backslash ->
          Buffer.add_char buffer '"';
          last_is_backslash := false
      | 'n' when !last_is_backslash ->
          Buffer.add_char buffer '\n';
          last_is_backslash := false
      | 't' when !last_is_backslash ->
          Buffer.add_char buffer '\t';
          last_is_backslash := false
      | 'r' when !last_is_backslash ->
          Buffer.add_char buffer '\r';
          last_is_backslash := false
      | 'b' when !last_is_backslash ->
          Buffer.add_char buffer '\b';
          last_is_backslash := false
      | 'f' when !last_is_backslash ->
          Buffer.add_char buffer '\x0C';
          last_is_backslash := false
      | 'v' when !last_is_backslash ->
          Buffer.add_char buffer '\x0B';
          last_is_backslash := false
      | _ ->
          Buffer.add_char buffer c;
          last_is_backslash := false)
    s;
  Buffer.contents buffer

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
    json
    |> member "command"
    |> to_string
    |> unescape_string
    |> String.split_on_char ' '
  in
  let c_files, command = split_c_files_args [] [] command in
  (* TODO: This is kind of rough parsing of the compilation_commands. Let's see
     if we find examples that are different. *)
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
