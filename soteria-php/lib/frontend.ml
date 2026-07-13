module Error = struct
  type t =
    | Unavailable of string
    | Sidecar_failed of string
    | Invalid_ir of string

  let pp formatter = function
    | Unavailable message | Sidecar_failed message ->
        Format.pp_print_string formatter message
    | Invalid_ir message ->
        Format.fprintf formatter "invalid frontend IR: %s" message
end

let development_script = Filename.concat "bin" "lower.php"

let scripts_from_site directory =
  [
    Filename.concat directory "lower.php";
    Filename.concat directory development_script;
  ]

let is_executable path =
  try
    Unix.access path [ Unix.X_OK ];
    not (Sys.is_directory path)
  with Unix.Unix_error _ | Sys_error _ -> false

let executable_path () =
  let executable = Sys.executable_name in
  let candidates =
    if Filename.is_relative executable && not (String.contains executable '/')
    then
      Sys.getenv_opt "PATH"
      |> Option.value ~default:""
      |> String.split_on_char ':'
      |> List.map (fun directory -> Filename.concat directory executable)
    else if Filename.is_relative executable then
      [ Filename.concat (Sys.getcwd ()) executable ]
    else [ executable ]
  in
  List.find_opt is_executable candidates
  |> Option.map (fun path ->
      try Unix.realpath path with Unix.Unix_error _ -> path)

let packaged_script () =
  Option.map
    (fun executable ->
      let prefix = Filename.dirname (Filename.dirname executable) in
      Filename.concat prefix "share/soteria-php/frontend/lower.php")
    (executable_path ())

let find_lower_script () =
  match Sys.getenv_opt "SOTERIA_PHP_FRONTEND" with
  | Some path when Sys.file_exists path -> Ok path
  | Some path ->
      Error (Error.Unavailable (path ^ ": frontend script not found"))
  | None -> (
      let development_path =
        Filename.concat "soteria-php/frontend" development_script
      in
      if Sys.file_exists development_path then Ok development_path
      else
        let packaged_paths = Option.to_list (packaged_script ()) in
        match
          List.find_opt Sys.file_exists
            (packaged_paths
            @ List.concat_map scripts_from_site Frontend_site.Sites.frontend)
        with
        | Some path -> Ok path
        | None ->
            Error
              (Error.Unavailable
                 "PHP frontend script not found; set SOTERIA_PHP_FRONTEND"))

let status_message = function
  | Unix.WEXITED code -> Printf.sprintf "frontend exited with status %d" code
  | Unix.WSIGNALED signal ->
      Printf.sprintf "frontend was killed by signal %d" signal
  | Unix.WSTOPPED signal ->
      Printf.sprintf "frontend was stopped by signal %d" signal

let read_outputs stdout_channel stderr_channel =
  let stdout_fd = Unix.descr_of_in_channel stdout_channel in
  let stderr_fd = Unix.descr_of_in_channel stderr_channel in
  let buffer = Bytes.create 4096 in
  let stdout_buffer = Buffer.create 4096 in
  let stderr_buffer = Buffer.create 256 in
  let stdout_open = ref true in
  let stderr_open = ref true in
  while !stdout_open || !stderr_open do
    let descriptors =
      (if !stdout_open then [ stdout_fd ] else [])
      @ if !stderr_open then [ stderr_fd ] else []
    in
    let ready, _, _ = Unix.select descriptors [] [] (-1.) in
    List.iter
      (fun descriptor ->
        let read = Unix.read descriptor buffer 0 (Bytes.length buffer) in
        if read = 0 then
          if descriptor = stdout_fd then stdout_open := false
          else stderr_open := false
        else
          let chunk = Bytes.sub_string buffer 0 read in
          if descriptor = stdout_fd then Buffer.add_string stdout_buffer chunk
          else Buffer.add_string stderr_buffer chunk)
      ready
  done;
  (Buffer.contents stdout_buffer, Buffer.contents stderr_buffer)

let run_sidecar script filename =
  try
    let process =
      Unix.open_process_args_full "php"
        [| "php"; script; filename |]
        (Unix.environment ())
    in
    let stdout_channel, _, stderr_channel = process in
    let output, error_output = read_outputs stdout_channel stderr_channel in
    let error_output = String.trim error_output in
    let status = Unix.close_process_full process in
    match status with
    | Unix.WEXITED 0 -> Ok output
    | status ->
        let message =
          if String.equal error_output "" then status_message status
          else error_output
        in
        Error (Error.Sidecar_failed message)
  with Unix.Unix_error (error, operation, _) ->
    Error
      (Error.Unavailable
         (Printf.sprintf "unable to run PHP frontend (%s: %s)" operation
            (Unix.error_message error)))

let decode output =
  match Yojson.Safe.from_string output with
  | json ->
      Result.map_error
        (fun error -> Error.Invalid_ir error)
        (Php_ir.of_yojson json)
  | exception Yojson.Json_error message -> Error (Error.Invalid_ir message)

let parse_file filename =
  let ( let* ) = Result.bind in
  let* script = find_lower_script () in
  let* output = run_sidecar script filename in
  decode output
