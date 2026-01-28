(** An error that happen due to a misconfiguration. *)
exception Config_error of string

(** [config_error msg] raises a {!Config_error} with the given message *)
let config_error msg = raise (Config_error msg)

let () =
  Printexc.register_printer @@ function
  | Config_error msg -> Some ("Config error: " ^ msg)
  | _ -> None
