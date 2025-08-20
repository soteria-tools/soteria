include Grace.Range

let to_yojson range =
  let source_to_yojson = function
    | `File file -> Some (`String file)
    | `String _ | `Reader _ -> None
  in
  let other_fields =
    [ ("start", `Int (start range :> int)); ("stop", `Int (stop range :> int)) ]
  in
  let all_fields =
    match source_to_yojson (source range) with
    | Some source -> ("source", source) :: other_fields
    | None -> other_fields
  in
  `Assoc all_fields

let of_yojson_exn (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let source_of_yojson_exn = function
    | `Null -> `File "unknown_file"
    | `String file -> `File file
    | _ -> raise (Type_error ("Invalid source", json))
  in
  let start = json |> member "start" |> to_int in
  let stop = json |> member "stop" |> to_int in
  let source = json |> member "source" |> source_of_yojson_exn in
  create ~source (Grace.Byte_index.of_int start) (Grace.Byte_index.of_int stop)

let of_yojson json =
  try Ok (of_yojson_exn json)
  with Yojson.Safe.Util.Type_error (msg, _) -> Error msg
