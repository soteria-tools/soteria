open Soteria_std
module Hstring = Hashtbl.Hstring
module Hsset = Hashset.Hstring
module Config = Config

type stat_entry =
  | Int of int  (** An integer statistic; joined by addition. *)
  | Float of float  (** A float statistic; joined by addition. *)
  | StrSeq of string Dynarray.t
      (** A sequence of strings; joined by concatenation. *)
  | Map of stat_entry Hstring.t
      (** A map from strings to stat entries; joined by merging entries
          recursively. *)
  | Yojson of Yojson.Safe.t
      (** A JSON value; joined by creating a JSON array of the two values if
          both are not arrays, or concatenating the two arrays if they are. Use
          this as a last resort when no other type fits. *)

let rec stat_entry_to_yojson = function
  | Int n -> `Int n
  | Float f -> `Float f
  | StrSeq arr -> Dynarray.to_yojson (fun s -> `String s) arr
  | Map m -> Hstring.to_yojson stat_entry_to_yojson m
  | Yojson json -> `List [ `String "RawJson"; json ]

let rec stat_entry_of_yojson : Yojson.Safe.t -> (stat_entry, string) result =
  function
  | `Int n -> Ok (Int n)
  | `Float f -> Ok (Float f)
  | `List (`String "RawJson" :: rest) -> (
      match rest with
      | [ json ] -> Ok (Yojson json)
      | _ -> Error "Invalid RawJson format in stat_entry_of_yojson")
  | `Assoc _ as e ->
      e |> Hstring.of_yojson stat_entry_of_yojson |> Result.map (fun m -> Map m)
  | `List _ as e ->
      e
      |> Dynarray.of_yojson (function
        | `String s -> Ok s
        | _ -> Error "Expected string in StrSeq")
      |> Result.map (fun arr -> StrSeq arr)
  | _ -> Error "Invalid format in stat_entry_of_yojson"

type t = stat_entry Hstring.t [@@deriving yojson]

exception Incompatible_entries

let create () = Hstring.create 0

let rec merge_entry e1 e2 =
  match (e1, e2) with
  | Int n1, Int n2 -> Int (n1 + n2)
  | Float f1, Float f2 -> Float (f1 +. f2)
  | StrSeq arr1, StrSeq arr2 ->
      let arr = Dynarray.create () in
      Dynarray.append arr arr1;
      Dynarray.append arr arr2;
      StrSeq arr
  | Map m1, Map m2 -> Map (merge m1 m2)
  | Yojson json1, Yojson json2 -> (
      match (json1, json2) with
      | `List l1, `List l2 -> Yojson (`List (l1 @ l2))
      | _ -> Yojson (`List [ json1; json2 ]))
  | _ -> raise Incompatible_entries

and merge m1 m2 =
  let m = Hstring.create (Hstring.length m1 + Hstring.length m2) in
  let add_pair key entry =
    match Hstring.find_opt m key with
    | Some existing_entry ->
        let merged_entry = merge_entry existing_entry entry in
        Hstring.replace m key merged_entry
    | None -> Hstring.add m key entry
  in
  Hstring.iter add_pair m1;
  Hstring.iter add_pair m2;
  m

type 'a with_stats = { res : 'a; stats : t }

let map_with_stats f { res; stats } = { res = f res; stats }
let with_empty_stats res = { res; stats = create () }
let as_int = function Int n -> n | _ -> raise Incompatible_entries
let as_float = function Float f -> f | _ -> raise Incompatible_entries
let as_strseq = function StrSeq arr -> arr | _ -> raise Incompatible_entries
let as_map = function Map m -> m | _ -> raise Incompatible_entries
let as_yojson = function Yojson json -> json | _ -> raise Incompatible_entries
let get_entry stats key = Hstring.find_opt stats key

let get_int ?(default = 0) stats key =
  match Hstring.find_opt stats key with Some e -> as_int e | None -> default

let get_float ?(default = 0.) stats key =
  match Hstring.find_opt stats key with Some e -> as_float e | None -> default

let get_strseq stats key =
  match Hstring.find_opt stats key with
  | Some e -> as_strseq e
  | None -> Dynarray.create ()

let get_map stats key =
  match Hstring.find_opt stats key with
  | Some e -> as_map e
  | None -> Hstring.create 0

type printer_config = (string * (t -> stat_entry Fmt.t)) option Hstring.t

let default_printer_config : printer_config = Hstring.create 0

let pp_list fmt_iter pp fn ft x =
  Fmt.pf ft "@,%a"
    (fmt_iter
       ?sep:(Some (Fmt.any "@\n"))
       fn
       (fun ft x -> Fmt.pf ft "- %a" pp x))
    x

let pp_iter_list fn pp ft x = pp_list Fmt.iter pp fn ft x
let pp_iter_bindings_list fn pp ft x = pp_list Fmt.iter_bindings pp fn ft x

let rec default_printer ft = function
  | Int n -> Fmt.int ft n
  | Float f -> Fmt.float ft f
  | StrSeq arr -> pp_iter_list Dynarray.iter Fmt.string ft arr
  | Map m ->
      pp_iter_bindings_list Hstring.iter
        (fun ft (key, entry) -> Fmt.pf ft "%s: %a" key default_printer entry)
        ft m
  | Yojson json -> Yojson.Safe.pretty_print ft json

let register_printer key ?name pp =
  let name = Option.value name ~default:key in
  Hstring.add default_printer_config key (Some (name, pp))

let disable_printer key = Hstring.add default_printer_config key None

let register_int_printer key ?name pp =
  register_printer key ?name @@ fun stats ft e ->
  match e with Int n -> pp stats ft n | _ -> default_printer ft e

let register_float_printer key ?name pp =
  register_printer key ?name @@ fun stats ft e ->
  match e with Float f -> pp stats ft f | _ -> default_printer ft e

let register_strseq_printer key ?name pp =
  register_printer key ?name @@ fun stats ft e ->
  match e with StrSeq arr -> pp stats ft arr | _ -> default_printer ft e

let register_map_printer key ?name pp =
  register_printer key ?name @@ fun stats ft e ->
  match e with Map m -> pp stats ft m | _ -> default_printer ft e

let dump t file =
  let oc = open_out file in
  let json = to_yojson t in
  Yojson.Safe.to_channel oc json;
  close_out oc

let pp ft (stats : t) =
  let open Logs.Printers in
  Fmt.pf ft "%a:@\n" (pp_style `Bold) "Statistics";
  Hstring.iter
    (fun key entry ->
      match Hstring.find_opt default_printer_config key with
      | Some (Some (name, pp)) ->
          Fmt.pf ft "@[<v 0>@[<v 2>• %s: %a@]@,@]@," name (pp stats) entry
      | Some None -> ()
      | None ->
          Fmt.pf ft "@[<v 0>@[<v 2>• %s: %a@]@,@]@," key default_printer entry)
    stats;
  Fmt.pf ft "@?"

let output t =
  match (Config.get ()).output_stats with
  | None -> ()
  | Some "stdout" -> pp Fmt.stdout t
  | Some file -> dump t file

module As_ctx = struct
  type _ Effect.t += Apply : (t -> unit) -> unit Effect.t

  let with_stats () f =
    let stats = create () in
    let res =
      try f ()
      with effect Apply f, k ->
        f stats;
        Effect.Deep.continue k ()
    in
    { res; stats }

  let with_stats_ignored () f =
    try f () with effect Apply _, k -> Effect.Deep.continue k ()

  let[@inline] apply f = Effect.perform (Apply f)

  let push_entry key entry =
    apply (fun stats ->
        match Hstring.find_opt stats key with
        | Some existing_entry ->
            let merged_entry = merge_entry existing_entry entry in
            Hstring.replace stats key merged_entry
        | None -> Hstring.add stats key entry)

  let add_int key n = push_entry key (Int n)
  let incr key = add_int key 1
  let add_float key f = push_entry key (Float f)

  let add_time_of_to key f =
    let start = Unix.gettimeofday () in
    let res = f () in
    add_float key (Unix.gettimeofday () -. start);
    res

  let push_str key s =
    (* We do a custom implementation here for performance *)
    apply (fun stats ->
        match Hstring.find_opt stats key with
        | Some (StrSeq arr) -> Dynarray.add_last arr s
        | Some e ->
            let arr = Dynarray.create () in
            Dynarray.add_last arr s;
            let new_entry = StrSeq arr in
            let merged_entry = merge_entry e new_entry in
            Hstring.replace stats key merged_entry
        | None ->
            let arr = Dynarray.create () in
            Dynarray.add_last arr s;
            let entry = StrSeq arr in
            Hstring.add stats key entry)

  let push_binding key subkey entry =
    (* We do a custom implementation here for performance *)
    apply (fun stats ->
        match Hstring.find_opt stats key with
        | Some (Map m) -> (
            match Hstring.find_opt m subkey with
            | Some existing_entry ->
                let merged_entry = merge_entry existing_entry entry in
                Hstring.replace m subkey merged_entry
            | None -> Hstring.add m subkey entry)
        | Some e ->
            let m = Hstring.create 1 in
            Hstring.add m subkey entry;
            let new_entry = Map m in
            let merged_entry = merge_entry e new_entry in
            Hstring.replace stats key merged_entry
        | None ->
            let m = Hstring.create 1 in
            Hstring.add m subkey entry;
            let entry = Map m in
            Hstring.add stats key entry)

  let push_string_binding key subkey str =
    push_binding key subkey (StrSeq (Dynarray.of_list [ str ]))
end
