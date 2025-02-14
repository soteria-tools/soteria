open Utils
open JsonUtils
open Syntaxes.Pipe
open Syntaxes.Result

type mutability = Not | Mut

(** Stores the provenance information of pointers stored in memory. Vec<(Size,
    Prov)> *)
and provenance_map = (int * int) list

and allocation = {
  bytes : int list;
  provenance : provenance_map;
  align : int;
  mutability : mutability;
}

and _alloc = Memory of allocation
and alloc = int * _alloc
and _function_decl = NormalSym of string | IntrinsicSym of string | NoOpSym
and function_decl = int * _function_decl
and projection = Deref | Field of (int * int)
and place = { local : int; projection : projection list }
and local = { ty : int; span : int; mutability : mutability }
and debug_source = { span : int; scope : int }

and debug_info = {
  name : string;
  source_info : debug_source;
  composite : unit;
  value : place;
  argument_index : unit;
}

and operand = Copy of place
and rvalue = Use of operand

and statement_raw =
  | StorageLive of int
  | Assign of { local : int; projection : projection list; value : rvalue }

and statement = statement_raw * int
and block = { statements : unit list; terminator : unit }

and body = {
  blocks : block list;
  locals : local list;
  arg_count : int;
  var_debug_info : debug_info list;
  spread_arg : unit;
  span : int;
}

and mono_item =
  | Fn of { name : string; id : int; body : body list }
      (** https://doc.rust-lang.org/beta/nightly-rustc/stable_mir/mir/mono/enum.MonoItem.html
      *)

and item = { name : string; details : unit; item_kind : mono_item }

and crate = {
  name : string;
  allocs : alloc list;
  functions : function_decl list;
  uneval_consts : unit list;
  items : item list;
}
[@@deriving eq, ord, show { with_path = false }]

let mutability_of_yojson = function
  | `String "Not" -> Ok Not
  | `String "Mut" -> Ok Mut
  | _ -> Error "Expected string \"Not\" or \"Mut\""

let provenance_map_of_yojson json =
  let f = function
    | `List l ->
        l
        |>+ ( function
        | `List [ `Int size; `Int prov ] -> Ok (size, prov)
        | _ -> Error "Expected list of two integers" )
        |> Res_list.combine
    | _ -> Error "Expected list of lists"
  in
  match json $> "ptrs" with
  | `Null -> Error "Expected field \"ptrs\""
  | json -> f json

let allocation_of_yojson json =
  let* bytes = json $> "bytes" |> to_int_list_res in
  let* provenance = json $> "provenance" |> provenance_map_of_yojson in
  let* align = json $> "align" |> to_int_res in
  let* mutability = json $> "mutability" |> mutability_of_yojson in
  Ok { bytes; provenance; align; mutability }

let alloc_of_yojson json =
  match json with
  | `List [ `Int id; `Assoc [ ("Memory", json) ] ] ->
      let* alloc = allocation_of_yojson json in
      Ok (id, Memory alloc)
  | `List [ `Int _; `Assoc [ (enum_name, _) ] ] ->
      Error ("Unhandled alloc enum name: " ^ enum_name)
  | _ -> Error "Expected list of id and allocation"

let function_decl_of_yojson json =
  match json with
  | `List [ `Int id; `Assoc [ ("NormalSym", `String name) ] ] ->
      Ok (id, NormalSym name)
  | `List [ `Int id; `Assoc [ ("IntrinsicSym", `String name) ] ] ->
      Ok (id, IntrinsicSym name)
  | `List [ `Int id; `Assoc [ ("NoOpSym", `String "") ] ] -> Ok (id, NoOpSym)
  | `List [ `Int _; `Assoc [ (enum_name, _) ] ] ->
      Error ("Unhandled function decl enum name: " ^ enum_name)
  | _ -> Error "Expected list of id and string"

let local_of_yojson json =
  let* ty = json $> "ty" |> to_int_res in
  let* span = json $> "span" |> to_int_res in
  let* mutability = json $> "mutability" |> mutability_of_yojson in
  Ok { ty; span; mutability }

let debug_source_of_yojson json =
  let* span = json $> "span" |> to_int_res in
  let* scope = json $> "scope" |> to_int_res in
  Ok { span; scope }

let projection_of_yojson json =
  match json with
  | `Assoc [ ("Field", json) ] -> (
      match json with
      | `List [ `Int field; `Int ty ] -> Ok (Field (field, ty))
      | _ -> Error "Expected list of two integers")
  | `String "Deref" -> Ok Deref
  | _ -> Error "Expected single property object"

let value_of_yojson json =
  match json with
  | `Assoc [ ("Place", json) ] ->
      let* local = json $> "local" |> to_int_res in
      let*@ projection = json $>@ "projection" |>+ projection_of_yojson in
      Ok { local; projection }
  | `Assoc [ (enum_name, _) ] ->
      Error ("Unhandled value enum name: " ^ enum_name)
  | _ -> Error "Expected single property object"

let debug_info_of_yojson json =
  let* name = json $> "name" |> to_string_res in
  let* source_info = json $> "source_info" |> debug_source_of_yojson in
  let* composite = json $> "composite" $>. () in
  let* value = json $> "value" |> value_of_yojson in
  let* argument_index = json $> "argument_index" $>. () in
  Ok { name; source_info; composite; value; argument_index }

let body_of_yojson json =
  let body = [] in
  let*@ locals = json $>@ "locals" |>+ local_of_yojson in
  let* arg_count = json $> "arg_count" |> to_int_res in
  let*@ var_debug_info =
    json $>@ "var_debug_info" |>+ fun _ ->
    Error "var_debug_info expected empty list"
  in
  let* spread_arg = json $> "spread_arg" $>. () in
  let* span = json $> "span" |> to_int_res in
  Ok { blocks = body; locals; arg_count; var_debug_info; spread_arg; span }

let mono_item_of_yojson json =
  match json with
  | `Assoc [ ("MonoItemFn", json) ] ->
      let* name = json $> "name" |> to_string_res in
      let* id = json $> "id" |> to_int_res in
      (* let* body = json $> "body" |> to_list_res in *)
      Ok (Fn { name; id; body = [] })
  | `Assoc [ (enum_name, _) ] ->
      Error ("Unhandled mono item enum name: " ^ enum_name)
  | _ -> Error "Expected single property object"

let item_of_yojson json =
  let* name = json $> "symbol_name" |> to_string_res in
  let* details = json $> "details" $>. () in
  let* item_kind = json $> "mono_item_kind" |> mono_item_of_yojson in
  Ok { name; details; item_kind }

let crate_of_yojson json =
  let* name = json $> "name" |> to_string_res in
  let*@ allocs = json $>@ "allocs" |>+ alloc_of_yojson in
  let*@ functions = json $>@ "functions" |>+ function_decl_of_yojson in
  let*@ uneval_consts =
    json $>@ "uneval_consts" |>+ fun _ ->
    Error "uneval_consts expected empty list"
  in
  let*@ items = json $>@ "items" |>+ item_of_yojson in
  Ok { name; allocs; functions; uneval_consts; items }
