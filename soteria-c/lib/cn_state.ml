open Csymex
open Csymex.Syntax
open Cerb_frontend

module Cell = struct
  type kind = WriteOnly | ReadWrite
  type t = { ty : Ctype.ctype; kind : kind; content : Aggregate_val.t }

  let no_fix = []

  let unwrap cell_opt =
    match cell_opt with
    | Some cell -> Result.ok cell
    | None -> Result.miss no_fix

  let load_aggregate ty cell_opt =
    let** cell = unwrap cell_opt in
    if Ctype.ctypeEqual cell.ty ty then
      match cell.kind with
      | WriteOnly -> Result.error `UninitializedMemoryAccess
      | ReadWrite -> Result.ok (cell.content, cell_opt)
    else give_up ~loc:(get_loc ()) "Type mismatch in CN cell load"

  let store ty value cell_opt =
    let** cell = unwrap cell_opt in
    if Ctype.ctypeEqual cell.ty ty then
      let new_cell = { ty = cell.ty; kind = ReadWrite; content = value } in
      Result.ok ((), Some new_cell)
    else give_up ~loc:(get_loc ()) "Type mismatch in CN cell store"
end
