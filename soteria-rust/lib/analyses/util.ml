open Soteria.Logs.Printers
open Error.Diagnostic
module Var = Soteria.Symex.Var

let fatal ?name ?(code = 2) err =
  let msg = Option.fold ~none:"Fatal: " ~some:(Fmt.str "Fatal (%s): ") name in
  print_diagnostic_simple ~severity:Error (msg ^ err);
  exit code

let pp_branches = pp_plural ~sing:"branch" ~plur:"branches"
let pp_functions = pp_plural ~sing:"function" ~plur:"functions"

let pp_pc ft pc =
  let open Fmt in
  if List.is_empty pc then string ft "empty"
  else (list ~sep:(any " /\\@, ") Typed.ppa) ft pc

(* Collect the variables that appear in [pc] that have a user-facing name
   and/or metadata attached, so we can print a small legend next to the PC.
   See https://github.com/soteria-tools/soteria/issues/290. *)
let pc_named_vars pc =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun c ->
      Typed.iter_vars c (fun (v, _) ->
          if not (Hashtbl.mem seen v) then
            match (Var.name v, Var.metadata v) with
            | None, None -> ()
            | n, m -> Hashtbl.replace seen v (n, m)))
    pc;
  Hashtbl.to_seq seen
  |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> Var.compare a b)

let pp_var_legend ft entries =
  let open Fmt in
  let pp_one ft (var, (name, metadata)) =
    let pp_name ft = function
      | Some n -> pf ft "%s (%a)" n Var.pp var
      | None -> Var.pp ft var
    in
    let pp_meta ft = function
      | Some m -> pf ft " — %s" m
      | None -> ()
    in
    pf ft "%a%a" pp_name name pp_meta metadata
  in
  pf ft "@[<v 2>%a@,%a@]" (pp_style `Bold) "Variables:"
    (list ~sep:(any "@,") pp_one)
    entries

let print_pcs pcs =
  let open Fmt in
  let pp_pc ft (pc, i) =
    pf ft "%a: @[<-1>%a@]" (pp_style `Bold) ("PC " ^ string_of_int i) pp_pc pc
  in
  if (Config.get ()).show_pcs then (
    let pcs_indexed = List.mapi (fun i pc -> (pc, i + 1)) pcs in
    Fmt.pr "%a@." (list ~sep:(any "@\n") pp_pc) pcs_indexed;
    let entries = pc_named_vars (List.concat pcs) in
    if not (List.is_empty entries) then Fmt.pr "%a@." pp_var_legend entries)
