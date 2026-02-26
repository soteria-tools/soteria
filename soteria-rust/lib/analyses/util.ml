open Soteria.Logs.Printers
open Error.Diagnostic

let fatal ?name ?(code = 2) err =
  let msg = Option.fold ~none:"Fatal: " ~some:(Fmt.str "Fatal (%s): ") name in
  print_diagnostic_simple ~severity:Error (msg ^ err);
  exit code

let pp_branches = pp_plural ~sing:"branch" ~plur:"branches"
let pp_functions = pp_plural ~sing:"function" ~plur:"functions"

let print_pcs pcs =
  let open Fmt in
  let pp_pc ft (pc, i) =
    let name = "PC " ^ string_of_int i ^ ":" in
    if List.is_empty pc then pf ft "%a empty" (pp_style `Bold) name
    else
      let pp_pc = list ~sep:(any " /\\@, ") Typed.ppa in
      pf ft "%a @[<-1>%a@]" (pp_style `Bold) name pp_pc pc
  in
  if (Config.get ()).show_pcs then
    let pcs_indexed = List.mapi (fun i pc -> (pc, i + 1)) pcs in
    Fmt.pr "%a@." (list ~sep:(any "@\n") pp_pc) pcs_indexed
