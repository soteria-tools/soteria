let parse_py filename =
  let open PyreAst.Parser in
  let file = Channels.read_file filename in
  let res = with_context (fun context -> Concrete.parse_module ~context file) in
  match res with
  | Ok ast -> ast
  | Error err -> Fmt.failwith "Error parsing %s: %s\n" filename err.message

let exec_module filename =
  let ast = parse_py filename in
  let symex = Interp.exec_module ast () in
  Pysymex.run symex

let exec_module_and_print filename =
  let results = exec_module filename in
  Fmt.pr "@[<v 2>Symex terminated with the following outcomes: %a@]@?"
    Fmt.(
      Dump.list @@ fun ft (r, _) ->
      Soteria_symex.Compo_res.pp
        ~ok:(Dump.pair (any "()") (any "()"))
        ~err:(any "()")
        ~miss:(Dump.list (any "()"))
        ft r)
    results
