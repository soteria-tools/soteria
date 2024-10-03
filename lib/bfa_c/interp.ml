open Cerb_frontend

let exec_stmt ~prog:_ (stmt : GenTypes.genTypeCategory AilSyntax.statement) =
  let stmt = match stmt with AnnotatedStatement (_, _, stmt) -> stmt in
  match stmt with AilSskip -> () | _ -> ()

let exec_fun ~prog ~args
    (fundef : GenTypes.genTypeCategory AilSyntax.sigma_function_definition) =
  if not @@ List.is_empty args then Fmt.failwith "function with arguments";
  L.info (fun m -> m "Executing a function");
  let _, (_, _, _, _, stmt) = fundef in
  exec_stmt ~prog stmt
