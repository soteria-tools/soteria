open Cerb_frontend

type stmt = GenTypes.genTypeCategory AilSyntax.statement
type fundef = GenTypes.genTypeCategory AilSyntax.sigma_function_definition
type state = unit

let exec_stmt ~prog:_ (_state : state) (stmt : stmt) : state =
  let stmt = match stmt with AnnotatedStatement (_, _, stmt) -> stmt in
  match stmt with AilSskip -> () | _ -> ()

let exec_fun ~prog ~args (fundef : fundef) =
  if not @@ List.is_empty args then Fmt.failwith "function with arguments";
  L.info (fun m -> m "Executing a function");
  let _, (_, _, _, _, stmt) = fundef in
  exec_stmt ~prog () stmt
