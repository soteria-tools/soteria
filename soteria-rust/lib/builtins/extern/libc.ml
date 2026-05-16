open Rust_val
open Typed.Syntax
open Typed.Infix

type fn = Exit

let fn_pats = [ ("exit", Exit) ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let exit args =
    match args with
    | [ Int code ] ->
        if%sat code ==@ U32.(0s) then error `OkExit else error (`Exit code)
    | _ -> failwith "exit: invalid arguments"

  let[@inline] fn_to_stub = function Exit -> exit
end
