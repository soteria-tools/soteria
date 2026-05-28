open Svalue
open Rust_val
open Typed.Syntax
open Typed.Infix

type fn = Exit | Sysconf

let fn_pats = [ ("exit", Exit); ("sysconf", Sysconf) ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let exit args =
    match args with
    | [ Int code ] ->
        if%sat code ==@ U32.(0s) then error `OkExit else error (`Exit code)
    | _ -> failwith "exit: invalid arguments"

  let sysconf args =
    match args with
    | [ Int _ ] ->
        (* https://man7.org/linux/man-pages/man3/sysconf.3.html
         * It is basically ok to always return the i64 `-1` saying "I don't know"
         *)
        let ret = Typed.BitVec.u64 Z.minus_one in
        ok (Int ret)
    | _ -> failwith "sysconf: invalid arguments"

  let[@inline] fn_to_stub = function Exit -> exit | Sysconf -> sysconf
end
