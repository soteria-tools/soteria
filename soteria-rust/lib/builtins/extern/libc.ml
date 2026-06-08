open Svalue
open Typed.Syntax
open Typed.Infix

type fn = Exit | Sysconf

let fn_pats = [ ("exit", Exit); ("sysconf", Sysconf) ]

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  let exit args =
    match args with
    | [ code ] ->
        let code = Typed.cast_i U32 code in
        if%sat code ==@ U32.(0s) then error `OkExit else error (`Exit code)
    | _ -> failwith "exit: invalid arguments"

  let sysconf _args =
    (* https://man7.org/linux/man-pages/man3/sysconf.3.html
     * It is basically ok to always return the i64 `-1` saying "I don't know"
     *)
    ok (Typed.BitVec.u64i (-1))

  let[@inline] fn_to_stub = function Exit -> exit | Sysconf -> sysconf
end
