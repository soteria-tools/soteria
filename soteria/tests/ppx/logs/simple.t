  $ ../test.sh simple.ml
  open Prelude
  
  let () =
    L.debug (fun m__ -> m__ "debug %d" 1);
    L.info (fun m__ -> m__ "info %s" "x");
    L.warn (fun m__ -> m__ "warn %a" Fmt.int 2);
    L.error (fun m__ -> m__ "error");
    L.trace (fun m__ -> m__ "trace");
    L.smt (fun m__ -> m__ "smt")
  Success ✅
