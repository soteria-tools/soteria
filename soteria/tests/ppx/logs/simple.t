  $ ../test.sh simple.ml
  open Prelude
  
  let () =
    if L.should_log L.Level.Debug then
      L.force_log ~level:L.Level.Debug "debug %d" 1;
    if L.should_log L.Level.Info then
      L.force_log ~level:L.Level.Info "info %s" "x";
    if L.should_log L.Level.Warn then
      L.force_log ~level:L.Level.Warn "warn %a" Fmt.int 2;
    if L.should_log L.Level.Error then L.force_log ~level:L.Level.Error "error";
    if L.should_log L.Level.Trace then L.force_log ~level:L.Level.Trace "trace";
    if L.should_log L.Level.Smt then L.force_log ~level:L.Level.Smt "smt"
  Success ✅
