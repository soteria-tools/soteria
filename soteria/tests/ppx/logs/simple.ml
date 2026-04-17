open Prelude

let () =
  [%l.debug "debug %d" 1];
  [%l.info "info %s" "x"];
  [%l.warn "warn %a" Fmt.int 2];
  [%l.error "error"];
  [%l.trace "trace"];
  [%l.smt "smt"]
